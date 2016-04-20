
# Merge https://spamassassin.apache.org/publiccorpus/ spam database.
# Prepare Azure ML upload.
preprocess <- function()
{
    spam <- merger("src/spam.txt");
    spam("spam")
    spam("spam_2")
    
    ham <- merger("src/ham.txt");
    ham("easy_ham")
    ham("easy_ham_2")
    ham("hard_ham")
    
    pack("Emails.zip", "src")
}

merger <- function(fileName)
{
    writer <- writer(fileName)
    
    function(folder)
    {
        scanner <- scanner(folder)
        repeat
        {
            body <- scanner()
            if(is.null(body))
                break
            
            writer(body)
        }        
    }
}

scanner <- function(folder)
{
    fileNames <- list.files(folder, full.names = TRUE);
    index <- 1
    
    function()
    {
        if(length(fileNames) < index)
            return(NULL)
        
        fileName <- fileNames[index]
        index <<- index + 1
        
        con <- file(fileName, "r")
        repeat
        {
            header <- readLines(con, 1)
            if(length(header) == 0 || nchar(trimws(header)) == 0)
                break
        }
        
        body <- readLines(con, warn = FALSE)
        close(con)
        
        if(length(body) == 0)
            ""
        else
            paste(body, collapse = " ")   
    }
}

writer <- function(fileName)
{
    if(file.exists(fileName))
        file.remove(fileName) 
    
    function(body)
        write(body, fileName, append = TRUE)
}

pack <- function(arch, src)
{
    if(file.exists(arch))
        file.remove(arch) 
    
    zip(arch, list.files(src, full.names = TRUE), "-j")
}
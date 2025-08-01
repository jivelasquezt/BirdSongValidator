list.wavs <- list.files("/Users/jorge/Documents/GitHub/Sound_Validator/sample_sounds", "*.wav$", recursive = T)
master_sounds <- data.frame(recording_id=1:length(list.wavs),
                            species=dirname(list.wavs),
                            confidence=sapply(strsplit(basename(list.wavs),"_"), "[[", 1),
                            file=list.wavs,
                            lat=0,
                            lon=0,
                            location="Ñambi",
                            date=sapply(strsplit(basename(list.wavs),"_"), "[[", 4),
                            time=sapply(strsplit(basename(list.wavs),"_"), "[[", 5))
openxlsx::write.xlsx(master_sounds, "/Users/jorge/Documents/GitHub/Sound_Validator/sample_sounds/master_recordings.xlsx")

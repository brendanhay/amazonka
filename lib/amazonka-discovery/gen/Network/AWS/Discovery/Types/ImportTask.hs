{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTask where

import Network.AWS.Discovery.Types.ImportStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An array of information related to the import task request that includes status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
--
--
--
-- /See:/ 'importTask' smart constructor.
data ImportTask = ImportTask'
  { _itApplicationImportSuccess ::
      !(Maybe Int),
    _itStatus :: !(Maybe ImportStatus),
    _itServerImportSuccess :: !(Maybe Int),
    _itImportCompletionTime :: !(Maybe POSIX),
    _itName :: !(Maybe Text),
    _itApplicationImportFailure :: !(Maybe Int),
    _itErrorsAndFailedEntriesZip :: !(Maybe Text),
    _itImportTaskId :: !(Maybe Text),
    _itImportDeletedTime :: !(Maybe POSIX),
    _itServerImportFailure :: !(Maybe Int),
    _itClientRequestToken :: !(Maybe Text),
    _itImportURL :: !(Maybe Text),
    _itImportRequestTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itApplicationImportSuccess' - The total number of application records in the import file that were successfully imported.
--
-- * 'itStatus' - The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
--
-- * 'itServerImportSuccess' - The total number of server records in the import file that were successfully imported.
--
-- * 'itImportCompletionTime' - The time that the import task request finished, presented in the Unix time stamp format.
--
-- * 'itName' - A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- * 'itApplicationImportFailure' - The total number of application records in the import file that failed to be imported.
--
-- * 'itErrorsAndFailedEntriesZip' - A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request. This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it. If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
--
-- * 'itImportTaskId' - The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
--
-- * 'itImportDeletedTime' - The time that the import task request was deleted, presented in the Unix time stamp format.
--
-- * 'itServerImportFailure' - The total number of server records in the import file that failed to be imported.
--
-- * 'itClientRequestToken' - A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
--
-- * 'itImportURL' - The URL for your import file that you've uploaded to Amazon S3.
--
-- * 'itImportRequestTime' - The time that the import task request was made, presented in the Unix time stamp format.
importTask ::
  ImportTask
importTask =
  ImportTask'
    { _itApplicationImportSuccess = Nothing,
      _itStatus = Nothing,
      _itServerImportSuccess = Nothing,
      _itImportCompletionTime = Nothing,
      _itName = Nothing,
      _itApplicationImportFailure = Nothing,
      _itErrorsAndFailedEntriesZip = Nothing,
      _itImportTaskId = Nothing,
      _itImportDeletedTime = Nothing,
      _itServerImportFailure = Nothing,
      _itClientRequestToken = Nothing,
      _itImportURL = Nothing,
      _itImportRequestTime = Nothing
    }

-- | The total number of application records in the import file that were successfully imported.
itApplicationImportSuccess :: Lens' ImportTask (Maybe Int)
itApplicationImportSuccess = lens _itApplicationImportSuccess (\s a -> s {_itApplicationImportSuccess = a})

-- | The status of the import task. An import can have the status of @IMPORT_COMPLETE@ and still have some records fail to import from the overall request. More information can be found in the downloadable archive defined in the @errorsAndFailedEntriesZip@ field, or in the Migration Hub management console.
itStatus :: Lens' ImportTask (Maybe ImportStatus)
itStatus = lens _itStatus (\s a -> s {_itStatus = a})

-- | The total number of server records in the import file that were successfully imported.
itServerImportSuccess :: Lens' ImportTask (Maybe Int)
itServerImportSuccess = lens _itServerImportSuccess (\s a -> s {_itServerImportSuccess = a})

-- | The time that the import task request finished, presented in the Unix time stamp format.
itImportCompletionTime :: Lens' ImportTask (Maybe UTCTime)
itImportCompletionTime = lens _itImportCompletionTime (\s a -> s {_itImportCompletionTime = a}) . mapping _Time

-- | A descriptive name for an import task. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
itName :: Lens' ImportTask (Maybe Text)
itName = lens _itName (\s a -> s {_itName = a})

-- | The total number of application records in the import file that failed to be imported.
itApplicationImportFailure :: Lens' ImportTask (Maybe Int)
itApplicationImportFailure = lens _itApplicationImportFailure (\s a -> s {_itApplicationImportFailure = a})

-- | A link to a compressed archive folder (in the ZIP format) that contains an error log and a file of failed records. You can use these two files to quickly identify records that failed, why they failed, and correct those records. Afterward, you can upload the corrected file to your Amazon S3 bucket and create another import task request. This field also includes authorization information so you can confirm the authenticity of the compressed archive before you download it. If some records failed to be imported we recommend that you correct the records in the failed entries file and then imports that failed entries file. This prevents you from having to correct and update the larger original file and attempt importing it again.
itErrorsAndFailedEntriesZip :: Lens' ImportTask (Maybe Text)
itErrorsAndFailedEntriesZip = lens _itErrorsAndFailedEntriesZip (\s a -> s {_itErrorsAndFailedEntriesZip = a})

-- | The unique ID for a specific import task. These IDs aren't globally unique, but they are unique within an AWS account.
itImportTaskId :: Lens' ImportTask (Maybe Text)
itImportTaskId = lens _itImportTaskId (\s a -> s {_itImportTaskId = a})

-- | The time that the import task request was deleted, presented in the Unix time stamp format.
itImportDeletedTime :: Lens' ImportTask (Maybe UTCTime)
itImportDeletedTime = lens _itImportDeletedTime (\s a -> s {_itImportDeletedTime = a}) . mapping _Time

-- | The total number of server records in the import file that failed to be imported.
itServerImportFailure :: Lens' ImportTask (Maybe Int)
itServerImportFailure = lens _itServerImportFailure (\s a -> s {_itServerImportFailure = a})

-- | A unique token used to prevent the same import request from occurring more than once. If you didn't provide a token, a token was automatically generated when the import task request was sent.
itClientRequestToken :: Lens' ImportTask (Maybe Text)
itClientRequestToken = lens _itClientRequestToken (\s a -> s {_itClientRequestToken = a})

-- | The URL for your import file that you've uploaded to Amazon S3.
itImportURL :: Lens' ImportTask (Maybe Text)
itImportURL = lens _itImportURL (\s a -> s {_itImportURL = a})

-- | The time that the import task request was made, presented in the Unix time stamp format.
itImportRequestTime :: Lens' ImportTask (Maybe UTCTime)
itImportRequestTime = lens _itImportRequestTime (\s a -> s {_itImportRequestTime = a}) . mapping _Time

instance FromJSON ImportTask where
  parseJSON =
    withObject
      "ImportTask"
      ( \x ->
          ImportTask'
            <$> (x .:? "applicationImportSuccess")
            <*> (x .:? "status")
            <*> (x .:? "serverImportSuccess")
            <*> (x .:? "importCompletionTime")
            <*> (x .:? "name")
            <*> (x .:? "applicationImportFailure")
            <*> (x .:? "errorsAndFailedEntriesZip")
            <*> (x .:? "importTaskId")
            <*> (x .:? "importDeletedTime")
            <*> (x .:? "serverImportFailure")
            <*> (x .:? "clientRequestToken")
            <*> (x .:? "importUrl")
            <*> (x .:? "importRequestTime")
      )

instance Hashable ImportTask

instance NFData ImportTask

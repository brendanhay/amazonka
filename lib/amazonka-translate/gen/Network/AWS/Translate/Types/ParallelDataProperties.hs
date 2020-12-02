{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataProperties where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataStatus

-- | The properties of a parallel data resource.
--
--
--
-- /See:/ 'parallelDataProperties' smart constructor.
data ParallelDataProperties = ParallelDataProperties'
  { _pdpStatus ::
      !(Maybe ParallelDataStatus),
    _pdpLastUpdatedAt :: !(Maybe POSIX),
    _pdpImportedRecordCount :: !(Maybe Integer),
    _pdpARN :: !(Maybe Text),
    _pdpTargetLanguageCodes :: !(Maybe [Text]),
    _pdpCreatedAt :: !(Maybe POSIX),
    _pdpFailedRecordCount :: !(Maybe Integer),
    _pdpImportedDataSize :: !(Maybe Integer),
    _pdpName :: !(Maybe Text),
    _pdpSourceLanguageCode :: !(Maybe Text),
    _pdpLatestUpdateAttemptAt :: !(Maybe POSIX),
    _pdpEncryptionKey :: !(Maybe EncryptionKey),
    _pdpLatestUpdateAttemptStatus ::
      !(Maybe ParallelDataStatus),
    _pdpMessage :: !(Maybe Text),
    _pdpDescription :: !(Maybe Text),
    _pdpSkippedRecordCount :: !(Maybe Integer),
    _pdpParallelDataConfig ::
      !(Maybe ParallelDataConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParallelDataProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdpStatus' - The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
--
-- * 'pdpLastUpdatedAt' - The time at which the parallel data resource was last updated.
--
-- * 'pdpImportedRecordCount' - The number of records successfully imported from the parallel data input file.
--
-- * 'pdpARN' - The Amazon Resource Name (ARN) of the parallel data resource.
--
-- * 'pdpTargetLanguageCodes' - The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
--
-- * 'pdpCreatedAt' - The time at which the parallel data resource was created.
--
-- * 'pdpFailedRecordCount' - The number of records unsuccessfully imported from the parallel data input file.
--
-- * 'pdpImportedDataSize' - The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
--
-- * 'pdpName' - The custom name assigned to the parallel data resource.
--
-- * 'pdpSourceLanguageCode' - The source language of the translations in the parallel data file.
--
-- * 'pdpLatestUpdateAttemptAt' - The time that the most recent update was attempted.
--
-- * 'pdpEncryptionKey' - Undocumented member.
--
-- * 'pdpLatestUpdateAttemptStatus' - The status of the most recent update attempt for the parallel data resource.
--
-- * 'pdpMessage' - Additional information from Amazon Translate about the parallel data resource.
--
-- * 'pdpDescription' - The description assigned to the parallel data resource.
--
-- * 'pdpSkippedRecordCount' - The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
--
-- * 'pdpParallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
parallelDataProperties ::
  ParallelDataProperties
parallelDataProperties =
  ParallelDataProperties'
    { _pdpStatus = Nothing,
      _pdpLastUpdatedAt = Nothing,
      _pdpImportedRecordCount = Nothing,
      _pdpARN = Nothing,
      _pdpTargetLanguageCodes = Nothing,
      _pdpCreatedAt = Nothing,
      _pdpFailedRecordCount = Nothing,
      _pdpImportedDataSize = Nothing,
      _pdpName = Nothing,
      _pdpSourceLanguageCode = Nothing,
      _pdpLatestUpdateAttemptAt = Nothing,
      _pdpEncryptionKey = Nothing,
      _pdpLatestUpdateAttemptStatus = Nothing,
      _pdpMessage = Nothing,
      _pdpDescription = Nothing,
      _pdpSkippedRecordCount = Nothing,
      _pdpParallelDataConfig = Nothing
    }

-- | The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
pdpStatus :: Lens' ParallelDataProperties (Maybe ParallelDataStatus)
pdpStatus = lens _pdpStatus (\s a -> s {_pdpStatus = a})

-- | The time at which the parallel data resource was last updated.
pdpLastUpdatedAt :: Lens' ParallelDataProperties (Maybe UTCTime)
pdpLastUpdatedAt = lens _pdpLastUpdatedAt (\s a -> s {_pdpLastUpdatedAt = a}) . mapping _Time

-- | The number of records successfully imported from the parallel data input file.
pdpImportedRecordCount :: Lens' ParallelDataProperties (Maybe Integer)
pdpImportedRecordCount = lens _pdpImportedRecordCount (\s a -> s {_pdpImportedRecordCount = a})

-- | The Amazon Resource Name (ARN) of the parallel data resource.
pdpARN :: Lens' ParallelDataProperties (Maybe Text)
pdpARN = lens _pdpARN (\s a -> s {_pdpARN = a})

-- | The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
pdpTargetLanguageCodes :: Lens' ParallelDataProperties [Text]
pdpTargetLanguageCodes = lens _pdpTargetLanguageCodes (\s a -> s {_pdpTargetLanguageCodes = a}) . _Default . _Coerce

-- | The time at which the parallel data resource was created.
pdpCreatedAt :: Lens' ParallelDataProperties (Maybe UTCTime)
pdpCreatedAt = lens _pdpCreatedAt (\s a -> s {_pdpCreatedAt = a}) . mapping _Time

-- | The number of records unsuccessfully imported from the parallel data input file.
pdpFailedRecordCount :: Lens' ParallelDataProperties (Maybe Integer)
pdpFailedRecordCount = lens _pdpFailedRecordCount (\s a -> s {_pdpFailedRecordCount = a})

-- | The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
pdpImportedDataSize :: Lens' ParallelDataProperties (Maybe Integer)
pdpImportedDataSize = lens _pdpImportedDataSize (\s a -> s {_pdpImportedDataSize = a})

-- | The custom name assigned to the parallel data resource.
pdpName :: Lens' ParallelDataProperties (Maybe Text)
pdpName = lens _pdpName (\s a -> s {_pdpName = a})

-- | The source language of the translations in the parallel data file.
pdpSourceLanguageCode :: Lens' ParallelDataProperties (Maybe Text)
pdpSourceLanguageCode = lens _pdpSourceLanguageCode (\s a -> s {_pdpSourceLanguageCode = a})

-- | The time that the most recent update was attempted.
pdpLatestUpdateAttemptAt :: Lens' ParallelDataProperties (Maybe UTCTime)
pdpLatestUpdateAttemptAt = lens _pdpLatestUpdateAttemptAt (\s a -> s {_pdpLatestUpdateAttemptAt = a}) . mapping _Time

-- | Undocumented member.
pdpEncryptionKey :: Lens' ParallelDataProperties (Maybe EncryptionKey)
pdpEncryptionKey = lens _pdpEncryptionKey (\s a -> s {_pdpEncryptionKey = a})

-- | The status of the most recent update attempt for the parallel data resource.
pdpLatestUpdateAttemptStatus :: Lens' ParallelDataProperties (Maybe ParallelDataStatus)
pdpLatestUpdateAttemptStatus = lens _pdpLatestUpdateAttemptStatus (\s a -> s {_pdpLatestUpdateAttemptStatus = a})

-- | Additional information from Amazon Translate about the parallel data resource.
pdpMessage :: Lens' ParallelDataProperties (Maybe Text)
pdpMessage = lens _pdpMessage (\s a -> s {_pdpMessage = a})

-- | The description assigned to the parallel data resource.
pdpDescription :: Lens' ParallelDataProperties (Maybe Text)
pdpDescription = lens _pdpDescription (\s a -> s {_pdpDescription = a})

-- | The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
pdpSkippedRecordCount :: Lens' ParallelDataProperties (Maybe Integer)
pdpSkippedRecordCount = lens _pdpSkippedRecordCount (\s a -> s {_pdpSkippedRecordCount = a})

-- | Specifies the format and S3 location of the parallel data input file.
pdpParallelDataConfig :: Lens' ParallelDataProperties (Maybe ParallelDataConfig)
pdpParallelDataConfig = lens _pdpParallelDataConfig (\s a -> s {_pdpParallelDataConfig = a})

instance FromJSON ParallelDataProperties where
  parseJSON =
    withObject
      "ParallelDataProperties"
      ( \x ->
          ParallelDataProperties'
            <$> (x .:? "Status")
            <*> (x .:? "LastUpdatedAt")
            <*> (x .:? "ImportedRecordCount")
            <*> (x .:? "Arn")
            <*> (x .:? "TargetLanguageCodes" .!= mempty)
            <*> (x .:? "CreatedAt")
            <*> (x .:? "FailedRecordCount")
            <*> (x .:? "ImportedDataSize")
            <*> (x .:? "Name")
            <*> (x .:? "SourceLanguageCode")
            <*> (x .:? "LatestUpdateAttemptAt")
            <*> (x .:? "EncryptionKey")
            <*> (x .:? "LatestUpdateAttemptStatus")
            <*> (x .:? "Message")
            <*> (x .:? "Description")
            <*> (x .:? "SkippedRecordCount")
            <*> (x .:? "ParallelDataConfig")
      )

instance Hashable ParallelDataProperties

instance NFData ParallelDataProperties

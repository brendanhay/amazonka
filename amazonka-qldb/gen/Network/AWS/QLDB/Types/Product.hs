{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.QLDB.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types.Sum

-- | /See:/ 'journalS3ExportDescription' smart constructor.
data JournalS3ExportDescription = JournalS3ExportDescription'
  { _jsedLedgerName            :: !Text
  , _jsedExportId              :: !Text
  , _jsedExportCreationTime    :: !POSIX
  , _jsedStatus                :: !ExportStatus
  , _jsedInclusiveStartTime    :: !POSIX
  , _jsedExclusiveEndTime      :: !POSIX
  , _jsedS3ExportConfiguration :: !S3ExportConfiguration
  , _jsedRoleARN               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JournalS3ExportDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsedLedgerName' - Undocumented member.
--
-- * 'jsedExportId' - Undocumented member.
--
-- * 'jsedExportCreationTime' - Undocumented member.
--
-- * 'jsedStatus' - Undocumented member.
--
-- * 'jsedInclusiveStartTime' - Undocumented member.
--
-- * 'jsedExclusiveEndTime' - Undocumented member.
--
-- * 'jsedS3ExportConfiguration' - Undocumented member.
--
-- * 'jsedRoleARN' - Undocumented member.
journalS3ExportDescription
    :: Text -- ^ 'jsedLedgerName'
    -> Text -- ^ 'jsedExportId'
    -> UTCTime -- ^ 'jsedExportCreationTime'
    -> ExportStatus -- ^ 'jsedStatus'
    -> UTCTime -- ^ 'jsedInclusiveStartTime'
    -> UTCTime -- ^ 'jsedExclusiveEndTime'
    -> S3ExportConfiguration -- ^ 'jsedS3ExportConfiguration'
    -> Text -- ^ 'jsedRoleARN'
    -> JournalS3ExportDescription
journalS3ExportDescription pLedgerName_ pExportId_ pExportCreationTime_ pStatus_ pInclusiveStartTime_ pExclusiveEndTime_ pS3ExportConfiguration_ pRoleARN_ =
  JournalS3ExportDescription'
    { _jsedLedgerName = pLedgerName_
    , _jsedExportId = pExportId_
    , _jsedExportCreationTime = _Time # pExportCreationTime_
    , _jsedStatus = pStatus_
    , _jsedInclusiveStartTime = _Time # pInclusiveStartTime_
    , _jsedExclusiveEndTime = _Time # pExclusiveEndTime_
    , _jsedS3ExportConfiguration = pS3ExportConfiguration_
    , _jsedRoleARN = pRoleARN_
    }


-- | Undocumented member.
jsedLedgerName :: Lens' JournalS3ExportDescription Text
jsedLedgerName = lens _jsedLedgerName (\ s a -> s{_jsedLedgerName = a})

-- | Undocumented member.
jsedExportId :: Lens' JournalS3ExportDescription Text
jsedExportId = lens _jsedExportId (\ s a -> s{_jsedExportId = a})

-- | Undocumented member.
jsedExportCreationTime :: Lens' JournalS3ExportDescription UTCTime
jsedExportCreationTime = lens _jsedExportCreationTime (\ s a -> s{_jsedExportCreationTime = a}) . _Time

-- | Undocumented member.
jsedStatus :: Lens' JournalS3ExportDescription ExportStatus
jsedStatus = lens _jsedStatus (\ s a -> s{_jsedStatus = a})

-- | Undocumented member.
jsedInclusiveStartTime :: Lens' JournalS3ExportDescription UTCTime
jsedInclusiveStartTime = lens _jsedInclusiveStartTime (\ s a -> s{_jsedInclusiveStartTime = a}) . _Time

-- | Undocumented member.
jsedExclusiveEndTime :: Lens' JournalS3ExportDescription UTCTime
jsedExclusiveEndTime = lens _jsedExclusiveEndTime (\ s a -> s{_jsedExclusiveEndTime = a}) . _Time

-- | Undocumented member.
jsedS3ExportConfiguration :: Lens' JournalS3ExportDescription S3ExportConfiguration
jsedS3ExportConfiguration = lens _jsedS3ExportConfiguration (\ s a -> s{_jsedS3ExportConfiguration = a})

-- | Undocumented member.
jsedRoleARN :: Lens' JournalS3ExportDescription Text
jsedRoleARN = lens _jsedRoleARN (\ s a -> s{_jsedRoleARN = a})

instance FromJSON JournalS3ExportDescription where
        parseJSON
          = withObject "JournalS3ExportDescription"
              (\ x ->
                 JournalS3ExportDescription' <$>
                   (x .: "LedgerName") <*> (x .: "ExportId") <*>
                     (x .: "ExportCreationTime")
                     <*> (x .: "Status")
                     <*> (x .: "InclusiveStartTime")
                     <*> (x .: "ExclusiveEndTime")
                     <*> (x .: "S3ExportConfiguration")
                     <*> (x .: "RoleArn"))

instance Hashable JournalS3ExportDescription where

instance NFData JournalS3ExportDescription where

-- | /See:/ 'ledgerSummary' smart constructor.
data LedgerSummary = LedgerSummary'
  { _lsState            :: !(Maybe LedgerState)
  , _lsName             :: !(Maybe Text)
  , _lsCreationDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LedgerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsState' - Undocumented member.
--
-- * 'lsName' - Undocumented member.
--
-- * 'lsCreationDateTime' - Undocumented member.
ledgerSummary
    :: LedgerSummary
ledgerSummary =
  LedgerSummary'
    {_lsState = Nothing, _lsName = Nothing, _lsCreationDateTime = Nothing}


-- | Undocumented member.
lsState :: Lens' LedgerSummary (Maybe LedgerState)
lsState = lens _lsState (\ s a -> s{_lsState = a})

-- | Undocumented member.
lsName :: Lens' LedgerSummary (Maybe Text)
lsName = lens _lsName (\ s a -> s{_lsName = a})

-- | Undocumented member.
lsCreationDateTime :: Lens' LedgerSummary (Maybe UTCTime)
lsCreationDateTime = lens _lsCreationDateTime (\ s a -> s{_lsCreationDateTime = a}) . mapping _Time

instance FromJSON LedgerSummary where
        parseJSON
          = withObject "LedgerSummary"
              (\ x ->
                 LedgerSummary' <$>
                   (x .:? "State") <*> (x .:? "Name") <*>
                     (x .:? "CreationDateTime"))

instance Hashable LedgerSummary where

instance NFData LedgerSummary where

-- | /See:/ 's3EncryptionConfiguration' smart constructor.
data S3EncryptionConfiguration = S3EncryptionConfiguration'
  { _secKMSKeyARN            :: !(Maybe Text)
  , _secObjectEncryptionType :: !S3ObjectEncryptionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secKMSKeyARN' - Undocumented member.
--
-- * 'secObjectEncryptionType' - Undocumented member.
s3EncryptionConfiguration
    :: S3ObjectEncryptionType -- ^ 'secObjectEncryptionType'
    -> S3EncryptionConfiguration
s3EncryptionConfiguration pObjectEncryptionType_ =
  S3EncryptionConfiguration'
    {_secKMSKeyARN = Nothing, _secObjectEncryptionType = pObjectEncryptionType_}


-- | Undocumented member.
secKMSKeyARN :: Lens' S3EncryptionConfiguration (Maybe Text)
secKMSKeyARN = lens _secKMSKeyARN (\ s a -> s{_secKMSKeyARN = a})

-- | Undocumented member.
secObjectEncryptionType :: Lens' S3EncryptionConfiguration S3ObjectEncryptionType
secObjectEncryptionType = lens _secObjectEncryptionType (\ s a -> s{_secObjectEncryptionType = a})

instance FromJSON S3EncryptionConfiguration where
        parseJSON
          = withObject "S3EncryptionConfiguration"
              (\ x ->
                 S3EncryptionConfiguration' <$>
                   (x .:? "KmsKeyArn") <*>
                     (x .: "ObjectEncryptionType"))

instance Hashable S3EncryptionConfiguration where

instance NFData S3EncryptionConfiguration where

instance ToJSON S3EncryptionConfiguration where
        toJSON S3EncryptionConfiguration'{..}
          = object
              (catMaybes
                 [("KmsKeyArn" .=) <$> _secKMSKeyARN,
                  Just
                    ("ObjectEncryptionType" .=
                       _secObjectEncryptionType)])

-- | /See:/ 's3ExportConfiguration' smart constructor.
data S3ExportConfiguration = S3ExportConfiguration'
  { _secBucket                  :: !Text
  , _secPrefix                  :: !Text
  , _secEncryptionConfiguration :: !S3EncryptionConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ExportConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secBucket' - Undocumented member.
--
-- * 'secPrefix' - Undocumented member.
--
-- * 'secEncryptionConfiguration' - Undocumented member.
s3ExportConfiguration
    :: Text -- ^ 'secBucket'
    -> Text -- ^ 'secPrefix'
    -> S3EncryptionConfiguration -- ^ 'secEncryptionConfiguration'
    -> S3ExportConfiguration
s3ExportConfiguration pBucket_ pPrefix_ pEncryptionConfiguration_ =
  S3ExportConfiguration'
    { _secBucket = pBucket_
    , _secPrefix = pPrefix_
    , _secEncryptionConfiguration = pEncryptionConfiguration_
    }


-- | Undocumented member.
secBucket :: Lens' S3ExportConfiguration Text
secBucket = lens _secBucket (\ s a -> s{_secBucket = a})

-- | Undocumented member.
secPrefix :: Lens' S3ExportConfiguration Text
secPrefix = lens _secPrefix (\ s a -> s{_secPrefix = a})

-- | Undocumented member.
secEncryptionConfiguration :: Lens' S3ExportConfiguration S3EncryptionConfiguration
secEncryptionConfiguration = lens _secEncryptionConfiguration (\ s a -> s{_secEncryptionConfiguration = a})

instance FromJSON S3ExportConfiguration where
        parseJSON
          = withObject "S3ExportConfiguration"
              (\ x ->
                 S3ExportConfiguration' <$>
                   (x .: "Bucket") <*> (x .: "Prefix") <*>
                     (x .: "EncryptionConfiguration"))

instance Hashable S3ExportConfiguration where

instance NFData S3ExportConfiguration where

instance ToJSON S3ExportConfiguration where
        toJSON S3ExportConfiguration'{..}
          = object
              (catMaybes
                 [Just ("Bucket" .= _secBucket),
                  Just ("Prefix" .= _secPrefix),
                  Just
                    ("EncryptionConfiguration" .=
                       _secEncryptionConfiguration)])

-- | /See:/ 'valueHolder' smart constructor.
newtype ValueHolder = ValueHolder'
  { _vhIonText :: Maybe (Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValueHolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vhIonText' - Undocumented member.
valueHolder
    :: ValueHolder
valueHolder = ValueHolder' {_vhIonText = Nothing}


-- | Undocumented member.
vhIonText :: Lens' ValueHolder (Maybe Text)
vhIonText = lens _vhIonText (\ s a -> s{_vhIonText = a}) . mapping _Sensitive

instance FromJSON ValueHolder where
        parseJSON
          = withObject "ValueHolder"
              (\ x -> ValueHolder' <$> (x .:? "IonText"))

instance Hashable ValueHolder where

instance NFData ValueHolder where

instance ToJSON ValueHolder where
        toJSON ValueHolder'{..}
          = object (catMaybes [("IonText" .=) <$> _vhIonText])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Product where

import           Network.AWS.Firehose.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Describes the buffering to perform before delivering data to the
-- destination.
--
-- /See:/ 'bufferingHints' smart constructor.
data BufferingHints = BufferingHints'
    { _bhSizeInMBs         :: !(Maybe Nat)
    , _bhIntervalInSeconds :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bhSizeInMBs'
--
-- * 'bhIntervalInSeconds'
bufferingHints
    :: BufferingHints
bufferingHints =
    BufferingHints'
    { _bhSizeInMBs = Nothing
    , _bhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it
-- to the destination. The default value is 5.
--
-- We recommend setting SizeInMBs to a value greater than the amount of
-- data you typically ingest into the delivery stream in 10 seconds. For
-- example, if you typically ingest data at 1 MB\/sec set SizeInMBs to be
-- 10 MB or higher.
bhSizeInMBs :: Lens' BufferingHints (Maybe Natural)
bhSizeInMBs = lens _bhSizeInMBs (\ s a -> s{_bhSizeInMBs = a}) . mapping _Nat;

-- | Buffer incoming data for the specified period of time, in seconds,
-- before delivering it to the destination. The default value is 300.
bhIntervalInSeconds :: Lens' BufferingHints (Maybe Natural)
bhIntervalInSeconds = lens _bhIntervalInSeconds (\ s a -> s{_bhIntervalInSeconds = a}) . mapping _Nat;

instance FromJSON BufferingHints where
        parseJSON
          = withObject "BufferingHints"
              (\ x ->
                 BufferingHints' <$>
                   (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds"))

instance Hashable BufferingHints

instance ToJSON BufferingHints where
        toJSON BufferingHints'{..}
          = object
              (catMaybes
                 [("SizeInMBs" .=) <$> _bhSizeInMBs,
                  ("IntervalInSeconds" .=) <$> _bhIntervalInSeconds])

-- | Describes a 'COPY' command for Amazon Redshift.
--
-- /See:/ 'copyCommand' smart constructor.
data CopyCommand = CopyCommand'
    { _ccCopyOptions      :: !(Maybe Text)
    , _ccDataTableColumns :: !(Maybe Text)
    , _ccDataTableName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCopyOptions'
--
-- * 'ccDataTableColumns'
--
-- * 'ccDataTableName'
copyCommand
    :: Text -- ^ 'ccDataTableName'
    -> CopyCommand
copyCommand pDataTableName_ =
    CopyCommand'
    { _ccCopyOptions = Nothing
    , _ccDataTableColumns = Nothing
    , _ccDataTableName = pDataTableName_
    }

-- | Optional parameters to use with the Amazon Redshift 'COPY' command. For
-- more information, see the \"Optional Parameters\" section of
-- <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command>.
-- Some possible examples that would apply to Amazon Kinesis Firehose are
-- as follows.
--
-- 'delimiter \'\\t\' lzop;' - fields are delimited with \"\\t\" (TAB
-- character) and compressed using lzop.
--
-- 'delimiter \'|' - fields are delimited with \"|\" (this is the default
-- delimiter).
--
-- 'delimiter \'|\' escape' - the delimiter should be escaped.
--
-- 'fixedwidth \'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6\''
-- - fields are fixed width in the source, with each width specified after
-- every column in the table.
--
-- 'JSON \'s3:\/\/mybucket\/jsonpaths.txt\'' - data is in JSON format, and
-- the path specified is the format of the data.
--
-- For more examples, see and
-- <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command exmaples>.
ccCopyOptions :: Lens' CopyCommand (Maybe Text)
ccCopyOptions = lens _ccCopyOptions (\ s a -> s{_ccCopyOptions = a});

-- | A comma-separated list of column names.
ccDataTableColumns :: Lens' CopyCommand (Maybe Text)
ccDataTableColumns = lens _ccDataTableColumns (\ s a -> s{_ccDataTableColumns = a});

-- | The name of the target table. The table must already exist in the
-- database.
ccDataTableName :: Lens' CopyCommand Text
ccDataTableName = lens _ccDataTableName (\ s a -> s{_ccDataTableName = a});

instance FromJSON CopyCommand where
        parseJSON
          = withObject "CopyCommand"
              (\ x ->
                 CopyCommand' <$>
                   (x .:? "CopyOptions") <*> (x .:? "DataTableColumns")
                     <*> (x .: "DataTableName"))

instance Hashable CopyCommand

instance ToJSON CopyCommand where
        toJSON CopyCommand'{..}
          = object
              (catMaybes
                 [("CopyOptions" .=) <$> _ccCopyOptions,
                  ("DataTableColumns" .=) <$> _ccDataTableColumns,
                  Just ("DataTableName" .= _ccDataTableName)])

-- | Contains information about a delivery stream.
--
-- /See:/ 'deliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
    { _dsdCreateTimestamp      :: !(Maybe POSIX)
    , _dsdLastUpdateTimestamp  :: !(Maybe POSIX)
    , _dsdDeliveryStreamName   :: !Text
    , _dsdDeliveryStreamARN    :: !Text
    , _dsdDeliveryStreamStatus :: !DeliveryStreamStatus
    , _dsdVersionId            :: !Text
    , _dsdDestinations         :: ![DestinationDescription]
    , _dsdHasMoreDestinations  :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdCreateTimestamp'
--
-- * 'dsdLastUpdateTimestamp'
--
-- * 'dsdDeliveryStreamName'
--
-- * 'dsdDeliveryStreamARN'
--
-- * 'dsdDeliveryStreamStatus'
--
-- * 'dsdVersionId'
--
-- * 'dsdDestinations'
--
-- * 'dsdHasMoreDestinations'
deliveryStreamDescription
    :: Text -- ^ 'dsdDeliveryStreamName'
    -> Text -- ^ 'dsdDeliveryStreamARN'
    -> DeliveryStreamStatus -- ^ 'dsdDeliveryStreamStatus'
    -> Text -- ^ 'dsdVersionId'
    -> Bool -- ^ 'dsdHasMoreDestinations'
    -> DeliveryStreamDescription
deliveryStreamDescription pDeliveryStreamName_ pDeliveryStreamARN_ pDeliveryStreamStatus_ pVersionId_ pHasMoreDestinations_ =
    DeliveryStreamDescription'
    { _dsdCreateTimestamp = Nothing
    , _dsdLastUpdateTimestamp = Nothing
    , _dsdDeliveryStreamName = pDeliveryStreamName_
    , _dsdDeliveryStreamARN = pDeliveryStreamARN_
    , _dsdDeliveryStreamStatus = pDeliveryStreamStatus_
    , _dsdVersionId = pVersionId_
    , _dsdDestinations = mempty
    , _dsdHasMoreDestinations = pHasMoreDestinations_
    }

-- | The date and time that the delivery stream was created.
dsdCreateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdCreateTimestamp = lens _dsdCreateTimestamp (\ s a -> s{_dsdCreateTimestamp = a}) . mapping _Time;

-- | The date and time that the delivery stream was last updated.
dsdLastUpdateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdLastUpdateTimestamp = lens _dsdLastUpdateTimestamp (\ s a -> s{_dsdLastUpdateTimestamp = a}) . mapping _Time;

-- | The name of the delivery stream.
dsdDeliveryStreamName :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamName = lens _dsdDeliveryStreamName (\ s a -> s{_dsdDeliveryStreamName = a});

-- | The Amazon Resource Name (ARN) of the delivery stream.
dsdDeliveryStreamARN :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamARN = lens _dsdDeliveryStreamARN (\ s a -> s{_dsdDeliveryStreamARN = a});

-- | The status of the delivery stream.
dsdDeliveryStreamStatus :: Lens' DeliveryStreamDescription DeliveryStreamStatus
dsdDeliveryStreamStatus = lens _dsdDeliveryStreamStatus (\ s a -> s{_dsdDeliveryStreamStatus = a});

-- | Used when calling the < UpdateDestination> operation. Each time the
-- destination is updated for the delivery stream, the VersionId is
-- changed, and the current VersionId is required when updating the
-- destination. This is so that the service knows it is applying the
-- changes to the correct version of the delivery stream.
dsdVersionId :: Lens' DeliveryStreamDescription Text
dsdVersionId = lens _dsdVersionId (\ s a -> s{_dsdVersionId = a});

-- | The destinations.
dsdDestinations :: Lens' DeliveryStreamDescription [DestinationDescription]
dsdDestinations = lens _dsdDestinations (\ s a -> s{_dsdDestinations = a}) . _Coerce;

-- | Indicates whether there are more destinations available to list.
dsdHasMoreDestinations :: Lens' DeliveryStreamDescription Bool
dsdHasMoreDestinations = lens _dsdHasMoreDestinations (\ s a -> s{_dsdHasMoreDestinations = a});

instance FromJSON DeliveryStreamDescription where
        parseJSON
          = withObject "DeliveryStreamDescription"
              (\ x ->
                 DeliveryStreamDescription' <$>
                   (x .:? "CreateTimestamp") <*>
                     (x .:? "LastUpdateTimestamp")
                     <*> (x .: "DeliveryStreamName")
                     <*> (x .: "DeliveryStreamARN")
                     <*> (x .: "DeliveryStreamStatus")
                     <*> (x .: "VersionId")
                     <*> (x .:? "Destinations" .!= mempty)
                     <*> (x .: "HasMoreDestinations"))

instance Hashable DeliveryStreamDescription

-- | Describes the destination for a delivery stream.
--
-- /See:/ 'destinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
    { _ddS3DestinationDescription       :: !(Maybe S3DestinationDescription)
    , _ddRedshiftDestinationDescription :: !(Maybe RedshiftDestinationDescription)
    , _ddDestinationId                  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddS3DestinationDescription'
--
-- * 'ddRedshiftDestinationDescription'
--
-- * 'ddDestinationId'
destinationDescription
    :: Text -- ^ 'ddDestinationId'
    -> DestinationDescription
destinationDescription pDestinationId_ =
    DestinationDescription'
    { _ddS3DestinationDescription = Nothing
    , _ddRedshiftDestinationDescription = Nothing
    , _ddDestinationId = pDestinationId_
    }

-- | The Amazon S3 destination.
ddS3DestinationDescription :: Lens' DestinationDescription (Maybe S3DestinationDescription)
ddS3DestinationDescription = lens _ddS3DestinationDescription (\ s a -> s{_ddS3DestinationDescription = a});

-- | The destination in Amazon Redshift.
ddRedshiftDestinationDescription :: Lens' DestinationDescription (Maybe RedshiftDestinationDescription)
ddRedshiftDestinationDescription = lens _ddRedshiftDestinationDescription (\ s a -> s{_ddRedshiftDestinationDescription = a});

-- | The ID of the destination.
ddDestinationId :: Lens' DestinationDescription Text
ddDestinationId = lens _ddDestinationId (\ s a -> s{_ddDestinationId = a});

instance FromJSON DestinationDescription where
        parseJSON
          = withObject "DestinationDescription"
              (\ x ->
                 DestinationDescription' <$>
                   (x .:? "S3DestinationDescription") <*>
                     (x .:? "RedshiftDestinationDescription")
                     <*> (x .: "DestinationId"))

instance Hashable DestinationDescription

-- | Describes the encryption for a destination in Amazon S3.
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
    { _ecNoEncryptionConfig  :: !(Maybe NoEncryptionConfig)
    , _ecKMSEncryptionConfig :: !(Maybe KMSEncryptionConfig)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecNoEncryptionConfig'
--
-- * 'ecKMSEncryptionConfig'
encryptionConfiguration
    :: EncryptionConfiguration
encryptionConfiguration =
    EncryptionConfiguration'
    { _ecNoEncryptionConfig = Nothing
    , _ecKMSEncryptionConfig = Nothing
    }

-- | Specifically override existing encryption information to ensure no
-- encryption is used.
ecNoEncryptionConfig :: Lens' EncryptionConfiguration (Maybe NoEncryptionConfig)
ecNoEncryptionConfig = lens _ecNoEncryptionConfig (\ s a -> s{_ecNoEncryptionConfig = a});

-- | The encryption key.
ecKMSEncryptionConfig :: Lens' EncryptionConfiguration (Maybe KMSEncryptionConfig)
ecKMSEncryptionConfig = lens _ecKMSEncryptionConfig (\ s a -> s{_ecKMSEncryptionConfig = a});

instance FromJSON EncryptionConfiguration where
        parseJSON
          = withObject "EncryptionConfiguration"
              (\ x ->
                 EncryptionConfiguration' <$>
                   (x .:? "NoEncryptionConfig") <*>
                     (x .:? "KMSEncryptionConfig"))

instance Hashable EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration'{..}
          = object
              (catMaybes
                 [("NoEncryptionConfig" .=) <$> _ecNoEncryptionConfig,
                  ("KMSEncryptionConfig" .=) <$>
                    _ecKMSEncryptionConfig])

-- | Describes an encryption key for a destination in Amazon S3.
--
-- /See:/ 'kmsEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
    { _kecAWSKMSKeyARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KMSEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kecAWSKMSKeyARN'
kmsEncryptionConfig
    :: Text -- ^ 'kecAWSKMSKeyARN'
    -> KMSEncryptionConfig
kmsEncryptionConfig pAWSKMSKeyARN_ =
    KMSEncryptionConfig'
    { _kecAWSKMSKeyARN = pAWSKMSKeyARN_
    }

-- | The ARN of the encryption key. Must belong to the same region as the
-- destination Amazon S3 bucket.
kecAWSKMSKeyARN :: Lens' KMSEncryptionConfig Text
kecAWSKMSKeyARN = lens _kecAWSKMSKeyARN (\ s a -> s{_kecAWSKMSKeyARN = a});

instance FromJSON KMSEncryptionConfig where
        parseJSON
          = withObject "KMSEncryptionConfig"
              (\ x ->
                 KMSEncryptionConfig' <$> (x .: "AWSKMSKeyARN"))

instance Hashable KMSEncryptionConfig

instance ToJSON KMSEncryptionConfig where
        toJSON KMSEncryptionConfig'{..}
          = object
              (catMaybes
                 [Just ("AWSKMSKeyARN" .= _kecAWSKMSKeyARN)])

-- | Contains the result for an individual record from a < PutRecordBatch>
-- request. If the record is successfully added to your delivery stream, it
-- receives a record ID. If the record fails to be added to your delivery
-- stream, the result includes an error code and an error message.
--
-- /See:/ 'putRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
    { _prbreRecordId     :: !(Maybe Text)
    , _prbreErrorCode    :: !(Maybe Text)
    , _prbreErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbreRecordId'
--
-- * 'prbreErrorCode'
--
-- * 'prbreErrorMessage'
putRecordBatchResponseEntry
    :: PutRecordBatchResponseEntry
putRecordBatchResponseEntry =
    PutRecordBatchResponseEntry'
    { _prbreRecordId = Nothing
    , _prbreErrorCode = Nothing
    , _prbreErrorMessage = Nothing
    }

-- | The ID of the record.
prbreRecordId :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreRecordId = lens _prbreRecordId (\ s a -> s{_prbreRecordId = a});

-- | The error code for an individual record result.
prbreErrorCode :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorCode = lens _prbreErrorCode (\ s a -> s{_prbreErrorCode = a});

-- | The error message for an individual record result.
prbreErrorMessage :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorMessage = lens _prbreErrorMessage (\ s a -> s{_prbreErrorMessage = a});

instance FromJSON PutRecordBatchResponseEntry where
        parseJSON
          = withObject "PutRecordBatchResponseEntry"
              (\ x ->
                 PutRecordBatchResponseEntry' <$>
                   (x .:? "RecordId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable PutRecordBatchResponseEntry

-- | The unit of data in a delivery stream.
--
-- /See:/ 'record' smart constructor.
newtype Record = Record'
    { _rData :: Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rData'
record
    :: ByteString -- ^ 'rData'
    -> Record
record pData_ =
    Record'
    { _rData = _Base64 # pData_
    }

-- | The data blob, which is base64-encoded when the blob is serialized. The
-- maximum size of the data blob, before base64-encoding, is 1,000 KB.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
rData :: Lens' Record ByteString
rData = lens _rData (\ s a -> s{_rData = a}) . _Base64;

instance Hashable Record

instance ToJSON Record where
        toJSON Record'{..}
          = object (catMaybes [Just ("Data" .= _rData)])

-- | Describes the configuration of a destination in Amazon Redshift.
--
-- /See:/ 'redshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
    { _rdcRoleARN         :: !Text
    , _rdcClusterJDBCURL  :: !Text
    , _rdcCopyCommand     :: !CopyCommand
    , _rdcUsername        :: !(Sensitive Text)
    , _rdcPassword        :: !(Sensitive Text)
    , _rdcS3Configuration :: !S3DestinationConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcRoleARN'
--
-- * 'rdcClusterJDBCURL'
--
-- * 'rdcCopyCommand'
--
-- * 'rdcUsername'
--
-- * 'rdcPassword'
--
-- * 'rdcS3Configuration'
redshiftDestinationConfiguration
    :: Text -- ^ 'rdcRoleARN'
    -> Text -- ^ 'rdcClusterJDBCURL'
    -> CopyCommand -- ^ 'rdcCopyCommand'
    -> Text -- ^ 'rdcUsername'
    -> Text -- ^ 'rdcPassword'
    -> S3DestinationConfiguration -- ^ 'rdcS3Configuration'
    -> RedshiftDestinationConfiguration
redshiftDestinationConfiguration pRoleARN_ pClusterJDBCURL_ pCopyCommand_ pUsername_ pPassword_ pS3Configuration_ =
    RedshiftDestinationConfiguration'
    { _rdcRoleARN = pRoleARN_
    , _rdcClusterJDBCURL = pClusterJDBCURL_
    , _rdcCopyCommand = pCopyCommand_
    , _rdcUsername = _Sensitive # pUsername_
    , _rdcPassword = _Sensitive # pPassword_
    , _rdcS3Configuration = pS3Configuration_
    }

-- | The ARN of the AWS credentials.
rdcRoleARN :: Lens' RedshiftDestinationConfiguration Text
rdcRoleARN = lens _rdcRoleARN (\ s a -> s{_rdcRoleARN = a});

-- | The database connection string.
rdcClusterJDBCURL :: Lens' RedshiftDestinationConfiguration Text
rdcClusterJDBCURL = lens _rdcClusterJDBCURL (\ s a -> s{_rdcClusterJDBCURL = a});

-- | The 'COPY' command.
rdcCopyCommand :: Lens' RedshiftDestinationConfiguration CopyCommand
rdcCopyCommand = lens _rdcCopyCommand (\ s a -> s{_rdcCopyCommand = a});

-- | The name of the user.
rdcUsername :: Lens' RedshiftDestinationConfiguration Text
rdcUsername = lens _rdcUsername (\ s a -> s{_rdcUsername = a}) . _Sensitive;

-- | The user password.
rdcPassword :: Lens' RedshiftDestinationConfiguration Text
rdcPassword = lens _rdcPassword (\ s a -> s{_rdcPassword = a}) . _Sensitive;

-- | The S3 configuration for the intermediate location from which Amazon
-- Redshift obtains data. Restrictions are described in the topic for
-- < CreateDeliveryStream>.
--
-- The compression formats 'SNAPPY' or 'ZIP' cannot be specified in
-- 'RedshiftDestinationConfiguration.S3Configuration' because the Amazon
-- Redshift 'COPY' operation that reads from the S3 bucket doesn\'t support
-- these compression formats.
rdcS3Configuration :: Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
rdcS3Configuration = lens _rdcS3Configuration (\ s a -> s{_rdcS3Configuration = a});

instance Hashable RedshiftDestinationConfiguration

instance ToJSON RedshiftDestinationConfiguration
         where
        toJSON RedshiftDestinationConfiguration'{..}
          = object
              (catMaybes
                 [Just ("RoleARN" .= _rdcRoleARN),
                  Just ("ClusterJDBCURL" .= _rdcClusterJDBCURL),
                  Just ("CopyCommand" .= _rdcCopyCommand),
                  Just ("Username" .= _rdcUsername),
                  Just ("Password" .= _rdcPassword),
                  Just ("S3Configuration" .= _rdcS3Configuration)])

-- | Describes a destination in Amazon Redshift.
--
-- /See:/ 'redshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
    { _rddRoleARN                  :: !Text
    , _rddClusterJDBCURL           :: !Text
    , _rddCopyCommand              :: !CopyCommand
    , _rddUsername                 :: !(Sensitive Text)
    , _rddS3DestinationDescription :: !S3DestinationDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rddRoleARN'
--
-- * 'rddClusterJDBCURL'
--
-- * 'rddCopyCommand'
--
-- * 'rddUsername'
--
-- * 'rddS3DestinationDescription'
redshiftDestinationDescription
    :: Text -- ^ 'rddRoleARN'
    -> Text -- ^ 'rddClusterJDBCURL'
    -> CopyCommand -- ^ 'rddCopyCommand'
    -> Text -- ^ 'rddUsername'
    -> S3DestinationDescription -- ^ 'rddS3DestinationDescription'
    -> RedshiftDestinationDescription
redshiftDestinationDescription pRoleARN_ pClusterJDBCURL_ pCopyCommand_ pUsername_ pS3DestinationDescription_ =
    RedshiftDestinationDescription'
    { _rddRoleARN = pRoleARN_
    , _rddClusterJDBCURL = pClusterJDBCURL_
    , _rddCopyCommand = pCopyCommand_
    , _rddUsername = _Sensitive # pUsername_
    , _rddS3DestinationDescription = pS3DestinationDescription_
    }

-- | The ARN of the AWS credentials.
rddRoleARN :: Lens' RedshiftDestinationDescription Text
rddRoleARN = lens _rddRoleARN (\ s a -> s{_rddRoleARN = a});

-- | The database connection string.
rddClusterJDBCURL :: Lens' RedshiftDestinationDescription Text
rddClusterJDBCURL = lens _rddClusterJDBCURL (\ s a -> s{_rddClusterJDBCURL = a});

-- | The 'COPY' command.
rddCopyCommand :: Lens' RedshiftDestinationDescription CopyCommand
rddCopyCommand = lens _rddCopyCommand (\ s a -> s{_rddCopyCommand = a});

-- | The name of the user.
rddUsername :: Lens' RedshiftDestinationDescription Text
rddUsername = lens _rddUsername (\ s a -> s{_rddUsername = a}) . _Sensitive;

-- | The Amazon S3 destination.
rddS3DestinationDescription :: Lens' RedshiftDestinationDescription S3DestinationDescription
rddS3DestinationDescription = lens _rddS3DestinationDescription (\ s a -> s{_rddS3DestinationDescription = a});

instance FromJSON RedshiftDestinationDescription
         where
        parseJSON
          = withObject "RedshiftDestinationDescription"
              (\ x ->
                 RedshiftDestinationDescription' <$>
                   (x .: "RoleARN") <*> (x .: "ClusterJDBCURL") <*>
                     (x .: "CopyCommand")
                     <*> (x .: "Username")
                     <*> (x .: "S3DestinationDescription"))

instance Hashable RedshiftDestinationDescription

-- | Describes an update for a destination in Amazon Redshift.
--
-- /See:/ 'redshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
    { _rduUsername       :: !(Maybe (Sensitive Text))
    , _rduS3Update       :: !(Maybe S3DestinationUpdate)
    , _rduPassword       :: !(Maybe (Sensitive Text))
    , _rduCopyCommand    :: !(Maybe CopyCommand)
    , _rduClusterJDBCURL :: !(Maybe Text)
    , _rduRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rduUsername'
--
-- * 'rduS3Update'
--
-- * 'rduPassword'
--
-- * 'rduCopyCommand'
--
-- * 'rduClusterJDBCURL'
--
-- * 'rduRoleARN'
redshiftDestinationUpdate
    :: RedshiftDestinationUpdate
redshiftDestinationUpdate =
    RedshiftDestinationUpdate'
    { _rduUsername = Nothing
    , _rduS3Update = Nothing
    , _rduPassword = Nothing
    , _rduCopyCommand = Nothing
    , _rduClusterJDBCURL = Nothing
    , _rduRoleARN = Nothing
    }

-- | The name of the user.
rduUsername :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduUsername = lens _rduUsername (\ s a -> s{_rduUsername = a}) . mapping _Sensitive;

-- | The Amazon S3 destination.
--
-- The compression formats 'SNAPPY' or 'ZIP' cannot be specified in
-- 'RedshiftDestinationUpdate.S3Update' because the Amazon Redshift 'COPY'
-- operation that reads from the S3 bucket doesn\'t support these
-- compression formats.
rduS3Update :: Lens' RedshiftDestinationUpdate (Maybe S3DestinationUpdate)
rduS3Update = lens _rduS3Update (\ s a -> s{_rduS3Update = a});

-- | The user password.
rduPassword :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduPassword = lens _rduPassword (\ s a -> s{_rduPassword = a}) . mapping _Sensitive;

-- | The 'COPY' command.
rduCopyCommand :: Lens' RedshiftDestinationUpdate (Maybe CopyCommand)
rduCopyCommand = lens _rduCopyCommand (\ s a -> s{_rduCopyCommand = a});

-- | The database connection string.
rduClusterJDBCURL :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduClusterJDBCURL = lens _rduClusterJDBCURL (\ s a -> s{_rduClusterJDBCURL = a});

-- | The ARN of the AWS credentials.
rduRoleARN :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduRoleARN = lens _rduRoleARN (\ s a -> s{_rduRoleARN = a});

instance Hashable RedshiftDestinationUpdate

instance ToJSON RedshiftDestinationUpdate where
        toJSON RedshiftDestinationUpdate'{..}
          = object
              (catMaybes
                 [("Username" .=) <$> _rduUsername,
                  ("S3Update" .=) <$> _rduS3Update,
                  ("Password" .=) <$> _rduPassword,
                  ("CopyCommand" .=) <$> _rduCopyCommand,
                  ("ClusterJDBCURL" .=) <$> _rduClusterJDBCURL,
                  ("RoleARN" .=) <$> _rduRoleARN])

-- | Describes the configuration of a destination in Amazon S3.
--
-- /See:/ 's3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
    { _sdcPrefix                  :: !(Maybe Text)
    , _sdcEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
    , _sdcCompressionFormat       :: !(Maybe CompressionFormat)
    , _sdcBufferingHints          :: !(Maybe BufferingHints)
    , _sdcRoleARN                 :: !Text
    , _sdcBucketARN               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcPrefix'
--
-- * 'sdcEncryptionConfiguration'
--
-- * 'sdcCompressionFormat'
--
-- * 'sdcBufferingHints'
--
-- * 'sdcRoleARN'
--
-- * 'sdcBucketARN'
s3DestinationConfiguration
    :: Text -- ^ 'sdcRoleARN'
    -> Text -- ^ 'sdcBucketARN'
    -> S3DestinationConfiguration
s3DestinationConfiguration pRoleARN_ pBucketARN_ =
    S3DestinationConfiguration'
    { _sdcPrefix = Nothing
    , _sdcEncryptionConfiguration = Nothing
    , _sdcCompressionFormat = Nothing
    , _sdcBufferingHints = Nothing
    , _sdcRoleARN = pRoleARN_
    , _sdcBucketARN = pBucketARN_
    }

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered S3 files. You can specify an extra prefix to be added in front
-- of the time format prefix. Note that if the prefix ends with a slash, it
-- appears as a folder in the S3 bucket. For more information, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format>
-- in the <http://docs.aws.amazon.com/firehose/latest/dev/ guide-fh-dev>.
sdcPrefix :: Lens' S3DestinationConfiguration (Maybe Text)
sdcPrefix = lens _sdcPrefix (\ s a -> s{_sdcPrefix = a});

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
sdcEncryptionConfiguration :: Lens' S3DestinationConfiguration (Maybe EncryptionConfiguration)
sdcEncryptionConfiguration = lens _sdcEncryptionConfiguration (\ s a -> s{_sdcEncryptionConfiguration = a});

-- | The compression format. If no value is specified, the default is
-- 'UNCOMPRESSED'.
--
-- The compression formats 'SNAPPY' or 'ZIP' cannot be specified for Amazon
-- Redshift destinations because they are not supported by the Amazon
-- Redshift 'COPY' operation that reads from the S3 bucket.
sdcCompressionFormat :: Lens' S3DestinationConfiguration (Maybe CompressionFormat)
sdcCompressionFormat = lens _sdcCompressionFormat (\ s a -> s{_sdcCompressionFormat = a});

-- | The buffering option. If no value is specified, 'BufferingHints' object
-- default values are used.
sdcBufferingHints :: Lens' S3DestinationConfiguration (Maybe BufferingHints)
sdcBufferingHints = lens _sdcBufferingHints (\ s a -> s{_sdcBufferingHints = a});

-- | The ARN of the AWS credentials.
sdcRoleARN :: Lens' S3DestinationConfiguration Text
sdcRoleARN = lens _sdcRoleARN (\ s a -> s{_sdcRoleARN = a});

-- | The ARN of the S3 bucket.
sdcBucketARN :: Lens' S3DestinationConfiguration Text
sdcBucketARN = lens _sdcBucketARN (\ s a -> s{_sdcBucketARN = a});

instance Hashable S3DestinationConfiguration

instance ToJSON S3DestinationConfiguration where
        toJSON S3DestinationConfiguration'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _sdcPrefix,
                  ("EncryptionConfiguration" .=) <$>
                    _sdcEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _sdcCompressionFormat,
                  ("BufferingHints" .=) <$> _sdcBufferingHints,
                  Just ("RoleARN" .= _sdcRoleARN),
                  Just ("BucketARN" .= _sdcBucketARN)])

-- | Describes a destination in Amazon S3.
--
-- /See:/ 's3DestinationDescription' smart constructor.
data S3DestinationDescription = S3DestinationDescription'
    { _sddPrefix                  :: !(Maybe Text)
    , _sddRoleARN                 :: !Text
    , _sddBucketARN               :: !Text
    , _sddBufferingHints          :: !BufferingHints
    , _sddCompressionFormat       :: !CompressionFormat
    , _sddEncryptionConfiguration :: !EncryptionConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sddPrefix'
--
-- * 'sddRoleARN'
--
-- * 'sddBucketARN'
--
-- * 'sddBufferingHints'
--
-- * 'sddCompressionFormat'
--
-- * 'sddEncryptionConfiguration'
s3DestinationDescription
    :: Text -- ^ 'sddRoleARN'
    -> Text -- ^ 'sddBucketARN'
    -> BufferingHints -- ^ 'sddBufferingHints'
    -> CompressionFormat -- ^ 'sddCompressionFormat'
    -> EncryptionConfiguration -- ^ 'sddEncryptionConfiguration'
    -> S3DestinationDescription
s3DestinationDescription pRoleARN_ pBucketARN_ pBufferingHints_ pCompressionFormat_ pEncryptionConfiguration_ =
    S3DestinationDescription'
    { _sddPrefix = Nothing
    , _sddRoleARN = pRoleARN_
    , _sddBucketARN = pBucketARN_
    , _sddBufferingHints = pBufferingHints_
    , _sddCompressionFormat = pCompressionFormat_
    , _sddEncryptionConfiguration = pEncryptionConfiguration_
    }

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered S3 files. You can specify an extra prefix to be added in front
-- of the time format prefix. Note that if the prefix ends with a slash, it
-- appears as a folder in the S3 bucket. For more information, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format>
-- in the <http://docs.aws.amazon.com/firehose/latest/dev/ guide-fh-dev>.
sddPrefix :: Lens' S3DestinationDescription (Maybe Text)
sddPrefix = lens _sddPrefix (\ s a -> s{_sddPrefix = a});

-- | The ARN of the AWS credentials.
sddRoleARN :: Lens' S3DestinationDescription Text
sddRoleARN = lens _sddRoleARN (\ s a -> s{_sddRoleARN = a});

-- | The ARN of the S3 bucket.
sddBucketARN :: Lens' S3DestinationDescription Text
sddBucketARN = lens _sddBucketARN (\ s a -> s{_sddBucketARN = a});

-- | The buffering option. If no value is specified, 'BufferingHints' object
-- default values are used.
sddBufferingHints :: Lens' S3DestinationDescription BufferingHints
sddBufferingHints = lens _sddBufferingHints (\ s a -> s{_sddBufferingHints = a});

-- | The compression format. If no value is specified, the default is
-- 'NOCOMPRESSION'.
sddCompressionFormat :: Lens' S3DestinationDescription CompressionFormat
sddCompressionFormat = lens _sddCompressionFormat (\ s a -> s{_sddCompressionFormat = a});

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
sddEncryptionConfiguration :: Lens' S3DestinationDescription EncryptionConfiguration
sddEncryptionConfiguration = lens _sddEncryptionConfiguration (\ s a -> s{_sddEncryptionConfiguration = a});

instance FromJSON S3DestinationDescription where
        parseJSON
          = withObject "S3DestinationDescription"
              (\ x ->
                 S3DestinationDescription' <$>
                   (x .:? "Prefix") <*> (x .: "RoleARN") <*>
                     (x .: "BucketARN")
                     <*> (x .: "BufferingHints")
                     <*> (x .: "CompressionFormat")
                     <*> (x .: "EncryptionConfiguration"))

instance Hashable S3DestinationDescription

-- | Describes an update for a destination in Amazon S3.
--
-- /See:/ 's3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
    { _sduPrefix                  :: !(Maybe Text)
    , _sduEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
    , _sduCompressionFormat       :: !(Maybe CompressionFormat)
    , _sduBufferingHints          :: !(Maybe BufferingHints)
    , _sduBucketARN               :: !(Maybe Text)
    , _sduRoleARN                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sduPrefix'
--
-- * 'sduEncryptionConfiguration'
--
-- * 'sduCompressionFormat'
--
-- * 'sduBufferingHints'
--
-- * 'sduBucketARN'
--
-- * 'sduRoleARN'
s3DestinationUpdate
    :: S3DestinationUpdate
s3DestinationUpdate =
    S3DestinationUpdate'
    { _sduPrefix = Nothing
    , _sduEncryptionConfiguration = Nothing
    , _sduCompressionFormat = Nothing
    , _sduBufferingHints = Nothing
    , _sduBucketARN = Nothing
    , _sduRoleARN = Nothing
    }

-- | The \"YYYY\/MM\/DD\/HH\" time format prefix is automatically used for
-- delivered S3 files. You can specify an extra prefix to be added in front
-- of the time format prefix. Note that if the prefix ends with a slash, it
-- appears as a folder in the S3 bucket. For more information, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format>
-- in the <http://docs.aws.amazon.com/firehose/latest/dev/ guide-fh-dev>.
sduPrefix :: Lens' S3DestinationUpdate (Maybe Text)
sduPrefix = lens _sduPrefix (\ s a -> s{_sduPrefix = a});

-- | The encryption configuration. If no value is specified, the default is
-- no encryption.
sduEncryptionConfiguration :: Lens' S3DestinationUpdate (Maybe EncryptionConfiguration)
sduEncryptionConfiguration = lens _sduEncryptionConfiguration (\ s a -> s{_sduEncryptionConfiguration = a});

-- | The compression format. If no value is specified, the default is
-- 'NOCOMPRESSION'.
--
-- The compression formats 'SNAPPY' or 'ZIP' cannot be specified for Amazon
-- Redshift destinations because they are not supported by the Amazon
-- Redshift 'COPY' operation that reads from the S3 bucket.
sduCompressionFormat :: Lens' S3DestinationUpdate (Maybe CompressionFormat)
sduCompressionFormat = lens _sduCompressionFormat (\ s a -> s{_sduCompressionFormat = a});

-- | The buffering option. If no value is specified, 'BufferingHints' object
-- default values are used.
sduBufferingHints :: Lens' S3DestinationUpdate (Maybe BufferingHints)
sduBufferingHints = lens _sduBufferingHints (\ s a -> s{_sduBufferingHints = a});

-- | The ARN of the S3 bucket.
sduBucketARN :: Lens' S3DestinationUpdate (Maybe Text)
sduBucketARN = lens _sduBucketARN (\ s a -> s{_sduBucketARN = a});

-- | The ARN of the AWS credentials.
sduRoleARN :: Lens' S3DestinationUpdate (Maybe Text)
sduRoleARN = lens _sduRoleARN (\ s a -> s{_sduRoleARN = a});

instance Hashable S3DestinationUpdate

instance ToJSON S3DestinationUpdate where
        toJSON S3DestinationUpdate'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _sduPrefix,
                  ("EncryptionConfiguration" .=) <$>
                    _sduEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _sduCompressionFormat,
                  ("BufferingHints" .=) <$> _sduBufferingHints,
                  ("BucketARN" .=) <$> _sduBucketARN,
                  ("RoleARN" .=) <$> _sduRoleARN])

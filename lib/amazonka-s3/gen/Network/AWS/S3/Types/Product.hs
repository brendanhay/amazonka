{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Sum

-- | Specifies the days since the initiation of an Incomplete Multipart Upload that Lifecycle will wait before permanently removing all parts of the upload.
--
-- /See:/ 'abortIncompleteMultipartUpload' smart constructor.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { _aimuDaysAfterInitiation :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortIncompleteMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aimuDaysAfterInitiation' - Indicates the number of days that must pass since initiation for Lifecycle to abort an Incomplete Multipart Upload.
abortIncompleteMultipartUpload
    :: AbortIncompleteMultipartUpload
abortIncompleteMultipartUpload =
  AbortIncompleteMultipartUpload' {_aimuDaysAfterInitiation = Nothing}


-- | Indicates the number of days that must pass since initiation for Lifecycle to abort an Incomplete Multipart Upload.
aimuDaysAfterInitiation :: Lens' AbortIncompleteMultipartUpload (Maybe Int)
aimuDaysAfterInitiation = lens _aimuDaysAfterInitiation (\ s a -> s{_aimuDaysAfterInitiation = a})

instance FromXML AbortIncompleteMultipartUpload where
        parseXML x
          = AbortIncompleteMultipartUpload' <$>
              (x .@? "DaysAfterInitiation")

instance Hashable AbortIncompleteMultipartUpload
         where

instance NFData AbortIncompleteMultipartUpload where

instance ToXML AbortIncompleteMultipartUpload where
        toXML AbortIncompleteMultipartUpload'{..}
          = mconcat
              ["DaysAfterInitiation" @= _aimuDaysAfterInitiation]

-- | /See:/ 'accelerateConfiguration' smart constructor.
newtype AccelerateConfiguration = AccelerateConfiguration'
  { _acStatus :: Maybe BucketAccelerateStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acStatus' - The accelerate configuration of the bucket.
accelerateConfiguration
    :: AccelerateConfiguration
accelerateConfiguration = AccelerateConfiguration' {_acStatus = Nothing}


-- | The accelerate configuration of the bucket.
acStatus :: Lens' AccelerateConfiguration (Maybe BucketAccelerateStatus)
acStatus = lens _acStatus (\ s a -> s{_acStatus = a})

instance Hashable AccelerateConfiguration where

instance NFData AccelerateConfiguration where

instance ToXML AccelerateConfiguration where
        toXML AccelerateConfiguration'{..}
          = mconcat ["Status" @= _acStatus]

-- | /See:/ 'accessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { _acpGrants :: !(Maybe [Grant])
  , _acpOwner  :: !(Maybe Owner)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessControlPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acpGrants' - A list of grants.
--
-- * 'acpOwner' - Undocumented member.
accessControlPolicy
    :: AccessControlPolicy
accessControlPolicy =
  AccessControlPolicy' {_acpGrants = Nothing, _acpOwner = Nothing}


-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\ s a -> s{_acpGrants = a}) . _Default . _Coerce

-- | Undocumented member.
acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\ s a -> s{_acpOwner = a})

instance Hashable AccessControlPolicy where

instance NFData AccessControlPolicy where

instance ToXML AccessControlPolicy where
        toXML AccessControlPolicy'{..}
          = mconcat
              ["AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _acpGrants),
               "Owner" @= _acpOwner]

-- | Container for information regarding the access control for replicas.
--
-- /See:/ 'accessControlTranslation' smart constructor.
newtype AccessControlTranslation = AccessControlTranslation'
  { _actOwner :: OwnerOverride
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessControlTranslation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actOwner' - The override value for the owner of the replica object.
accessControlTranslation
    :: OwnerOverride -- ^ 'actOwner'
    -> AccessControlTranslation
accessControlTranslation pOwner_ =
  AccessControlTranslation' {_actOwner = pOwner_}


-- | The override value for the owner of the replica object.
actOwner :: Lens' AccessControlTranslation OwnerOverride
actOwner = lens _actOwner (\ s a -> s{_actOwner = a})

instance FromXML AccessControlTranslation where
        parseXML x
          = AccessControlTranslation' <$> (x .@ "Owner")

instance Hashable AccessControlTranslation where

instance NFData AccessControlTranslation where

instance ToXML AccessControlTranslation where
        toXML AccessControlTranslation'{..}
          = mconcat ["Owner" @= _actOwner]

-- | /See:/ 'analyticsAndOperator' smart constructor.
data AnalyticsAndOperator = AnalyticsAndOperator'
  { _aaoPrefix :: !(Maybe Text)
  , _aaoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaoPrefix' - The prefix to use when evaluating an AND predicate.
--
-- * 'aaoTags' - The list of tags to use when evaluating an AND predicate.
analyticsAndOperator
    :: AnalyticsAndOperator
analyticsAndOperator =
  AnalyticsAndOperator' {_aaoPrefix = Nothing, _aaoTags = Nothing}


-- | The prefix to use when evaluating an AND predicate.
aaoPrefix :: Lens' AnalyticsAndOperator (Maybe Text)
aaoPrefix = lens _aaoPrefix (\ s a -> s{_aaoPrefix = a})

-- | The list of tags to use when evaluating an AND predicate.
aaoTags :: Lens' AnalyticsAndOperator [Tag]
aaoTags = lens _aaoTags (\ s a -> s{_aaoTags = a}) . _Default . _Coerce

instance FromXML AnalyticsAndOperator where
        parseXML x
          = AnalyticsAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable AnalyticsAndOperator where

instance NFData AnalyticsAndOperator where

instance ToXML AnalyticsAndOperator where
        toXML AnalyticsAndOperator'{..}
          = mconcat
              ["Prefix" @= _aaoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _aaoTags)]

-- | /See:/ 'analyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { _acFilter               :: !(Maybe AnalyticsFilter)
  , _acId                   :: !Text
  , _acStorageClassAnalysis :: !StorageClassAnalysis
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acFilter' - The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- * 'acId' - The identifier used to represent an analytics configuration.
--
-- * 'acStorageClassAnalysis' - If present, it indicates that data related to access patterns will be collected and made available to analyze the tradeoffs between different storage classes.
analyticsConfiguration
    :: Text -- ^ 'acId'
    -> StorageClassAnalysis -- ^ 'acStorageClassAnalysis'
    -> AnalyticsConfiguration
analyticsConfiguration pId_ pStorageClassAnalysis_ =
  AnalyticsConfiguration'
    { _acFilter = Nothing
    , _acId = pId_
    , _acStorageClassAnalysis = pStorageClassAnalysis_
    }


-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
acFilter :: Lens' AnalyticsConfiguration (Maybe AnalyticsFilter)
acFilter = lens _acFilter (\ s a -> s{_acFilter = a})

-- | The identifier used to represent an analytics configuration.
acId :: Lens' AnalyticsConfiguration Text
acId = lens _acId (\ s a -> s{_acId = a})

-- | If present, it indicates that data related to access patterns will be collected and made available to analyze the tradeoffs between different storage classes.
acStorageClassAnalysis :: Lens' AnalyticsConfiguration StorageClassAnalysis
acStorageClassAnalysis = lens _acStorageClassAnalysis (\ s a -> s{_acStorageClassAnalysis = a})

instance FromXML AnalyticsConfiguration where
        parseXML x
          = AnalyticsConfiguration' <$>
              (x .@? "Filter") <*> (x .@ "Id") <*>
                (x .@ "StorageClassAnalysis")

instance Hashable AnalyticsConfiguration where

instance NFData AnalyticsConfiguration where

instance ToXML AnalyticsConfiguration where
        toXML AnalyticsConfiguration'{..}
          = mconcat
              ["Filter" @= _acFilter, "Id" @= _acId,
               "StorageClassAnalysis" @= _acStorageClassAnalysis]

-- | /See:/ 'analyticsExportDestination' smart constructor.
newtype AnalyticsExportDestination = AnalyticsExportDestination'
  { _aedS3BucketDestination :: AnalyticsS3BucketDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsExportDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aedS3BucketDestination' - A destination signifying output to an S3 bucket.
analyticsExportDestination
    :: AnalyticsS3BucketDestination -- ^ 'aedS3BucketDestination'
    -> AnalyticsExportDestination
analyticsExportDestination pS3BucketDestination_ =
  AnalyticsExportDestination' {_aedS3BucketDestination = pS3BucketDestination_}


-- | A destination signifying output to an S3 bucket.
aedS3BucketDestination :: Lens' AnalyticsExportDestination AnalyticsS3BucketDestination
aedS3BucketDestination = lens _aedS3BucketDestination (\ s a -> s{_aedS3BucketDestination = a})

instance FromXML AnalyticsExportDestination where
        parseXML x
          = AnalyticsExportDestination' <$>
              (x .@ "S3BucketDestination")

instance Hashable AnalyticsExportDestination where

instance NFData AnalyticsExportDestination where

instance ToXML AnalyticsExportDestination where
        toXML AnalyticsExportDestination'{..}
          = mconcat
              ["S3BucketDestination" @= _aedS3BucketDestination]

-- | /See:/ 'analyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { _afTag    :: !(Maybe Tag)
  , _afPrefix :: !(Maybe Text)
  , _afAnd    :: !(Maybe AnalyticsAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afTag' - The tag to use when evaluating an analytics filter.
--
-- * 'afPrefix' - The prefix to use when evaluating an analytics filter.
--
-- * 'afAnd' - A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
analyticsFilter
    :: AnalyticsFilter
analyticsFilter =
  AnalyticsFilter' {_afTag = Nothing, _afPrefix = Nothing, _afAnd = Nothing}


-- | The tag to use when evaluating an analytics filter.
afTag :: Lens' AnalyticsFilter (Maybe Tag)
afTag = lens _afTag (\ s a -> s{_afTag = a})

-- | The prefix to use when evaluating an analytics filter.
afPrefix :: Lens' AnalyticsFilter (Maybe Text)
afPrefix = lens _afPrefix (\ s a -> s{_afPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
afAnd :: Lens' AnalyticsFilter (Maybe AnalyticsAndOperator)
afAnd = lens _afAnd (\ s a -> s{_afAnd = a})

instance FromXML AnalyticsFilter where
        parseXML x
          = AnalyticsFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable AnalyticsFilter where

instance NFData AnalyticsFilter where

instance ToXML AnalyticsFilter where
        toXML AnalyticsFilter'{..}
          = mconcat
              ["Tag" @= _afTag, "Prefix" @= _afPrefix,
               "And" @= _afAnd]

-- | /See:/ 'analyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { _asbdBucketAccountId :: !(Maybe Text)
  , _asbdPrefix          :: !(Maybe Text)
  , _asbdFormat          :: !AnalyticsS3ExportFileFormat
  , _asbdBucket          :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnalyticsS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asbdBucketAccountId' - The account ID that owns the destination bucket. If no account ID is provided, the owner will not be validated prior to exporting data.
--
-- * 'asbdPrefix' - The prefix to use when exporting data. The exported data begins with this prefix.
--
-- * 'asbdFormat' - The file format used when exporting data to Amazon S3.
--
-- * 'asbdBucket' - The Amazon resource name (ARN) of the bucket to which data is exported.
analyticsS3BucketDestination
    :: AnalyticsS3ExportFileFormat -- ^ 'asbdFormat'
    -> BucketName -- ^ 'asbdBucket'
    -> AnalyticsS3BucketDestination
analyticsS3BucketDestination pFormat_ pBucket_ =
  AnalyticsS3BucketDestination'
    { _asbdBucketAccountId = Nothing
    , _asbdPrefix = Nothing
    , _asbdFormat = pFormat_
    , _asbdBucket = pBucket_
    }


-- | The account ID that owns the destination bucket. If no account ID is provided, the owner will not be validated prior to exporting data.
asbdBucketAccountId :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdBucketAccountId = lens _asbdBucketAccountId (\ s a -> s{_asbdBucketAccountId = a})

-- | The prefix to use when exporting data. The exported data begins with this prefix.
asbdPrefix :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdPrefix = lens _asbdPrefix (\ s a -> s{_asbdPrefix = a})

-- | The file format used when exporting data to Amazon S3.
asbdFormat :: Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
asbdFormat = lens _asbdFormat (\ s a -> s{_asbdFormat = a})

-- | The Amazon resource name (ARN) of the bucket to which data is exported.
asbdBucket :: Lens' AnalyticsS3BucketDestination BucketName
asbdBucket = lens _asbdBucket (\ s a -> s{_asbdBucket = a})

instance FromXML AnalyticsS3BucketDestination where
        parseXML x
          = AnalyticsS3BucketDestination' <$>
              (x .@? "BucketAccountId") <*> (x .@? "Prefix") <*>
                (x .@ "Format")
                <*> (x .@ "Bucket")

instance Hashable AnalyticsS3BucketDestination where

instance NFData AnalyticsS3BucketDestination where

instance ToXML AnalyticsS3BucketDestination where
        toXML AnalyticsS3BucketDestination'{..}
          = mconcat
              ["BucketAccountId" @= _asbdBucketAccountId,
               "Prefix" @= _asbdPrefix, "Format" @= _asbdFormat,
               "Bucket" @= _asbdBucket]

-- | /See:/ 'bucket' smart constructor.
data Bucket = Bucket'
  { _bCreationDate :: !RFC822
  , _bName         :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationDate' - Date the bucket was created.
--
-- * 'bName' - The name of the bucket.
bucket
    :: UTCTime -- ^ 'bCreationDate'
    -> BucketName -- ^ 'bName'
    -> Bucket
bucket pCreationDate_ pName_ =
  Bucket' {_bCreationDate = _Time # pCreationDate_, _bName = pName_}


-- | Date the bucket was created.
bCreationDate :: Lens' Bucket UTCTime
bCreationDate = lens _bCreationDate (\ s a -> s{_bCreationDate = a}) . _Time

-- | The name of the bucket.
bName :: Lens' Bucket BucketName
bName = lens _bName (\ s a -> s{_bName = a})

instance FromXML Bucket where
        parseXML x
          = Bucket' <$> (x .@ "CreationDate") <*> (x .@ "Name")

instance Hashable Bucket where

instance NFData Bucket where

-- | /See:/ 'bucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { _blcRules :: [LifecycleRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blcRules' - Undocumented member.
bucketLifecycleConfiguration
    :: BucketLifecycleConfiguration
bucketLifecycleConfiguration =
  BucketLifecycleConfiguration' {_blcRules = mempty}


-- | Undocumented member.
blcRules :: Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = lens _blcRules (\ s a -> s{_blcRules = a}) . _Coerce

instance Hashable BucketLifecycleConfiguration where

instance NFData BucketLifecycleConfiguration where

instance ToXML BucketLifecycleConfiguration where
        toXML BucketLifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _blcRules]

-- | /See:/ 'bucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
  { _blsLoggingEnabled :: Maybe LoggingEnabled
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BucketLoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blsLoggingEnabled' - Undocumented member.
bucketLoggingStatus
    :: BucketLoggingStatus
bucketLoggingStatus = BucketLoggingStatus' {_blsLoggingEnabled = Nothing}


-- | Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\ s a -> s{_blsLoggingEnabled = a})

instance Hashable BucketLoggingStatus where

instance NFData BucketLoggingStatus where

instance ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus'{..}
          = mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

-- | /See:/ 'corsConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
  { _ccCORSRules :: [CORSRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CORSConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCORSRules' - Undocumented member.
corsConfiguration
    :: CORSConfiguration
corsConfiguration = CORSConfiguration' {_ccCORSRules = mempty}


-- | Undocumented member.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\ s a -> s{_ccCORSRules = a}) . _Coerce

instance Hashable CORSConfiguration where

instance NFData CORSConfiguration where

instance ToXML CORSConfiguration where
        toXML CORSConfiguration'{..}
          = mconcat [toXMLList "CORSRule" _ccCORSRules]

-- | /See:/ 'corsRule' smart constructor.
data CORSRule = CORSRule'
  { _crMaxAgeSeconds  :: !(Maybe Int)
  , _crAllowedHeaders :: !(Maybe [Text])
  , _crExposeHeaders  :: !(Maybe [Text])
  , _crAllowedMethods :: ![Text]
  , _crAllowedOrigins :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crMaxAgeSeconds' - The time in seconds that your browser is to cache the preflight response for the specified resource.
--
-- * 'crAllowedHeaders' - Specifies which headers are allowed in a pre-flight OPTIONS request.
--
-- * 'crExposeHeaders' - One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript XMLHttpRequest object).
--
-- * 'crAllowedMethods' - Identifies HTTP methods that the domain/origin specified in the rule is allowed to execute.
--
-- * 'crAllowedOrigins' - One or more origins you want customers to be able to access the bucket from.
corsRule
    :: CORSRule
corsRule =
  CORSRule'
    { _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crExposeHeaders = Nothing
    , _crAllowedMethods = mempty
    , _crAllowedOrigins = mempty
    }


-- | The time in seconds that your browser is to cache the preflight response for the specified resource.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a})

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default . _Coerce

-- | One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript XMLHttpRequest object).
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default . _Coerce

-- | Identifies HTTP methods that the domain/origin specified in the rule is allowed to execute.
crAllowedMethods :: Lens' CORSRule [Text]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Coerce

-- | One or more origins you want customers to be able to access the bucket from.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Coerce

instance FromXML CORSRule where
        parseXML x
          = CORSRule' <$>
              (x .@? "MaxAgeSeconds") <*>
                (may (parseXMLList "AllowedHeader") x)
                <*> (may (parseXMLList "ExposeHeader") x)
                <*> (parseXMLList "AllowedMethod" x)
                <*> (parseXMLList "AllowedOrigin" x)

instance Hashable CORSRule where

instance NFData CORSRule where

instance ToXML CORSRule where
        toXML CORSRule'{..}
          = mconcat
              ["MaxAgeSeconds" @= _crMaxAgeSeconds,
               toXML
                 (toXMLList "AllowedHeader" <$> _crAllowedHeaders),
               toXML
                 (toXMLList "ExposeHeader" <$> _crExposeHeaders),
               toXMLList "AllowedMethod" _crAllowedMethods,
               toXMLList "AllowedOrigin" _crAllowedOrigins]

-- | Describes how a CSV-formatted input object is formatted.
--
-- /See:/ 'csvInput' smart constructor.
data CSVInput = CSVInput'
  { _ciQuoteCharacter       :: !(Maybe Text)
  , _ciRecordDelimiter      :: !(Maybe Text)
  , _ciFileHeaderInfo       :: !(Maybe FileHeaderInfo)
  , _ciQuoteEscapeCharacter :: !(Maybe Text)
  , _ciComments             :: !(Maybe Text)
  , _ciFieldDelimiter       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CSVInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciQuoteCharacter' - Value used for escaping where the field delimiter is part of the value.
--
-- * 'ciRecordDelimiter' - Value used to separate individual records.
--
-- * 'ciFileHeaderInfo' - Describes the first line of input. Valid values: None, Ignore, Use.
--
-- * 'ciQuoteEscapeCharacter' - Single character used for escaping the quote character inside an already escaped value.
--
-- * 'ciComments' - Single character used to indicate a row should be ignored when present at the start of a row.
--
-- * 'ciFieldDelimiter' - Value used to separate individual fields in a record.
csvInput
    :: CSVInput
csvInput =
  CSVInput'
    { _ciQuoteCharacter = Nothing
    , _ciRecordDelimiter = Nothing
    , _ciFileHeaderInfo = Nothing
    , _ciQuoteEscapeCharacter = Nothing
    , _ciComments = Nothing
    , _ciFieldDelimiter = Nothing
    }


-- | Value used for escaping where the field delimiter is part of the value.
ciQuoteCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteCharacter = lens _ciQuoteCharacter (\ s a -> s{_ciQuoteCharacter = a})

-- | Value used to separate individual records.
ciRecordDelimiter :: Lens' CSVInput (Maybe Text)
ciRecordDelimiter = lens _ciRecordDelimiter (\ s a -> s{_ciRecordDelimiter = a})

-- | Describes the first line of input. Valid values: None, Ignore, Use.
ciFileHeaderInfo :: Lens' CSVInput (Maybe FileHeaderInfo)
ciFileHeaderInfo = lens _ciFileHeaderInfo (\ s a -> s{_ciFileHeaderInfo = a})

-- | Single character used for escaping the quote character inside an already escaped value.
ciQuoteEscapeCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteEscapeCharacter = lens _ciQuoteEscapeCharacter (\ s a -> s{_ciQuoteEscapeCharacter = a})

-- | Single character used to indicate a row should be ignored when present at the start of a row.
ciComments :: Lens' CSVInput (Maybe Text)
ciComments = lens _ciComments (\ s a -> s{_ciComments = a})

-- | Value used to separate individual fields in a record.
ciFieldDelimiter :: Lens' CSVInput (Maybe Text)
ciFieldDelimiter = lens _ciFieldDelimiter (\ s a -> s{_ciFieldDelimiter = a})

instance Hashable CSVInput where

instance NFData CSVInput where

instance ToXML CSVInput where
        toXML CSVInput'{..}
          = mconcat
              ["QuoteCharacter" @= _ciQuoteCharacter,
               "RecordDelimiter" @= _ciRecordDelimiter,
               "FileHeaderInfo" @= _ciFileHeaderInfo,
               "QuoteEscapeCharacter" @= _ciQuoteEscapeCharacter,
               "Comments" @= _ciComments,
               "FieldDelimiter" @= _ciFieldDelimiter]

-- | Describes how CSV-formatted results are formatted.
--
-- /See:/ 'csvOutput' smart constructor.
data CSVOutput = CSVOutput'
  { _coQuoteCharacter       :: !(Maybe Text)
  , _coQuoteFields          :: !(Maybe QuoteFields)
  , _coRecordDelimiter      :: !(Maybe Text)
  , _coQuoteEscapeCharacter :: !(Maybe Text)
  , _coFieldDelimiter       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CSVOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coQuoteCharacter' - Value used for escaping where the field delimiter is part of the value.
--
-- * 'coQuoteFields' - Indicates whether or not all output fields should be quoted.
--
-- * 'coRecordDelimiter' - Value used to separate individual records.
--
-- * 'coQuoteEscapeCharacter' - Single character used for escaping the quote character inside an already escaped value.
--
-- * 'coFieldDelimiter' - Value used to separate individual fields in a record.
csvOutput
    :: CSVOutput
csvOutput =
  CSVOutput'
    { _coQuoteCharacter = Nothing
    , _coQuoteFields = Nothing
    , _coRecordDelimiter = Nothing
    , _coQuoteEscapeCharacter = Nothing
    , _coFieldDelimiter = Nothing
    }


-- | Value used for escaping where the field delimiter is part of the value.
coQuoteCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteCharacter = lens _coQuoteCharacter (\ s a -> s{_coQuoteCharacter = a})

-- | Indicates whether or not all output fields should be quoted.
coQuoteFields :: Lens' CSVOutput (Maybe QuoteFields)
coQuoteFields = lens _coQuoteFields (\ s a -> s{_coQuoteFields = a})

-- | Value used to separate individual records.
coRecordDelimiter :: Lens' CSVOutput (Maybe Text)
coRecordDelimiter = lens _coRecordDelimiter (\ s a -> s{_coRecordDelimiter = a})

-- | Single character used for escaping the quote character inside an already escaped value.
coQuoteEscapeCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteEscapeCharacter = lens _coQuoteEscapeCharacter (\ s a -> s{_coQuoteEscapeCharacter = a})

-- | Value used to separate individual fields in a record.
coFieldDelimiter :: Lens' CSVOutput (Maybe Text)
coFieldDelimiter = lens _coFieldDelimiter (\ s a -> s{_coFieldDelimiter = a})

instance Hashable CSVOutput where

instance NFData CSVOutput where

instance ToXML CSVOutput where
        toXML CSVOutput'{..}
          = mconcat
              ["QuoteCharacter" @= _coQuoteCharacter,
               "QuoteFields" @= _coQuoteFields,
               "RecordDelimiter" @= _coRecordDelimiter,
               "QuoteEscapeCharacter" @= _coQuoteEscapeCharacter,
               "FieldDelimiter" @= _coFieldDelimiter]

-- | /See:/ 'commonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix'
  { _cpPrefix :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommonPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPrefix' - Undocumented member.
commonPrefix
    :: CommonPrefix
commonPrefix = CommonPrefix' {_cpPrefix = Nothing}


-- | Undocumented member.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\ s a -> s{_cpPrefix = a})

instance FromXML CommonPrefix where
        parseXML x = CommonPrefix' <$> (x .@? "Prefix")

instance Hashable CommonPrefix where

instance NFData CommonPrefix where

-- | /See:/ 'completedMultipartUpload' smart constructor.
newtype CompletedMultipartUpload = CompletedMultipartUpload'
  { _cmuParts :: Maybe (List1 CompletedPart)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompletedMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuParts' - Undocumented member.
completedMultipartUpload
    :: CompletedMultipartUpload
completedMultipartUpload = CompletedMultipartUpload' {_cmuParts = Nothing}


-- | Undocumented member.
cmuParts :: Lens' CompletedMultipartUpload (Maybe (NonEmpty CompletedPart))
cmuParts = lens _cmuParts (\ s a -> s{_cmuParts = a}) . mapping _List1

instance Hashable CompletedMultipartUpload where

instance NFData CompletedMultipartUpload where

instance ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload'{..}
          = mconcat [toXML (toXMLList "Part" <$> _cmuParts)]

-- | /See:/ 'completedPart' smart constructor.
data CompletedPart = CompletedPart'
  { _cpPartNumber :: !Int
  , _cpETag       :: !ETag
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CompletedPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPartNumber' - Part number that identifies the part. This is a positive integer between 1 and 10,000.
--
-- * 'cpETag' - Entity tag returned when the part was uploaded.
completedPart
    :: Int -- ^ 'cpPartNumber'
    -> ETag -- ^ 'cpETag'
    -> CompletedPart
completedPart pPartNumber_ pETag_ =
  CompletedPart' {_cpPartNumber = pPartNumber_, _cpETag = pETag_}


-- | Part number that identifies the part. This is a positive integer between 1 and 10,000.
cpPartNumber :: Lens' CompletedPart Int
cpPartNumber = lens _cpPartNumber (\ s a -> s{_cpPartNumber = a})

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart ETag
cpETag = lens _cpETag (\ s a -> s{_cpETag = a})

instance Hashable CompletedPart where

instance NFData CompletedPart where

instance ToXML CompletedPart where
        toXML CompletedPart'{..}
          = mconcat
              ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]

-- | /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cKeyPrefixEquals             :: !(Maybe Text)
  , _cHTTPErrorCodeReturnedEquals :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cKeyPrefixEquals' - The object key name prefix when the redirect is applied. For example, to redirect requests for ExamplePage.html, the key prefix will be ExamplePage.html. To redirect request for all pages with the prefix docs/, the key prefix will be /docs, which identifies all objects in the docs/ folder. Required when the parent element Condition is specified and sibling HttpErrorCodeReturnedEquals is not specified. If both conditions are specified, both must be true for the redirect to be applied.
--
-- * 'cHTTPErrorCodeReturnedEquals' - The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element Condition is specified and sibling KeyPrefixEquals is not specified. If both are specified, then both must be true for the redirect to be applied.
condition
    :: Condition
condition =
  Condition'
    {_cKeyPrefixEquals = Nothing, _cHTTPErrorCodeReturnedEquals = Nothing}


-- | The object key name prefix when the redirect is applied. For example, to redirect requests for ExamplePage.html, the key prefix will be ExamplePage.html. To redirect request for all pages with the prefix docs/, the key prefix will be /docs, which identifies all objects in the docs/ folder. Required when the parent element Condition is specified and sibling HttpErrorCodeReturnedEquals is not specified. If both conditions are specified, both must be true for the redirect to be applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals = lens _cKeyPrefixEquals (\ s a -> s{_cKeyPrefixEquals = a})

-- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element Condition is specified and sibling KeyPrefixEquals is not specified. If both are specified, then both must be true for the redirect to be applied.
cHTTPErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHTTPErrorCodeReturnedEquals = lens _cHTTPErrorCodeReturnedEquals (\ s a -> s{_cHTTPErrorCodeReturnedEquals = a})

instance FromXML Condition where
        parseXML x
          = Condition' <$>
              (x .@? "KeyPrefixEquals") <*>
                (x .@? "HttpErrorCodeReturnedEquals")

instance Hashable Condition where

instance NFData Condition where

instance ToXML Condition where
        toXML Condition'{..}
          = mconcat
              ["KeyPrefixEquals" @= _cKeyPrefixEquals,
               "HttpErrorCodeReturnedEquals" @=
                 _cHTTPErrorCodeReturnedEquals]

-- | /See:/ 'continuationEvent' smart constructor.
data ContinuationEvent =
  ContinuationEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinuationEvent' with the minimum fields required to make a request.
--
continuationEvent
    :: ContinuationEvent
continuationEvent = ContinuationEvent'


instance FromXML ContinuationEvent where
        parseXML = const (pure ContinuationEvent')

instance Hashable ContinuationEvent where

instance NFData ContinuationEvent where

-- | /See:/ 'copyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { _corETag         :: !(Maybe ETag)
  , _corLastModified :: !(Maybe RFC822)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyObjectResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corETag' - Undocumented member.
--
-- * 'corLastModified' - Undocumented member.
copyObjectResult
    :: CopyObjectResult
copyObjectResult =
  CopyObjectResult' {_corETag = Nothing, _corLastModified = Nothing}


-- | Undocumented member.
corETag :: Lens' CopyObjectResult (Maybe ETag)
corETag = lens _corETag (\ s a -> s{_corETag = a})

-- | Undocumented member.
corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\ s a -> s{_corLastModified = a}) . mapping _Time

instance FromXML CopyObjectResult where
        parseXML x
          = CopyObjectResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyObjectResult where

instance NFData CopyObjectResult where

-- | /See:/ 'copyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { _cprETag         :: !(Maybe ETag)
  , _cprLastModified :: !(Maybe RFC822)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyPartResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprETag' - Entity tag of the object.
--
-- * 'cprLastModified' - Date and time at which the object was uploaded.
copyPartResult
    :: CopyPartResult
copyPartResult =
  CopyPartResult' {_cprETag = Nothing, _cprLastModified = Nothing}


-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\ s a -> s{_cprETag = a})

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\ s a -> s{_cprLastModified = a}) . mapping _Time

instance FromXML CopyPartResult where
        parseXML x
          = CopyPartResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

instance Hashable CopyPartResult where

instance NFData CopyPartResult where

-- | /See:/ 'createBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
  { _cbcLocationConstraint :: Maybe LocationConstraint
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBucketConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbcLocationConstraint' - Specifies the region where the bucket will be created. If you don't specify a region, the bucket will be created in US Standard.
createBucketConfiguration
    :: CreateBucketConfiguration
createBucketConfiguration =
  CreateBucketConfiguration' {_cbcLocationConstraint = Nothing}


-- | Specifies the region where the bucket will be created. If you don't specify a region, the bucket will be created in US Standard.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe LocationConstraint)
cbcLocationConstraint = lens _cbcLocationConstraint (\ s a -> s{_cbcLocationConstraint = a})

instance Hashable CreateBucketConfiguration where

instance NFData CreateBucketConfiguration where

instance ToXML CreateBucketConfiguration where
        toXML CreateBucketConfiguration'{..}
          = mconcat
              ["LocationConstraint" @= _cbcLocationConstraint]

-- | /See:/ 'delete'' smart constructor.
data Delete = Delete'
  { _dQuiet   :: !(Maybe Bool)
  , _dObjects :: ![ObjectIdentifier]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dQuiet' - Element to enable quiet mode for the request. When you add this element, you must set its value to true.
--
-- * 'dObjects' - Undocumented member.
delete'
    :: Delete
delete' = Delete' {_dQuiet = Nothing, _dObjects = mempty}


-- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\ s a -> s{_dQuiet = a})

-- | Undocumented member.
dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\ s a -> s{_dObjects = a}) . _Coerce

instance Hashable Delete where

instance NFData Delete where

instance ToXML Delete where
        toXML Delete'{..}
          = mconcat
              ["Quiet" @= _dQuiet, toXMLList "Object" _dObjects]

-- | /See:/ 'deleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { _dmeVersionId    :: !(Maybe ObjectVersionId)
  , _dmeIsLatest     :: !(Maybe Bool)
  , _dmeOwner        :: !(Maybe Owner)
  , _dmeKey          :: !(Maybe ObjectKey)
  , _dmeLastModified :: !(Maybe RFC822)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMarkerEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmeVersionId' - Version ID of an object.
--
-- * 'dmeIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'dmeOwner' - Undocumented member.
--
-- * 'dmeKey' - The object key.
--
-- * 'dmeLastModified' - Date and time the object was last modified.
deleteMarkerEntry
    :: DeleteMarkerEntry
deleteMarkerEntry =
  DeleteMarkerEntry'
    { _dmeVersionId = Nothing
    , _dmeIsLatest = Nothing
    , _dmeOwner = Nothing
    , _dmeKey = Nothing
    , _dmeLastModified = Nothing
    }


-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\ s a -> s{_dmeVersionId = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\ s a -> s{_dmeIsLatest = a})

-- | Undocumented member.
dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\ s a -> s{_dmeOwner = a})

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\ s a -> s{_dmeKey = a})

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\ s a -> s{_dmeLastModified = a}) . mapping _Time

instance FromXML DeleteMarkerEntry where
        parseXML x
          = DeleteMarkerEntry' <$>
              (x .@? "VersionId") <*> (x .@? "IsLatest") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "LastModified")

instance Hashable DeleteMarkerEntry where

instance NFData DeleteMarkerEntry where

-- | /See:/ 'deletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { _dVersionId             :: !(Maybe ObjectVersionId)
  , _dDeleteMarker          :: !(Maybe Bool)
  , _dDeleteMarkerVersionId :: !(Maybe Text)
  , _dKey                   :: !(Maybe ObjectKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletedObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionId' - Undocumented member.
--
-- * 'dDeleteMarker' - Undocumented member.
--
-- * 'dDeleteMarkerVersionId' - Undocumented member.
--
-- * 'dKey' - Undocumented member.
deletedObject
    :: DeletedObject
deletedObject =
  DeletedObject'
    { _dVersionId = Nothing
    , _dDeleteMarker = Nothing
    , _dDeleteMarkerVersionId = Nothing
    , _dKey = Nothing
    }


-- | Undocumented member.
dVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dVersionId = lens _dVersionId (\ s a -> s{_dVersionId = a})

-- | Undocumented member.
dDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dDeleteMarker = lens _dDeleteMarker (\ s a -> s{_dDeleteMarker = a})

-- | Undocumented member.
dDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dDeleteMarkerVersionId = lens _dDeleteMarkerVersionId (\ s a -> s{_dDeleteMarkerVersionId = a})

-- | Undocumented member.
dKey :: Lens' DeletedObject (Maybe ObjectKey)
dKey = lens _dKey (\ s a -> s{_dKey = a})

instance FromXML DeletedObject where
        parseXML x
          = DeletedObject' <$>
              (x .@? "VersionId") <*> (x .@? "DeleteMarker") <*>
                (x .@? "DeleteMarkerVersionId")
                <*> (x .@? "Key")

instance Hashable DeletedObject where

instance NFData DeletedObject where

-- | Container for replication destination information.
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dAccessControlTranslation :: !(Maybe AccessControlTranslation)
  , _dAccount                  :: !(Maybe Text)
  , _dStorageClass             :: !(Maybe StorageClass)
  , _dEncryptionConfiguration  :: !(Maybe EncryptionConfiguration)
  , _dBucket                   :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAccessControlTranslation' - Container for information regarding the access control for replicas.
--
-- * 'dAccount' - Account ID of the destination bucket. Currently this is only being verified if Access Control Translation is enabled
--
-- * 'dStorageClass' - The class of storage used to store the object.
--
-- * 'dEncryptionConfiguration' - Container for information regarding encryption based configuration for replicas.
--
-- * 'dBucket' - Amazon resource name (ARN) of the bucket where you want Amazon S3 to store replicas of the object identified by the rule.
destination
    :: BucketName -- ^ 'dBucket'
    -> Destination
destination pBucket_ =
  Destination'
    { _dAccessControlTranslation = Nothing
    , _dAccount = Nothing
    , _dStorageClass = Nothing
    , _dEncryptionConfiguration = Nothing
    , _dBucket = pBucket_
    }


-- | Container for information regarding the access control for replicas.
dAccessControlTranslation :: Lens' Destination (Maybe AccessControlTranslation)
dAccessControlTranslation = lens _dAccessControlTranslation (\ s a -> s{_dAccessControlTranslation = a})

-- | Account ID of the destination bucket. Currently this is only being verified if Access Control Translation is enabled
dAccount :: Lens' Destination (Maybe Text)
dAccount = lens _dAccount (\ s a -> s{_dAccount = a})

-- | The class of storage used to store the object.
dStorageClass :: Lens' Destination (Maybe StorageClass)
dStorageClass = lens _dStorageClass (\ s a -> s{_dStorageClass = a})

-- | Container for information regarding encryption based configuration for replicas.
dEncryptionConfiguration :: Lens' Destination (Maybe EncryptionConfiguration)
dEncryptionConfiguration = lens _dEncryptionConfiguration (\ s a -> s{_dEncryptionConfiguration = a})

-- | Amazon resource name (ARN) of the bucket where you want Amazon S3 to store replicas of the object identified by the rule.
dBucket :: Lens' Destination BucketName
dBucket = lens _dBucket (\ s a -> s{_dBucket = a})

instance FromXML Destination where
        parseXML x
          = Destination' <$>
              (x .@? "AccessControlTranslation") <*>
                (x .@? "Account")
                <*> (x .@? "StorageClass")
                <*> (x .@? "EncryptionConfiguration")
                <*> (x .@ "Bucket")

instance Hashable Destination where

instance NFData Destination where

instance ToXML Destination where
        toXML Destination'{..}
          = mconcat
              ["AccessControlTranslation" @=
                 _dAccessControlTranslation,
               "Account" @= _dAccount,
               "StorageClass" @= _dStorageClass,
               "EncryptionConfiguration" @=
                 _dEncryptionConfiguration,
               "Bucket" @= _dBucket]

-- | Describes the server-side encryption that will be applied to the restore results.
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eKMSKeyId       :: !(Maybe (Sensitive Text))
  , _eKMSContext     :: !(Maybe Text)
  , _eEncryptionType :: !ServerSideEncryption
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eKMSKeyId' - If the encryption type is aws:kms, this optional value specifies the AWS KMS key ID to use for encryption of job results.
--
-- * 'eKMSContext' - If the encryption type is aws:kms, this optional value can be used to specify the encryption context for the restore results.
--
-- * 'eEncryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3 (e.g., AES256, aws:kms).
encryption
    :: ServerSideEncryption -- ^ 'eEncryptionType'
    -> Encryption
encryption pEncryptionType_ =
  Encryption'
    { _eKMSKeyId = Nothing
    , _eKMSContext = Nothing
    , _eEncryptionType = pEncryptionType_
    }


-- | If the encryption type is aws:kms, this optional value specifies the AWS KMS key ID to use for encryption of job results.
eKMSKeyId :: Lens' Encryption (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a}) . mapping _Sensitive

-- | If the encryption type is aws:kms, this optional value can be used to specify the encryption context for the restore results.
eKMSContext :: Lens' Encryption (Maybe Text)
eKMSContext = lens _eKMSContext (\ s a -> s{_eKMSContext = a})

-- | The server-side encryption algorithm used when storing job results in Amazon S3 (e.g., AES256, aws:kms).
eEncryptionType :: Lens' Encryption ServerSideEncryption
eEncryptionType = lens _eEncryptionType (\ s a -> s{_eEncryptionType = a})

instance Hashable Encryption where

instance NFData Encryption where

instance ToXML Encryption where
        toXML Encryption'{..}
          = mconcat
              ["KMSKeyId" @= _eKMSKeyId,
               "KMSContext" @= _eKMSContext,
               "EncryptionType" @= _eEncryptionType]

-- | Container for information regarding encryption based configuration for replicas.
--
-- /See:/ 'encryptionConfiguration' smart constructor.
newtype EncryptionConfiguration = EncryptionConfiguration'
  { _ecReplicaKMSKeyId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecReplicaKMSKeyId' - The id of the KMS key used to encrypt the replica object.
encryptionConfiguration
    :: EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration' {_ecReplicaKMSKeyId = Nothing}


-- | The id of the KMS key used to encrypt the replica object.
ecReplicaKMSKeyId :: Lens' EncryptionConfiguration (Maybe Text)
ecReplicaKMSKeyId = lens _ecReplicaKMSKeyId (\ s a -> s{_ecReplicaKMSKeyId = a})

instance FromXML EncryptionConfiguration where
        parseXML x
          = EncryptionConfiguration' <$>
              (x .@? "ReplicaKmsKeyID")

instance Hashable EncryptionConfiguration where

instance NFData EncryptionConfiguration where

instance ToXML EncryptionConfiguration where
        toXML EncryptionConfiguration'{..}
          = mconcat ["ReplicaKmsKeyID" @= _ecReplicaKMSKeyId]

-- | /See:/ 'endEvent' smart constructor.
data EndEvent =
  EndEvent'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndEvent' with the minimum fields required to make a request.
--
endEvent
    :: EndEvent
endEvent = EndEvent'


instance FromXML EndEvent where
        parseXML = const (pure EndEvent')

instance Hashable EndEvent where

instance NFData EndEvent where

-- | /See:/ 'errorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument'
  { _edKey :: ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edKey' - The object key name to use when a 4XX class error occurs.
errorDocument
    :: ObjectKey -- ^ 'edKey'
    -> ErrorDocument
errorDocument pKey_ = ErrorDocument' {_edKey = pKey_}


-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\ s a -> s{_edKey = a})

instance FromXML ErrorDocument where
        parseXML x = ErrorDocument' <$> (x .@ "Key")

instance Hashable ErrorDocument where

instance NFData ErrorDocument where

instance ToXML ErrorDocument where
        toXML ErrorDocument'{..} = mconcat ["Key" @= _edKey]

-- | Container for key value pair that defines the criteria for the filter rule.
--
-- /See:/ 'filterRule' smart constructor.
data FilterRule = FilterRule'
  { _frValue :: !(Maybe Text)
  , _frName  :: !(Maybe FilterRuleName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frValue' - Undocumented member.
--
-- * 'frName' - <http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
filterRule
    :: FilterRule
filterRule = FilterRule' {_frValue = Nothing, _frName = Nothing}


-- | Undocumented member.
frValue :: Lens' FilterRule (Maybe Text)
frValue = lens _frValue (\ s a -> s{_frValue = a})

-- | <http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
frName :: Lens' FilterRule (Maybe FilterRuleName)
frName = lens _frName (\ s a -> s{_frName = a})

instance FromXML FilterRule where
        parseXML x
          = FilterRule' <$> (x .@? "Value") <*> (x .@? "Name")

instance Hashable FilterRule where

instance NFData FilterRule where

instance ToXML FilterRule where
        toXML FilterRule'{..}
          = mconcat ["Value" @= _frValue, "Name" @= _frName]

-- | /See:/ 'glacierJobParameters' smart constructor.
newtype GlacierJobParameters = GlacierJobParameters'
  { _gjpTier :: Tier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlacierJobParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjpTier' - Glacier retrieval tier at which the restore will be processed.
glacierJobParameters
    :: Tier -- ^ 'gjpTier'
    -> GlacierJobParameters
glacierJobParameters pTier_ = GlacierJobParameters' {_gjpTier = pTier_}


-- | Glacier retrieval tier at which the restore will be processed.
gjpTier :: Lens' GlacierJobParameters Tier
gjpTier = lens _gjpTier (\ s a -> s{_gjpTier = a})

instance Hashable GlacierJobParameters where

instance NFData GlacierJobParameters where

instance ToXML GlacierJobParameters where
        toXML GlacierJobParameters'{..}
          = mconcat ["Tier" @= _gjpTier]

-- | /See:/ 'grant' smart constructor.
data Grant = Grant'
  { _gPermission :: !(Maybe Permission)
  , _gGrantee    :: !(Maybe Grantee)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPermission' - Specifies the permission given to the grantee.
--
-- * 'gGrantee' - Undocumented member.
grant
    :: Grant
grant = Grant' {_gPermission = Nothing, _gGrantee = Nothing}


-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\ s a -> s{_gPermission = a})

-- | Undocumented member.
gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\ s a -> s{_gGrantee = a})

instance FromXML Grant where
        parseXML x
          = Grant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance Hashable Grant where

instance NFData Grant where

instance ToXML Grant where
        toXML Grant'{..}
          = mconcat
              ["Permission" @= _gPermission,
               "Grantee" @= _gGrantee]

-- | /See:/ 'grantee' smart constructor.
data Grantee = Grantee'
  { _gURI          :: !(Maybe Text)
  , _gEmailAddress :: !(Maybe Text)
  , _gDisplayName  :: !(Maybe Text)
  , _gId           :: !(Maybe Text)
  , _gType         :: !Type
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Grantee' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gURI' - URI of the grantee group.
--
-- * 'gEmailAddress' - Email address of the grantee.
--
-- * 'gDisplayName' - Screen name of the grantee.
--
-- * 'gId' - The canonical user ID of the grantee.
--
-- * 'gType' - Type of grantee
grantee
    :: Type -- ^ 'gType'
    -> Grantee
grantee pType_ =
  Grantee'
    { _gURI = Nothing
    , _gEmailAddress = Nothing
    , _gDisplayName = Nothing
    , _gId = Nothing
    , _gType = pType_
    }


-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\ s a -> s{_gURI = a})

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\ s a -> s{_gEmailAddress = a})

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\ s a -> s{_gDisplayName = a})

-- | The canonical user ID of the grantee.
gId :: Lens' Grantee (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a})

-- | Type of grantee
gType :: Lens' Grantee Type
gType = lens _gType (\ s a -> s{_gType = a})

instance FromXML Grantee where
        parseXML x
          = Grantee' <$>
              (x .@? "URI") <*> (x .@? "EmailAddress") <*>
                (x .@? "DisplayName")
                <*> (x .@? "ID")
                <*> (x .@ "xsi:type")

instance Hashable Grantee where

instance NFData Grantee where

instance ToXML Grantee where
        toXML Grantee'{..}
          = mconcat
              ["URI" @= _gURI, "EmailAddress" @= _gEmailAddress,
               "DisplayName" @= _gDisplayName, "ID" @= _gId,
               "xsi:type" @@= _gType]

-- | /See:/ 'indexDocument' smart constructor.
newtype IndexDocument = IndexDocument'
  { _idSuffix :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSuffix' - A suffix that is appended to a request that is for a directory on the website endpoint (e.g. if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
indexDocument
    :: Text -- ^ 'idSuffix'
    -> IndexDocument
indexDocument pSuffix_ = IndexDocument' {_idSuffix = pSuffix_}


-- | A suffix that is appended to a request that is for a directory on the website endpoint (e.g. if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\ s a -> s{_idSuffix = a})

instance FromXML IndexDocument where
        parseXML x = IndexDocument' <$> (x .@ "Suffix")

instance Hashable IndexDocument where

instance NFData IndexDocument where

instance ToXML IndexDocument where
        toXML IndexDocument'{..}
          = mconcat ["Suffix" @= _idSuffix]

-- | /See:/ 'initiator' smart constructor.
data Initiator = Initiator'
  { _iDisplayName :: !(Maybe Text)
  , _iId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Initiator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iDisplayName' - Name of the Principal.
--
-- * 'iId' - If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
initiator
    :: Initiator
initiator = Initiator' {_iDisplayName = Nothing, _iId = Nothing}


-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a})

-- | If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
iId :: Lens' Initiator (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a})

instance FromXML Initiator where
        parseXML x
          = Initiator' <$>
              (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Initiator where

instance NFData Initiator where

-- | Describes the serialization format of the object.
--
-- /See:/ 'inputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { _isJSON            :: !(Maybe JSONInput)
  , _isCSV             :: !(Maybe CSVInput)
  , _isCompressionType :: !(Maybe CompressionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isJSON' - Specifies JSON as object's input serialization format.
--
-- * 'isCSV' - Describes the serialization of a CSV-encoded object.
--
-- * 'isCompressionType' - Specifies object's compression format. Valid values: NONE, GZIP. Default Value: NONE.
inputSerialization
    :: InputSerialization
inputSerialization =
  InputSerialization'
    {_isJSON = Nothing, _isCSV = Nothing, _isCompressionType = Nothing}


-- | Specifies JSON as object's input serialization format.
isJSON :: Lens' InputSerialization (Maybe JSONInput)
isJSON = lens _isJSON (\ s a -> s{_isJSON = a})

-- | Describes the serialization of a CSV-encoded object.
isCSV :: Lens' InputSerialization (Maybe CSVInput)
isCSV = lens _isCSV (\ s a -> s{_isCSV = a})

-- | Specifies object's compression format. Valid values: NONE, GZIP. Default Value: NONE.
isCompressionType :: Lens' InputSerialization (Maybe CompressionType)
isCompressionType = lens _isCompressionType (\ s a -> s{_isCompressionType = a})

instance Hashable InputSerialization where

instance NFData InputSerialization where

instance ToXML InputSerialization where
        toXML InputSerialization'{..}
          = mconcat
              ["JSON" @= _isJSON, "CSV" @= _isCSV,
               "CompressionType" @= _isCompressionType]

-- | /See:/ 'inventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { _icOptionalFields         :: !(Maybe [InventoryOptionalField])
  , _icFilter                 :: !(Maybe InventoryFilter)
  , _icDestination            :: !InventoryDestination
  , _icIsEnabled              :: !Bool
  , _icId                     :: !Text
  , _icIncludedObjectVersions :: !InventoryIncludedObjectVersions
  , _icSchedule               :: !InventorySchedule
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icOptionalFields' - Contains the optional fields that are included in the inventory results.
--
-- * 'icFilter' - Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- * 'icDestination' - Contains information about where to publish the inventory results.
--
-- * 'icIsEnabled' - Specifies whether the inventory is enabled or disabled.
--
-- * 'icId' - The ID used to identify the inventory configuration.
--
-- * 'icIncludedObjectVersions' - Specifies which object version(s) to included in the inventory results.
--
-- * 'icSchedule' - Specifies the schedule for generating inventory results.
inventoryConfiguration
    :: InventoryDestination -- ^ 'icDestination'
    -> Bool -- ^ 'icIsEnabled'
    -> Text -- ^ 'icId'
    -> InventoryIncludedObjectVersions -- ^ 'icIncludedObjectVersions'
    -> InventorySchedule -- ^ 'icSchedule'
    -> InventoryConfiguration
inventoryConfiguration pDestination_ pIsEnabled_ pId_ pIncludedObjectVersions_ pSchedule_ =
  InventoryConfiguration'
    { _icOptionalFields = Nothing
    , _icFilter = Nothing
    , _icDestination = pDestination_
    , _icIsEnabled = pIsEnabled_
    , _icId = pId_
    , _icIncludedObjectVersions = pIncludedObjectVersions_
    , _icSchedule = pSchedule_
    }


-- | Contains the optional fields that are included in the inventory results.
icOptionalFields :: Lens' InventoryConfiguration [InventoryOptionalField]
icOptionalFields = lens _icOptionalFields (\ s a -> s{_icOptionalFields = a}) . _Default . _Coerce

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
icFilter :: Lens' InventoryConfiguration (Maybe InventoryFilter)
icFilter = lens _icFilter (\ s a -> s{_icFilter = a})

-- | Contains information about where to publish the inventory results.
icDestination :: Lens' InventoryConfiguration InventoryDestination
icDestination = lens _icDestination (\ s a -> s{_icDestination = a})

-- | Specifies whether the inventory is enabled or disabled.
icIsEnabled :: Lens' InventoryConfiguration Bool
icIsEnabled = lens _icIsEnabled (\ s a -> s{_icIsEnabled = a})

-- | The ID used to identify the inventory configuration.
icId :: Lens' InventoryConfiguration Text
icId = lens _icId (\ s a -> s{_icId = a})

-- | Specifies which object version(s) to included in the inventory results.
icIncludedObjectVersions :: Lens' InventoryConfiguration InventoryIncludedObjectVersions
icIncludedObjectVersions = lens _icIncludedObjectVersions (\ s a -> s{_icIncludedObjectVersions = a})

-- | Specifies the schedule for generating inventory results.
icSchedule :: Lens' InventoryConfiguration InventorySchedule
icSchedule = lens _icSchedule (\ s a -> s{_icSchedule = a})

instance FromXML InventoryConfiguration where
        parseXML x
          = InventoryConfiguration' <$>
              (x .@? "OptionalFields" .!@ mempty >>=
                 may (parseXMLList "Field"))
                <*> (x .@? "Filter")
                <*> (x .@ "Destination")
                <*> (x .@ "IsEnabled")
                <*> (x .@ "Id")
                <*> (x .@ "IncludedObjectVersions")
                <*> (x .@ "Schedule")

instance Hashable InventoryConfiguration where

instance NFData InventoryConfiguration where

instance ToXML InventoryConfiguration where
        toXML InventoryConfiguration'{..}
          = mconcat
              ["OptionalFields" @=
                 toXML (toXMLList "Field" <$> _icOptionalFields),
               "Filter" @= _icFilter,
               "Destination" @= _icDestination,
               "IsEnabled" @= _icIsEnabled, "Id" @= _icId,
               "IncludedObjectVersions" @=
                 _icIncludedObjectVersions,
               "Schedule" @= _icSchedule]

-- | /See:/ 'inventoryDestination' smart constructor.
newtype InventoryDestination = InventoryDestination'
  { _idS3BucketDestination :: InventoryS3BucketDestination
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idS3BucketDestination' - Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
inventoryDestination
    :: InventoryS3BucketDestination -- ^ 'idS3BucketDestination'
    -> InventoryDestination
inventoryDestination pS3BucketDestination_ =
  InventoryDestination' {_idS3BucketDestination = pS3BucketDestination_}


-- | Contains the bucket name, file format, bucket owner (optional), and prefix (optional) where inventory results are published.
idS3BucketDestination :: Lens' InventoryDestination InventoryS3BucketDestination
idS3BucketDestination = lens _idS3BucketDestination (\ s a -> s{_idS3BucketDestination = a})

instance FromXML InventoryDestination where
        parseXML x
          = InventoryDestination' <$>
              (x .@ "S3BucketDestination")

instance Hashable InventoryDestination where

instance NFData InventoryDestination where

instance ToXML InventoryDestination where
        toXML InventoryDestination'{..}
          = mconcat
              ["S3BucketDestination" @= _idS3BucketDestination]

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
-- /See:/ 'inventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { _ieSSES3  :: !(Maybe SSES3)
  , _ieSSEKMS :: !(Maybe SSEKMS)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieSSES3' - Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
--
-- * 'ieSSEKMS' - Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
inventoryEncryption
    :: InventoryEncryption
inventoryEncryption =
  InventoryEncryption' {_ieSSES3 = Nothing, _ieSSEKMS = Nothing}


-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
ieSSES3 :: Lens' InventoryEncryption (Maybe SSES3)
ieSSES3 = lens _ieSSES3 (\ s a -> s{_ieSSES3 = a})

-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
ieSSEKMS :: Lens' InventoryEncryption (Maybe SSEKMS)
ieSSEKMS = lens _ieSSEKMS (\ s a -> s{_ieSSEKMS = a})

instance FromXML InventoryEncryption where
        parseXML x
          = InventoryEncryption' <$>
              (x .@? "SSE-S3") <*> (x .@? "SSE-KMS")

instance Hashable InventoryEncryption where

instance NFData InventoryEncryption where

instance ToXML InventoryEncryption where
        toXML InventoryEncryption'{..}
          = mconcat
              ["SSE-S3" @= _ieSSES3, "SSE-KMS" @= _ieSSEKMS]

-- | /See:/ 'inventoryFilter' smart constructor.
newtype InventoryFilter = InventoryFilter'
  { _ifPrefix :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifPrefix' - The prefix that an object must have to be included in the inventory results.
inventoryFilter
    :: Text -- ^ 'ifPrefix'
    -> InventoryFilter
inventoryFilter pPrefix_ = InventoryFilter' {_ifPrefix = pPrefix_}


-- | The prefix that an object must have to be included in the inventory results.
ifPrefix :: Lens' InventoryFilter Text
ifPrefix = lens _ifPrefix (\ s a -> s{_ifPrefix = a})

instance FromXML InventoryFilter where
        parseXML x = InventoryFilter' <$> (x .@ "Prefix")

instance Hashable InventoryFilter where

instance NFData InventoryFilter where

instance ToXML InventoryFilter where
        toXML InventoryFilter'{..}
          = mconcat ["Prefix" @= _ifPrefix]

-- | /See:/ 'inventoryS3BucketDestination' smart constructor.
data InventoryS3BucketDestination = InventoryS3BucketDestination'
  { _isbdPrefix     :: !(Maybe Text)
  , _isbdAccountId  :: !(Maybe Text)
  , _isbdEncryption :: !(Maybe InventoryEncryption)
  , _isbdBucket     :: !BucketName
  , _isbdFormat     :: !InventoryFormat
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isbdPrefix' - The prefix that is prepended to all inventory results.
--
-- * 'isbdAccountId' - The ID of the account that owns the destination bucket.
--
-- * 'isbdEncryption' - Contains the type of server-side encryption used to encrypt the inventory results.
--
-- * 'isbdBucket' - The Amazon resource name (ARN) of the bucket where inventory results will be published.
--
-- * 'isbdFormat' - Specifies the output format of the inventory results.
inventoryS3BucketDestination
    :: BucketName -- ^ 'isbdBucket'
    -> InventoryFormat -- ^ 'isbdFormat'
    -> InventoryS3BucketDestination
inventoryS3BucketDestination pBucket_ pFormat_ =
  InventoryS3BucketDestination'
    { _isbdPrefix = Nothing
    , _isbdAccountId = Nothing
    , _isbdEncryption = Nothing
    , _isbdBucket = pBucket_
    , _isbdFormat = pFormat_
    }


-- | The prefix that is prepended to all inventory results.
isbdPrefix :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdPrefix = lens _isbdPrefix (\ s a -> s{_isbdPrefix = a})

-- | The ID of the account that owns the destination bucket.
isbdAccountId :: Lens' InventoryS3BucketDestination (Maybe Text)
isbdAccountId = lens _isbdAccountId (\ s a -> s{_isbdAccountId = a})

-- | Contains the type of server-side encryption used to encrypt the inventory results.
isbdEncryption :: Lens' InventoryS3BucketDestination (Maybe InventoryEncryption)
isbdEncryption = lens _isbdEncryption (\ s a -> s{_isbdEncryption = a})

-- | The Amazon resource name (ARN) of the bucket where inventory results will be published.
isbdBucket :: Lens' InventoryS3BucketDestination BucketName
isbdBucket = lens _isbdBucket (\ s a -> s{_isbdBucket = a})

-- | Specifies the output format of the inventory results.
isbdFormat :: Lens' InventoryS3BucketDestination InventoryFormat
isbdFormat = lens _isbdFormat (\ s a -> s{_isbdFormat = a})

instance FromXML InventoryS3BucketDestination where
        parseXML x
          = InventoryS3BucketDestination' <$>
              (x .@? "Prefix") <*> (x .@? "AccountId") <*>
                (x .@? "Encryption")
                <*> (x .@ "Bucket")
                <*> (x .@ "Format")

instance Hashable InventoryS3BucketDestination where

instance NFData InventoryS3BucketDestination where

instance ToXML InventoryS3BucketDestination where
        toXML InventoryS3BucketDestination'{..}
          = mconcat
              ["Prefix" @= _isbdPrefix,
               "AccountId" @= _isbdAccountId,
               "Encryption" @= _isbdEncryption,
               "Bucket" @= _isbdBucket, "Format" @= _isbdFormat]

-- | /See:/ 'inventorySchedule' smart constructor.
newtype InventorySchedule = InventorySchedule'
  { _isFrequency :: InventoryFrequency
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventorySchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isFrequency' - Specifies how frequently inventory results are produced.
inventorySchedule
    :: InventoryFrequency -- ^ 'isFrequency'
    -> InventorySchedule
inventorySchedule pFrequency_ = InventorySchedule' {_isFrequency = pFrequency_}


-- | Specifies how frequently inventory results are produced.
isFrequency :: Lens' InventorySchedule InventoryFrequency
isFrequency = lens _isFrequency (\ s a -> s{_isFrequency = a})

instance FromXML InventorySchedule where
        parseXML x
          = InventorySchedule' <$> (x .@ "Frequency")

instance Hashable InventorySchedule where

instance NFData InventorySchedule where

instance ToXML InventorySchedule where
        toXML InventorySchedule'{..}
          = mconcat ["Frequency" @= _isFrequency]

-- | /See:/ 'jsonInput' smart constructor.
newtype JSONInput = JSONInput'
  { _jiType :: Maybe JSONType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jiType' - The type of JSON. Valid values: Document, Lines.
jsonInput
    :: JSONInput
jsonInput = JSONInput' {_jiType = Nothing}


-- | The type of JSON. Valid values: Document, Lines.
jiType :: Lens' JSONInput (Maybe JSONType)
jiType = lens _jiType (\ s a -> s{_jiType = a})

instance Hashable JSONInput where

instance NFData JSONInput where

instance ToXML JSONInput where
        toXML JSONInput'{..} = mconcat ["Type" @= _jiType]

-- | /See:/ 'jsonOutput' smart constructor.
newtype JSONOutput = JSONOutput'
  { _joRecordDelimiter :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'joRecordDelimiter' - The value used to separate individual records in the output.
jsonOutput
    :: JSONOutput
jsonOutput = JSONOutput' {_joRecordDelimiter = Nothing}


-- | The value used to separate individual records in the output.
joRecordDelimiter :: Lens' JSONOutput (Maybe Text)
joRecordDelimiter = lens _joRecordDelimiter (\ s a -> s{_joRecordDelimiter = a})

instance Hashable JSONOutput where

instance NFData JSONOutput where

instance ToXML JSONOutput where
        toXML JSONOutput'{..}
          = mconcat ["RecordDelimiter" @= _joRecordDelimiter]

-- | Container for specifying the AWS Lambda notification configuration.
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { _lfcId                :: !(Maybe Text)
  , _lfcFilter            :: !(Maybe NotificationConfigurationFilter)
  , _lfcLambdaFunctionARN :: !Text
  , _lfcEvents            :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfcId' - Undocumented member.
--
-- * 'lfcFilter' - Undocumented member.
--
-- * 'lfcLambdaFunctionARN' - Lambda cloud function ARN that Amazon S3 can invoke when it detects events of the specified type.
--
-- * 'lfcEvents' - Undocumented member.
lambdaFunctionConfiguration
    :: Text -- ^ 'lfcLambdaFunctionARN'
    -> LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN_ =
  LambdaFunctionConfiguration'
    { _lfcId = Nothing
    , _lfcFilter = Nothing
    , _lfcLambdaFunctionARN = pLambdaFunctionARN_
    , _lfcEvents = mempty
    }


-- | Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\ s a -> s{_lfcId = a})

-- | Undocumented member.
lfcFilter :: Lens' LambdaFunctionConfiguration (Maybe NotificationConfigurationFilter)
lfcFilter = lens _lfcFilter (\ s a -> s{_lfcFilter = a})

-- | Lambda cloud function ARN that Amazon S3 can invoke when it detects events of the specified type.
lfcLambdaFunctionARN :: Lens' LambdaFunctionConfiguration Text
lfcLambdaFunctionARN = lens _lfcLambdaFunctionARN (\ s a -> s{_lfcLambdaFunctionARN = a})

-- | Undocumented member.
lfcEvents :: Lens' LambdaFunctionConfiguration [Event]
lfcEvents = lens _lfcEvents (\ s a -> s{_lfcEvents = a}) . _Coerce

instance FromXML LambdaFunctionConfiguration where
        parseXML x
          = LambdaFunctionConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*>
                (x .@ "CloudFunction")
                <*> (parseXMLList "Event" x)

instance Hashable LambdaFunctionConfiguration where

instance NFData LambdaFunctionConfiguration where

instance ToXML LambdaFunctionConfiguration where
        toXML LambdaFunctionConfiguration'{..}
          = mconcat
              ["Id" @= _lfcId, "Filter" @= _lfcFilter,
               "CloudFunction" @= _lfcLambdaFunctionARN,
               toXMLList "Event" _lfcEvents]

-- | /See:/ 'lifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
  { _leDays                      :: !(Maybe Int)
  , _leDate                      :: !(Maybe RFC822)
  , _leExpiredObjectDeleteMarker :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leDays' - Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
--
-- * 'leDate' - Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
--
-- * 'leExpiredObjectDeleteMarker' - Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
lifecycleExpiration
    :: LifecycleExpiration
lifecycleExpiration =
  LifecycleExpiration'
    { _leDays = Nothing
    , _leDate = Nothing
    , _leExpiredObjectDeleteMarker = Nothing
    }


-- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\ s a -> s{_leDays = a})

-- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\ s a -> s{_leDate = a}) . mapping _Time

-- | Indicates whether Amazon S3 will remove a delete marker with no noncurrent versions. If set to true, the delete marker will be expired; if set to false the policy takes no action. This cannot be specified with Days or Date in a Lifecycle Expiration Policy.
leExpiredObjectDeleteMarker :: Lens' LifecycleExpiration (Maybe Bool)
leExpiredObjectDeleteMarker = lens _leExpiredObjectDeleteMarker (\ s a -> s{_leExpiredObjectDeleteMarker = a})

instance FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "ExpiredObjectDeleteMarker")

instance Hashable LifecycleExpiration where

instance NFData LifecycleExpiration where

instance ToXML LifecycleExpiration where
        toXML LifecycleExpiration'{..}
          = mconcat
              ["Days" @= _leDays, "Date" @= _leDate,
               "ExpiredObjectDeleteMarker" @=
                 _leExpiredObjectDeleteMarker]

-- | /See:/ 'lifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
  { _lrTransitions                    :: !(Maybe [Transition])
  , _lrNoncurrentVersionExpiration    :: !(Maybe NoncurrentVersionExpiration)
  , _lrPrefix                         :: !(Maybe Text)
  , _lrNoncurrentVersionTransitions   :: !(Maybe [NoncurrentVersionTransition])
  , _lrExpiration                     :: !(Maybe LifecycleExpiration)
  , _lrId                             :: !(Maybe Text)
  , _lrFilter                         :: !(Maybe LifecycleRuleFilter)
  , _lrAbortIncompleteMultipartUpload :: !(Maybe AbortIncompleteMultipartUpload)
  , _lrStatus                         :: !ExpirationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrTransitions' - Undocumented member.
--
-- * 'lrNoncurrentVersionExpiration' - Undocumented member.
--
-- * 'lrPrefix' - Prefix identifying one or more objects to which the rule applies. This is deprecated; use Filter instead.
--
-- * 'lrNoncurrentVersionTransitions' - Undocumented member.
--
-- * 'lrExpiration' - Undocumented member.
--
-- * 'lrId' - Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- * 'lrFilter' - Undocumented member.
--
-- * 'lrAbortIncompleteMultipartUpload' - Undocumented member.
--
-- * 'lrStatus' - If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
lifecycleRule
    :: ExpirationStatus -- ^ 'lrStatus'
    -> LifecycleRule
lifecycleRule pStatus_ =
  LifecycleRule'
    { _lrTransitions = Nothing
    , _lrNoncurrentVersionExpiration = Nothing
    , _lrPrefix = Nothing
    , _lrNoncurrentVersionTransitions = Nothing
    , _lrExpiration = Nothing
    , _lrId = Nothing
    , _lrFilter = Nothing
    , _lrAbortIncompleteMultipartUpload = Nothing
    , _lrStatus = pStatus_
    }


-- | Undocumented member.
lrTransitions :: Lens' LifecycleRule [Transition]
lrTransitions = lens _lrTransitions (\ s a -> s{_lrTransitions = a}) . _Default . _Coerce

-- | Undocumented member.
lrNoncurrentVersionExpiration :: Lens' LifecycleRule (Maybe NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = lens _lrNoncurrentVersionExpiration (\ s a -> s{_lrNoncurrentVersionExpiration = a})

-- | Prefix identifying one or more objects to which the rule applies. This is deprecated; use Filter instead.
lrPrefix :: Lens' LifecycleRule (Maybe Text)
lrPrefix = lens _lrPrefix (\ s a -> s{_lrPrefix = a})

-- | Undocumented member.
lrNoncurrentVersionTransitions :: Lens' LifecycleRule [NoncurrentVersionTransition]
lrNoncurrentVersionTransitions = lens _lrNoncurrentVersionTransitions (\ s a -> s{_lrNoncurrentVersionTransitions = a}) . _Default . _Coerce

-- | Undocumented member.
lrExpiration :: Lens' LifecycleRule (Maybe LifecycleExpiration)
lrExpiration = lens _lrExpiration (\ s a -> s{_lrExpiration = a})

-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
lrId :: Lens' LifecycleRule (Maybe Text)
lrId = lens _lrId (\ s a -> s{_lrId = a})

-- | Undocumented member.
lrFilter :: Lens' LifecycleRule (Maybe LifecycleRuleFilter)
lrFilter = lens _lrFilter (\ s a -> s{_lrFilter = a})

-- | Undocumented member.
lrAbortIncompleteMultipartUpload :: Lens' LifecycleRule (Maybe AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = lens _lrAbortIncompleteMultipartUpload (\ s a -> s{_lrAbortIncompleteMultipartUpload = a})

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the rule is not currently being applied.
lrStatus :: Lens' LifecycleRule ExpirationStatus
lrStatus = lens _lrStatus (\ s a -> s{_lrStatus = a})

instance FromXML LifecycleRule where
        parseXML x
          = LifecycleRule' <$>
              (may (parseXMLList "Transition") x) <*>
                (x .@? "NoncurrentVersionExpiration")
                <*> (x .@? "Prefix")
                <*>
                (may (parseXMLList "NoncurrentVersionTransition") x)
                <*> (x .@? "Expiration")
                <*> (x .@? "ID")
                <*> (x .@? "Filter")
                <*> (x .@? "AbortIncompleteMultipartUpload")
                <*> (x .@ "Status")

instance Hashable LifecycleRule where

instance NFData LifecycleRule where

instance ToXML LifecycleRule where
        toXML LifecycleRule'{..}
          = mconcat
              [toXML (toXMLList "Transition" <$> _lrTransitions),
               "NoncurrentVersionExpiration" @=
                 _lrNoncurrentVersionExpiration,
               "Prefix" @= _lrPrefix,
               toXML
                 (toXMLList "NoncurrentVersionTransition" <$>
                    _lrNoncurrentVersionTransitions),
               "Expiration" @= _lrExpiration, "ID" @= _lrId,
               "Filter" @= _lrFilter,
               "AbortIncompleteMultipartUpload" @=
                 _lrAbortIncompleteMultipartUpload,
               "Status" @= _lrStatus]

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
--
-- /See:/ 'lifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
  { _lraoPrefix :: !(Maybe Text)
  , _lraoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRuleAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lraoPrefix' - Undocumented member.
--
-- * 'lraoTags' - All of these tags must exist in the object's tag set in order for the rule to apply.
lifecycleRuleAndOperator
    :: LifecycleRuleAndOperator
lifecycleRuleAndOperator =
  LifecycleRuleAndOperator' {_lraoPrefix = Nothing, _lraoTags = Nothing}


-- | Undocumented member.
lraoPrefix :: Lens' LifecycleRuleAndOperator (Maybe Text)
lraoPrefix = lens _lraoPrefix (\ s a -> s{_lraoPrefix = a})

-- | All of these tags must exist in the object's tag set in order for the rule to apply.
lraoTags :: Lens' LifecycleRuleAndOperator [Tag]
lraoTags = lens _lraoTags (\ s a -> s{_lraoTags = a}) . _Default . _Coerce

instance FromXML LifecycleRuleAndOperator where
        parseXML x
          = LifecycleRuleAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable LifecycleRuleAndOperator where

instance NFData LifecycleRuleAndOperator where

instance ToXML LifecycleRuleAndOperator where
        toXML LifecycleRuleAndOperator'{..}
          = mconcat
              ["Prefix" @= _lraoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _lraoTags)]

-- | The Filter is used to identify objects that a Lifecycle Rule applies to. A Filter must have exactly one of Prefix, Tag, or And specified.
--
-- /See:/ 'lifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { _lrfTag    :: !(Maybe Tag)
  , _lrfPrefix :: !(Maybe Text)
  , _lrfAnd    :: !(Maybe LifecycleRuleAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleRuleFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfTag' - This tag must exist in the object's tag set in order for the rule to apply.
--
-- * 'lrfPrefix' - Prefix identifying one or more objects to which the rule applies.
--
-- * 'lrfAnd' - Undocumented member.
lifecycleRuleFilter
    :: LifecycleRuleFilter
lifecycleRuleFilter =
  LifecycleRuleFilter'
    {_lrfTag = Nothing, _lrfPrefix = Nothing, _lrfAnd = Nothing}


-- | This tag must exist in the object's tag set in order for the rule to apply.
lrfTag :: Lens' LifecycleRuleFilter (Maybe Tag)
lrfTag = lens _lrfTag (\ s a -> s{_lrfTag = a})

-- | Prefix identifying one or more objects to which the rule applies.
lrfPrefix :: Lens' LifecycleRuleFilter (Maybe Text)
lrfPrefix = lens _lrfPrefix (\ s a -> s{_lrfPrefix = a})

-- | Undocumented member.
lrfAnd :: Lens' LifecycleRuleFilter (Maybe LifecycleRuleAndOperator)
lrfAnd = lens _lrfAnd (\ s a -> s{_lrfAnd = a})

instance FromXML LifecycleRuleFilter where
        parseXML x
          = LifecycleRuleFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable LifecycleRuleFilter where

instance NFData LifecycleRuleFilter where

instance ToXML LifecycleRuleFilter where
        toXML LifecycleRuleFilter'{..}
          = mconcat
              ["Tag" @= _lrfTag, "Prefix" @= _lrfPrefix,
               "And" @= _lrfAnd]

-- | Container for logging information. Presence of this element indicates that logging is enabled. Parameters TargetBucket and TargetPrefix are required in this case.
--
-- /See:/ 'loggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
  { _leTargetGrants :: !(Maybe [TargetGrant])
  , _leTargetBucket :: !Text
  , _leTargetPrefix :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leTargetGrants' - Undocumented member.
--
-- * 'leTargetBucket' - Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case you should choose a different TargetPrefix for each source bucket so that the delivered log files can be distinguished by key.
--
-- * 'leTargetPrefix' - This element lets you specify a prefix for the keys that the log files will be stored under.
loggingEnabled
    :: Text -- ^ 'leTargetBucket'
    -> Text -- ^ 'leTargetPrefix'
    -> LoggingEnabled
loggingEnabled pTargetBucket_ pTargetPrefix_ =
  LoggingEnabled'
    { _leTargetGrants = Nothing
    , _leTargetBucket = pTargetBucket_
    , _leTargetPrefix = pTargetPrefix_
    }


-- | Undocumented member.
leTargetGrants :: Lens' LoggingEnabled [TargetGrant]
leTargetGrants = lens _leTargetGrants (\ s a -> s{_leTargetGrants = a}) . _Default . _Coerce

-- | Specifies the bucket where you want Amazon S3 to store server access logs. You can have your logs delivered to any bucket that you own, including the same bucket that is being logged. You can also configure multiple buckets to deliver their logs to the same target bucket. In this case you should choose a different TargetPrefix for each source bucket so that the delivered log files can be distinguished by key.
leTargetBucket :: Lens' LoggingEnabled Text
leTargetBucket = lens _leTargetBucket (\ s a -> s{_leTargetBucket = a})

-- | This element lets you specify a prefix for the keys that the log files will be stored under.
leTargetPrefix :: Lens' LoggingEnabled Text
leTargetPrefix = lens _leTargetPrefix (\ s a -> s{_leTargetPrefix = a})

instance FromXML LoggingEnabled where
        parseXML x
          = LoggingEnabled' <$>
              (x .@? "TargetGrants" .!@ mempty >>=
                 may (parseXMLList "Grant"))
                <*> (x .@ "TargetBucket")
                <*> (x .@ "TargetPrefix")

instance Hashable LoggingEnabled where

instance NFData LoggingEnabled where

instance ToXML LoggingEnabled where
        toXML LoggingEnabled'{..}
          = mconcat
              ["TargetGrants" @=
                 toXML (toXMLList "Grant" <$> _leTargetGrants),
               "TargetBucket" @= _leTargetBucket,
               "TargetPrefix" @= _leTargetPrefix]

-- | A metadata key-value pair to store with an object.
--
-- /See:/ 'metadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { _meValue :: !(Maybe Text)
  , _meName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetadataEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meValue' - Undocumented member.
--
-- * 'meName' - Undocumented member.
metadataEntry
    :: MetadataEntry
metadataEntry = MetadataEntry' {_meValue = Nothing, _meName = Nothing}


-- | Undocumented member.
meValue :: Lens' MetadataEntry (Maybe Text)
meValue = lens _meValue (\ s a -> s{_meValue = a})

-- | Undocumented member.
meName :: Lens' MetadataEntry (Maybe Text)
meName = lens _meName (\ s a -> s{_meName = a})

instance Hashable MetadataEntry where

instance NFData MetadataEntry where

instance ToXML MetadataEntry where
        toXML MetadataEntry'{..}
          = mconcat ["Value" @= _meValue, "Name" @= _meName]

-- | /See:/ 'metricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { _maoPrefix :: !(Maybe Text)
  , _maoTags   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsAndOperator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maoPrefix' - The prefix used when evaluating an AND predicate.
--
-- * 'maoTags' - The list of tags used when evaluating an AND predicate.
metricsAndOperator
    :: MetricsAndOperator
metricsAndOperator =
  MetricsAndOperator' {_maoPrefix = Nothing, _maoTags = Nothing}


-- | The prefix used when evaluating an AND predicate.
maoPrefix :: Lens' MetricsAndOperator (Maybe Text)
maoPrefix = lens _maoPrefix (\ s a -> s{_maoPrefix = a})

-- | The list of tags used when evaluating an AND predicate.
maoTags :: Lens' MetricsAndOperator [Tag]
maoTags = lens _maoTags (\ s a -> s{_maoTags = a}) . _Default . _Coerce

instance FromXML MetricsAndOperator where
        parseXML x
          = MetricsAndOperator' <$>
              (x .@? "Prefix") <*>
                (x .@? "Tag" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable MetricsAndOperator where

instance NFData MetricsAndOperator where

instance ToXML MetricsAndOperator where
        toXML MetricsAndOperator'{..}
          = mconcat
              ["Prefix" @= _maoPrefix,
               "Tag" @= toXML (toXMLList "Tag" <$> _maoTags)]

-- | /See:/ 'metricsConfiguration' smart constructor.
data MetricsConfiguration = MetricsConfiguration'
  { _mcFilter :: !(Maybe MetricsFilter)
  , _mcId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcFilter' - Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- * 'mcId' - The ID used to identify the metrics configuration.
metricsConfiguration
    :: Text -- ^ 'mcId'
    -> MetricsConfiguration
metricsConfiguration pId_ =
  MetricsConfiguration' {_mcFilter = Nothing, _mcId = pId_}


-- | Specifies a metrics configuration filter. The metrics configuration will only include objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
mcFilter :: Lens' MetricsConfiguration (Maybe MetricsFilter)
mcFilter = lens _mcFilter (\ s a -> s{_mcFilter = a})

-- | The ID used to identify the metrics configuration.
mcId :: Lens' MetricsConfiguration Text
mcId = lens _mcId (\ s a -> s{_mcId = a})

instance FromXML MetricsConfiguration where
        parseXML x
          = MetricsConfiguration' <$>
              (x .@? "Filter") <*> (x .@ "Id")

instance Hashable MetricsConfiguration where

instance NFData MetricsConfiguration where

instance ToXML MetricsConfiguration where
        toXML MetricsConfiguration'{..}
          = mconcat ["Filter" @= _mcFilter, "Id" @= _mcId]

-- | /See:/ 'metricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { _mfTag    :: !(Maybe Tag)
  , _mfPrefix :: !(Maybe Text)
  , _mfAnd    :: !(Maybe MetricsAndOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfTag' - The tag used when evaluating a metrics filter.
--
-- * 'mfPrefix' - The prefix used when evaluating a metrics filter.
--
-- * 'mfAnd' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
metricsFilter
    :: MetricsFilter
metricsFilter =
  MetricsFilter' {_mfTag = Nothing, _mfPrefix = Nothing, _mfAnd = Nothing}


-- | The tag used when evaluating a metrics filter.
mfTag :: Lens' MetricsFilter (Maybe Tag)
mfTag = lens _mfTag (\ s a -> s{_mfTag = a})

-- | The prefix used when evaluating a metrics filter.
mfPrefix :: Lens' MetricsFilter (Maybe Text)
mfPrefix = lens _mfPrefix (\ s a -> s{_mfPrefix = a})

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
mfAnd :: Lens' MetricsFilter (Maybe MetricsAndOperator)
mfAnd = lens _mfAnd (\ s a -> s{_mfAnd = a})

instance FromXML MetricsFilter where
        parseXML x
          = MetricsFilter' <$>
              (x .@? "Tag") <*> (x .@? "Prefix") <*> (x .@? "And")

instance Hashable MetricsFilter where

instance NFData MetricsFilter where

instance ToXML MetricsFilter where
        toXML MetricsFilter'{..}
          = mconcat
              ["Tag" @= _mfTag, "Prefix" @= _mfPrefix,
               "And" @= _mfAnd]

-- | /See:/ 'multipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { _muInitiated    :: !(Maybe RFC822)
  , _muInitiator    :: !(Maybe Initiator)
  , _muOwner        :: !(Maybe Owner)
  , _muKey          :: !(Maybe ObjectKey)
  , _muStorageClass :: !(Maybe StorageClass)
  , _muUploadId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muInitiated' - Date and time at which the multipart upload was initiated.
--
-- * 'muInitiator' - Identifies who initiated the multipart upload.
--
-- * 'muOwner' - Undocumented member.
--
-- * 'muKey' - Key of the object for which the multipart upload was initiated.
--
-- * 'muStorageClass' - The class of storage used to store the object.
--
-- * 'muUploadId' - Upload ID that identifies the multipart upload.
multipartUpload
    :: MultipartUpload
multipartUpload =
  MultipartUpload'
    { _muInitiated = Nothing
    , _muInitiator = Nothing
    , _muOwner = Nothing
    , _muKey = Nothing
    , _muStorageClass = Nothing
    , _muUploadId = Nothing
    }


-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\ s a -> s{_muInitiated = a}) . mapping _Time

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\ s a -> s{_muInitiator = a})

-- | Undocumented member.
muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\ s a -> s{_muOwner = a})

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\ s a -> s{_muKey = a})

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\ s a -> s{_muStorageClass = a})

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\ s a -> s{_muUploadId = a})

instance FromXML MultipartUpload where
        parseXML x
          = MultipartUpload' <$>
              (x .@? "Initiated") <*> (x .@? "Initiator") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "UploadId")

instance Hashable MultipartUpload where

instance NFData MultipartUpload where

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { _nveNoncurrentDays :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoncurrentVersionExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nveNoncurrentDays' - <http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
noncurrentVersionExpiration
    :: Int -- ^ 'nveNoncurrentDays'
    -> NoncurrentVersionExpiration
noncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration' {_nveNoncurrentDays = pNoncurrentDays_}


-- | <http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays = lens _nveNoncurrentDays (\ s a -> s{_nveNoncurrentDays = a})

instance FromXML NoncurrentVersionExpiration where
        parseXML x
          = NoncurrentVersionExpiration' <$>
              (x .@ "NoncurrentDays")

instance Hashable NoncurrentVersionExpiration where

instance NFData NoncurrentVersionExpiration where

instance ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration'{..}
          = mconcat ["NoncurrentDays" @= _nveNoncurrentDays]

-- | Container for the transition rule that describes when noncurrent objects transition to the STANDARD_IA, ONEZONE_IA or GLACIER storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the STANDARD_IA, ONEZONE_IA or GLACIER storage class at a specific period in the object's lifetime.
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { _nvtNoncurrentDays :: !Int
  , _nvtStorageClass   :: !TransitionStorageClass
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoncurrentVersionTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nvtNoncurrentDays' - <http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
--
-- * 'nvtStorageClass' - The class of storage used to store the object.
noncurrentVersionTransition
    :: Int -- ^ 'nvtNoncurrentDays'
    -> TransitionStorageClass -- ^ 'nvtStorageClass'
    -> NoncurrentVersionTransition
noncurrentVersionTransition pNoncurrentDays_ pStorageClass_ =
  NoncurrentVersionTransition'
    {_nvtNoncurrentDays = pNoncurrentDays_, _nvtStorageClass = pStorageClass_}


-- | <http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays = lens _nvtNoncurrentDays (\ s a -> s{_nvtNoncurrentDays = a})

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = lens _nvtStorageClass (\ s a -> s{_nvtStorageClass = a})

instance FromXML NoncurrentVersionTransition where
        parseXML x
          = NoncurrentVersionTransition' <$>
              (x .@ "NoncurrentDays") <*> (x .@ "StorageClass")

instance Hashable NoncurrentVersionTransition where

instance NFData NoncurrentVersionTransition where

instance ToXML NoncurrentVersionTransition where
        toXML NoncurrentVersionTransition'{..}
          = mconcat
              ["NoncurrentDays" @= _nvtNoncurrentDays,
               "StorageClass" @= _nvtStorageClass]

-- | Container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off on the bucket.
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncQueueConfigurations          :: !(Maybe [QueueConfiguration])
  , _ncTopicConfigurations          :: !(Maybe [TopicConfiguration])
  , _ncLambdaFunctionConfigurations :: !(Maybe [LambdaFunctionConfiguration])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncQueueConfigurations' - Undocumented member.
--
-- * 'ncTopicConfigurations' - Undocumented member.
--
-- * 'ncLambdaFunctionConfigurations' - Undocumented member.
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration'
    { _ncQueueConfigurations = Nothing
    , _ncTopicConfigurations = Nothing
    , _ncLambdaFunctionConfigurations = Nothing
    }


-- | Undocumented member.
ncQueueConfigurations :: Lens' NotificationConfiguration [QueueConfiguration]
ncQueueConfigurations = lens _ncQueueConfigurations (\ s a -> s{_ncQueueConfigurations = a}) . _Default . _Coerce

-- | Undocumented member.
ncTopicConfigurations :: Lens' NotificationConfiguration [TopicConfiguration]
ncTopicConfigurations = lens _ncTopicConfigurations (\ s a -> s{_ncTopicConfigurations = a}) . _Default . _Coerce

-- | Undocumented member.
ncLambdaFunctionConfigurations :: Lens' NotificationConfiguration [LambdaFunctionConfiguration]
ncLambdaFunctionConfigurations = lens _ncLambdaFunctionConfigurations (\ s a -> s{_ncLambdaFunctionConfigurations = a}) . _Default . _Coerce

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (may (parseXMLList "QueueConfiguration") x) <*>
                (may (parseXMLList "TopicConfiguration") x)
                <*>
                (may (parseXMLList "CloudFunctionConfiguration") x)

instance Hashable NotificationConfiguration where

instance NFData NotificationConfiguration where

instance ToXML NotificationConfiguration where
        toXML NotificationConfiguration'{..}
          = mconcat
              [toXML
                 (toXMLList "QueueConfiguration" <$>
                    _ncQueueConfigurations),
               toXML
                 (toXMLList "TopicConfiguration" <$>
                    _ncTopicConfigurations),
               toXML
                 (toXMLList "CloudFunctionConfiguration" <$>
                    _ncLambdaFunctionConfigurations)]

-- | <http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
--
-- /See:/ 'notificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
  { _ncfKey :: Maybe S3KeyFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfigurationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncfKey' - Undocumented member.
notificationConfigurationFilter
    :: NotificationConfigurationFilter
notificationConfigurationFilter =
  NotificationConfigurationFilter' {_ncfKey = Nothing}


-- | Undocumented member.
ncfKey :: Lens' NotificationConfigurationFilter (Maybe S3KeyFilter)
ncfKey = lens _ncfKey (\ s a -> s{_ncfKey = a})

instance FromXML NotificationConfigurationFilter
         where
        parseXML x
          = NotificationConfigurationFilter' <$>
              (x .@? "S3Key")

instance Hashable NotificationConfigurationFilter
         where

instance NFData NotificationConfigurationFilter where

instance ToXML NotificationConfigurationFilter where
        toXML NotificationConfigurationFilter'{..}
          = mconcat ["S3Key" @= _ncfKey]

-- | /See:/ 'object'' smart constructor.
data Object = Object'
  { _oOwner        :: !(Maybe Owner)
  , _oETag         :: !ETag
  , _oSize         :: !Int
  , _oKey          :: !ObjectKey
  , _oStorageClass :: !ObjectStorageClass
  , _oLastModified :: !RFC822
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOwner' - Undocumented member.
--
-- * 'oETag' - Undocumented member.
--
-- * 'oSize' - Undocumented member.
--
-- * 'oKey' - Undocumented member.
--
-- * 'oStorageClass' - The class of storage used to store the object.
--
-- * 'oLastModified' - Undocumented member.
object'
    :: ETag -- ^ 'oETag'
    -> Int -- ^ 'oSize'
    -> ObjectKey -- ^ 'oKey'
    -> ObjectStorageClass -- ^ 'oStorageClass'
    -> UTCTime -- ^ 'oLastModified'
    -> Object
object' pETag_ pSize_ pKey_ pStorageClass_ pLastModified_ =
  Object'
    { _oOwner = Nothing
    , _oETag = pETag_
    , _oSize = pSize_
    , _oKey = pKey_
    , _oStorageClass = pStorageClass_
    , _oLastModified = _Time # pLastModified_
    }


-- | Undocumented member.
oOwner :: Lens' Object (Maybe Owner)
oOwner = lens _oOwner (\ s a -> s{_oOwner = a})

-- | Undocumented member.
oETag :: Lens' Object ETag
oETag = lens _oETag (\ s a -> s{_oETag = a})

-- | Undocumented member.
oSize :: Lens' Object Int
oSize = lens _oSize (\ s a -> s{_oSize = a})

-- | Undocumented member.
oKey :: Lens' Object ObjectKey
oKey = lens _oKey (\ s a -> s{_oKey = a})

-- | The class of storage used to store the object.
oStorageClass :: Lens' Object ObjectStorageClass
oStorageClass = lens _oStorageClass (\ s a -> s{_oStorageClass = a})

-- | Undocumented member.
oLastModified :: Lens' Object UTCTime
oLastModified = lens _oLastModified (\ s a -> s{_oLastModified = a}) . _Time

instance FromXML Object where
        parseXML x
          = Object' <$>
              (x .@? "Owner") <*> (x .@ "ETag") <*> (x .@ "Size")
                <*> (x .@ "Key")
                <*> (x .@ "StorageClass")
                <*> (x .@ "LastModified")

instance Hashable Object where

instance NFData Object where

-- | /See:/ 'objectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { _oiVersionId :: !(Maybe ObjectVersionId)
  , _oiKey       :: !ObjectKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiVersionId' - VersionId for the specific version of the object to delete.
--
-- * 'oiKey' - Key name of the object to delete.
objectIdentifier
    :: ObjectKey -- ^ 'oiKey'
    -> ObjectIdentifier
objectIdentifier pKey_ =
  ObjectIdentifier' {_oiVersionId = Nothing, _oiKey = pKey_}


-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\ s a -> s{_oiVersionId = a})

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\ s a -> s{_oiKey = a})

instance Hashable ObjectIdentifier where

instance NFData ObjectIdentifier where

instance ToXML ObjectIdentifier where
        toXML ObjectIdentifier'{..}
          = mconcat
              ["VersionId" @= _oiVersionId, "Key" @= _oiKey]

-- | /See:/ 'objectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { _ovETag         :: !(Maybe ETag)
  , _ovVersionId    :: !(Maybe ObjectVersionId)
  , _ovSize         :: !(Maybe Int)
  , _ovIsLatest     :: !(Maybe Bool)
  , _ovOwner        :: !(Maybe Owner)
  , _ovKey          :: !(Maybe ObjectKey)
  , _ovStorageClass :: !(Maybe ObjectVersionStorageClass)
  , _ovLastModified :: !(Maybe RFC822)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovETag' - Undocumented member.
--
-- * 'ovVersionId' - Version ID of an object.
--
-- * 'ovSize' - Size in bytes of the object.
--
-- * 'ovIsLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- * 'ovOwner' - Undocumented member.
--
-- * 'ovKey' - The object key.
--
-- * 'ovStorageClass' - The class of storage used to store the object.
--
-- * 'ovLastModified' - Date and time the object was last modified.
objectVersion
    :: ObjectVersion
objectVersion =
  ObjectVersion'
    { _ovETag = Nothing
    , _ovVersionId = Nothing
    , _ovSize = Nothing
    , _ovIsLatest = Nothing
    , _ovOwner = Nothing
    , _ovKey = Nothing
    , _ovStorageClass = Nothing
    , _ovLastModified = Nothing
    }


-- | Undocumented member.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\ s a -> s{_ovETag = a})

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\ s a -> s{_ovVersionId = a})

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\ s a -> s{_ovSize = a})

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\ s a -> s{_ovIsLatest = a})

-- | Undocumented member.
ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\ s a -> s{_ovOwner = a})

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\ s a -> s{_ovKey = a})

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\ s a -> s{_ovStorageClass = a})

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\ s a -> s{_ovLastModified = a}) . mapping _Time

instance FromXML ObjectVersion where
        parseXML x
          = ObjectVersion' <$>
              (x .@? "ETag") <*> (x .@? "VersionId") <*>
                (x .@? "Size")
                <*> (x .@? "IsLatest")
                <*> (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "LastModified")

instance Hashable ObjectVersion where

instance NFData ObjectVersion where

-- | Describes the location where the restore job's output is stored.
--
-- /See:/ 'outputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { _olS3 :: Maybe S3Location
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olS3' - Describes an S3 location that will receive the results of the restore request.
outputLocation
    :: OutputLocation
outputLocation = OutputLocation' {_olS3 = Nothing}


-- | Describes an S3 location that will receive the results of the restore request.
olS3 :: Lens' OutputLocation (Maybe S3Location)
olS3 = lens _olS3 (\ s a -> s{_olS3 = a})

instance Hashable OutputLocation where

instance NFData OutputLocation where

instance ToXML OutputLocation where
        toXML OutputLocation'{..} = mconcat ["S3" @= _olS3]

-- | Describes how results of the Select job are serialized.
--
-- /See:/ 'outputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { _osJSON :: !(Maybe JSONOutput)
  , _osCSV  :: !(Maybe CSVOutput)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osJSON' - Specifies JSON as request's output serialization format.
--
-- * 'osCSV' - Describes the serialization of CSV-encoded Select results.
outputSerialization
    :: OutputSerialization
outputSerialization = OutputSerialization' {_osJSON = Nothing, _osCSV = Nothing}


-- | Specifies JSON as request's output serialization format.
osJSON :: Lens' OutputSerialization (Maybe JSONOutput)
osJSON = lens _osJSON (\ s a -> s{_osJSON = a})

-- | Describes the serialization of CSV-encoded Select results.
osCSV :: Lens' OutputSerialization (Maybe CSVOutput)
osCSV = lens _osCSV (\ s a -> s{_osCSV = a})

instance Hashable OutputSerialization where

instance NFData OutputSerialization where

instance ToXML OutputSerialization where
        toXML OutputSerialization'{..}
          = mconcat ["JSON" @= _osJSON, "CSV" @= _osCSV]

-- | /See:/ 'owner' smart constructor.
data Owner = Owner'
  { _oDisplayName :: !(Maybe Text)
  , _oId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDisplayName' - Undocumented member.
--
-- * 'oId' - Undocumented member.
owner
    :: Owner
owner = Owner' {_oDisplayName = Nothing, _oId = Nothing}


-- | Undocumented member.
oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\ s a -> s{_oDisplayName = a})

-- | Undocumented member.
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a})

instance FromXML Owner where
        parseXML x
          = Owner' <$> (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Owner where

instance NFData Owner where

instance ToXML Owner where
        toXML Owner'{..}
          = mconcat
              ["DisplayName" @= _oDisplayName, "ID" @= _oId]

-- | /See:/ 'part' smart constructor.
data Part = Part'
  { _pETag         :: !(Maybe ETag)
  , _pSize         :: !(Maybe Int)
  , _pPartNumber   :: !(Maybe Int)
  , _pLastModified :: !(Maybe RFC822)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Part' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pETag' - Entity tag returned when the part was uploaded.
--
-- * 'pSize' - Size of the uploaded part data.
--
-- * 'pPartNumber' - Part number identifying the part. This is a positive integer between 1 and 10,000.
--
-- * 'pLastModified' - Date and time at which the part was uploaded.
part
    :: Part
part =
  Part'
    { _pETag = Nothing
    , _pSize = Nothing
    , _pPartNumber = Nothing
    , _pLastModified = Nothing
    }


-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe ETag)
pETag = lens _pETag (\ s a -> s{_pETag = a})

-- | Size of the uploaded part data.
pSize :: Lens' Part (Maybe Int)
pSize = lens _pSize (\ s a -> s{_pSize = a})

-- | Part number identifying the part. This is a positive integer between 1 and 10,000.
pPartNumber :: Lens' Part (Maybe Int)
pPartNumber = lens _pPartNumber (\ s a -> s{_pPartNumber = a})

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe UTCTime)
pLastModified = lens _pLastModified (\ s a -> s{_pLastModified = a}) . mapping _Time

instance FromXML Part where
        parseXML x
          = Part' <$>
              (x .@? "ETag") <*> (x .@? "Size") <*>
                (x .@? "PartNumber")
                <*> (x .@? "LastModified")

instance Hashable Part where

instance NFData Part where

-- | /See:/ 'progress' smart constructor.
data Progress = Progress'
  { _pBytesReturned  :: !(Maybe Integer)
  , _pBytesScanned   :: !(Maybe Integer)
  , _pBytesProcessed :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Progress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pBytesReturned' - Current number of bytes of records payload data returned.
--
-- * 'pBytesScanned' - Current number of object bytes scanned.
--
-- * 'pBytesProcessed' - Current number of uncompressed object bytes processed.
progress
    :: Progress
progress =
  Progress'
    { _pBytesReturned = Nothing
    , _pBytesScanned = Nothing
    , _pBytesProcessed = Nothing
    }


-- | Current number of bytes of records payload data returned.
pBytesReturned :: Lens' Progress (Maybe Integer)
pBytesReturned = lens _pBytesReturned (\ s a -> s{_pBytesReturned = a})

-- | Current number of object bytes scanned.
pBytesScanned :: Lens' Progress (Maybe Integer)
pBytesScanned = lens _pBytesScanned (\ s a -> s{_pBytesScanned = a})

-- | Current number of uncompressed object bytes processed.
pBytesProcessed :: Lens' Progress (Maybe Integer)
pBytesProcessed = lens _pBytesProcessed (\ s a -> s{_pBytesProcessed = a})

instance FromXML Progress where
        parseXML x
          = Progress' <$>
              (x .@? "BytesReturned") <*> (x .@? "BytesScanned")
                <*> (x .@? "BytesProcessed")

instance Hashable Progress where

instance NFData Progress where

-- | /See:/ 'progressEvent' smart constructor.
newtype ProgressEvent = ProgressEvent'
  { _peDetails :: Maybe Progress
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProgressEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peDetails' - The Progress event details.
progressEvent
    :: ProgressEvent
progressEvent = ProgressEvent' {_peDetails = Nothing}


-- | The Progress event details.
peDetails :: Lens' ProgressEvent (Maybe Progress)
peDetails = lens _peDetails (\ s a -> s{_peDetails = a})

instance FromXML ProgressEvent where
        parseXML x = ProgressEvent' <$> (x .@? "Details")

instance Hashable ProgressEvent where

instance NFData ProgressEvent where

-- | Container for specifying an configuration when you want Amazon S3 to publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
--
-- /See:/ 'queueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { _qcId       :: !(Maybe Text)
  , _qcFilter   :: !(Maybe NotificationConfigurationFilter)
  , _qcQueueARN :: !Text
  , _qcEvents   :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueueConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qcId' - Undocumented member.
--
-- * 'qcFilter' - Undocumented member.
--
-- * 'qcQueueARN' - Amazon SQS queue ARN to which Amazon S3 will publish a message when it detects events of specified type.
--
-- * 'qcEvents' - Undocumented member.
queueConfiguration
    :: Text -- ^ 'qcQueueARN'
    -> QueueConfiguration
queueConfiguration pQueueARN_ =
  QueueConfiguration'
    { _qcId = Nothing
    , _qcFilter = Nothing
    , _qcQueueARN = pQueueARN_
    , _qcEvents = mempty
    }


-- | Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\ s a -> s{_qcId = a})

-- | Undocumented member.
qcFilter :: Lens' QueueConfiguration (Maybe NotificationConfigurationFilter)
qcFilter = lens _qcFilter (\ s a -> s{_qcFilter = a})

-- | Amazon SQS queue ARN to which Amazon S3 will publish a message when it detects events of specified type.
qcQueueARN :: Lens' QueueConfiguration Text
qcQueueARN = lens _qcQueueARN (\ s a -> s{_qcQueueARN = a})

-- | Undocumented member.
qcEvents :: Lens' QueueConfiguration [Event]
qcEvents = lens _qcEvents (\ s a -> s{_qcEvents = a}) . _Coerce

instance FromXML QueueConfiguration where
        parseXML x
          = QueueConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Queue")
                <*> (parseXMLList "Event" x)

instance Hashable QueueConfiguration where

instance NFData QueueConfiguration where

instance ToXML QueueConfiguration where
        toXML QueueConfiguration'{..}
          = mconcat
              ["Id" @= _qcId, "Filter" @= _qcFilter,
               "Queue" @= _qcQueueARN, toXMLList "Event" _qcEvents]

-- | /See:/ 'recordsEvent' smart constructor.
newtype RecordsEvent = RecordsEvent'
  { _rePayload :: Maybe Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rePayload' - The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
recordsEvent
    :: RecordsEvent
recordsEvent = RecordsEvent' {_rePayload = Nothing}


-- | The byte array of partial, one or more result records.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rePayload :: Lens' RecordsEvent (Maybe ByteString)
rePayload = lens _rePayload (\ s a -> s{_rePayload = a}) . mapping _Base64

instance FromXML RecordsEvent where
        parseXML x = RecordsEvent' <$> (x .@? "Payload")

instance Hashable RecordsEvent where

instance NFData RecordsEvent where

-- | /See:/ 'redirect' smart constructor.
data Redirect = Redirect'
  { _rHostName             :: !(Maybe Text)
  , _rProtocol             :: !(Maybe Protocol)
  , _rHTTPRedirectCode     :: !(Maybe Text)
  , _rReplaceKeyWith       :: !(Maybe Text)
  , _rReplaceKeyPrefixWith :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Redirect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHostName' - The host name to use in the redirect request.
--
-- * 'rProtocol' - Protocol to use (http, https) when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rHTTPRedirectCode' - The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- * 'rReplaceKeyWith' - The specific object key to use in the redirect request. For example, redirect request to error.html. Not required if one of the sibling is present. Can be present only if ReplaceKeyPrefixWith is not provided.
--
-- * 'rReplaceKeyPrefixWith' - The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix docs/ (objects in the docs/ folder) to documents/, you can set a condition block with KeyPrefixEquals set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents. Not required if one of the siblings is present. Can be present only if ReplaceKeyWith is not provided.
redirect
    :: Redirect
redirect =
  Redirect'
    { _rHostName = Nothing
    , _rProtocol = Nothing
    , _rHTTPRedirectCode = Nothing
    , _rReplaceKeyWith = Nothing
    , _rReplaceKeyPrefixWith = Nothing
    }


-- | The host name to use in the redirect request.
rHostName :: Lens' Redirect (Maybe Text)
rHostName = lens _rHostName (\ s a -> s{_rHostName = a})

-- | Protocol to use (http, https) when redirecting requests. The default is the protocol that is used in the original request.
rProtocol :: Lens' Redirect (Maybe Protocol)
rProtocol = lens _rProtocol (\ s a -> s{_rProtocol = a})

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
rHTTPRedirectCode :: Lens' Redirect (Maybe Text)
rHTTPRedirectCode = lens _rHTTPRedirectCode (\ s a -> s{_rHTTPRedirectCode = a})

-- | The specific object key to use in the redirect request. For example, redirect request to error.html. Not required if one of the sibling is present. Can be present only if ReplaceKeyPrefixWith is not provided.
rReplaceKeyWith :: Lens' Redirect (Maybe Text)
rReplaceKeyWith = lens _rReplaceKeyWith (\ s a -> s{_rReplaceKeyWith = a})

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix docs/ (objects in the docs/ folder) to documents/, you can set a condition block with KeyPrefixEquals set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents. Not required if one of the siblings is present. Can be present only if ReplaceKeyWith is not provided.
rReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
rReplaceKeyPrefixWith = lens _rReplaceKeyPrefixWith (\ s a -> s{_rReplaceKeyPrefixWith = a})

instance FromXML Redirect where
        parseXML x
          = Redirect' <$>
              (x .@? "HostName") <*> (x .@? "Protocol") <*>
                (x .@? "HttpRedirectCode")
                <*> (x .@? "ReplaceKeyWith")
                <*> (x .@? "ReplaceKeyPrefixWith")

instance Hashable Redirect where

instance NFData Redirect where

instance ToXML Redirect where
        toXML Redirect'{..}
          = mconcat
              ["HostName" @= _rHostName, "Protocol" @= _rProtocol,
               "HttpRedirectCode" @= _rHTTPRedirectCode,
               "ReplaceKeyWith" @= _rReplaceKeyWith,
               "ReplaceKeyPrefixWith" @= _rReplaceKeyPrefixWith]

-- | /See:/ 'redirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { _rartProtocol :: !(Maybe Protocol)
  , _rartHostName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedirectAllRequestsTo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rartProtocol' - Protocol to use (http, https) when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rartHostName' - Name of the host where requests will be redirected.
redirectAllRequestsTo
    :: Text -- ^ 'rartHostName'
    -> RedirectAllRequestsTo
redirectAllRequestsTo pHostName_ =
  RedirectAllRequestsTo' {_rartProtocol = Nothing, _rartHostName = pHostName_}


-- | Protocol to use (http, https) when redirecting requests. The default is the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\ s a -> s{_rartProtocol = a})

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\ s a -> s{_rartHostName = a})

instance FromXML RedirectAllRequestsTo where
        parseXML x
          = RedirectAllRequestsTo' <$>
              (x .@? "Protocol") <*> (x .@ "HostName")

instance Hashable RedirectAllRequestsTo where

instance NFData RedirectAllRequestsTo where

instance ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo'{..}
          = mconcat
              ["Protocol" @= _rartProtocol,
               "HostName" @= _rartHostName]

-- | Container for replication rules. You can add as many as 1,000 rules. Total replication configuration size can be up to 2 MB.
--
-- /See:/ 'replicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
  { _rcRole  :: !Text
  , _rcRules :: ![ReplicationRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRole' - Amazon Resource Name (ARN) of an IAM role for Amazon S3 to assume when replicating the objects.
--
-- * 'rcRules' - Container for information about a particular replication rule. Replication configuration must have at least one rule and can contain up to 1,000 rules.
replicationConfiguration
    :: Text -- ^ 'rcRole'
    -> ReplicationConfiguration
replicationConfiguration pRole_ =
  ReplicationConfiguration' {_rcRole = pRole_, _rcRules = mempty}


-- | Amazon Resource Name (ARN) of an IAM role for Amazon S3 to assume when replicating the objects.
rcRole :: Lens' ReplicationConfiguration Text
rcRole = lens _rcRole (\ s a -> s{_rcRole = a})

-- | Container for information about a particular replication rule. Replication configuration must have at least one rule and can contain up to 1,000 rules.
rcRules :: Lens' ReplicationConfiguration [ReplicationRule]
rcRules = lens _rcRules (\ s a -> s{_rcRules = a}) . _Coerce

instance FromXML ReplicationConfiguration where
        parseXML x
          = ReplicationConfiguration' <$>
              (x .@ "Role") <*> (parseXMLList "Rule" x)

instance Hashable ReplicationConfiguration where

instance NFData ReplicationConfiguration where

instance ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration'{..}
          = mconcat
              ["Role" @= _rcRole, toXMLList "Rule" _rcRules]

-- | Container for information about a particular replication rule.
--
-- /See:/ 'replicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { _rrId                      :: !(Maybe Text)
  , _rrSourceSelectionCriteria :: !(Maybe SourceSelectionCriteria)
  , _rrPrefix                  :: !Text
  , _rrStatus                  :: !ReplicationRuleStatus
  , _rrDestination             :: !Destination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrId' - Unique identifier for the rule. The value cannot be longer than 255 characters.
--
-- * 'rrSourceSelectionCriteria' - Container for filters that define which source objects should be replicated.
--
-- * 'rrPrefix' - Object keyname prefix identifying one or more objects to which the rule applies. Maximum prefix length can be up to 1,024 characters. Overlapping prefixes are not supported.
--
-- * 'rrStatus' - The rule is ignored if status is not Enabled.
--
-- * 'rrDestination' - Container for replication destination information.
replicationRule
    :: Text -- ^ 'rrPrefix'
    -> ReplicationRuleStatus -- ^ 'rrStatus'
    -> Destination -- ^ 'rrDestination'
    -> ReplicationRule
replicationRule pPrefix_ pStatus_ pDestination_ =
  ReplicationRule'
    { _rrId = Nothing
    , _rrSourceSelectionCriteria = Nothing
    , _rrPrefix = pPrefix_
    , _rrStatus = pStatus_
    , _rrDestination = pDestination_
    }


-- | Unique identifier for the rule. The value cannot be longer than 255 characters.
rrId :: Lens' ReplicationRule (Maybe Text)
rrId = lens _rrId (\ s a -> s{_rrId = a})

-- | Container for filters that define which source objects should be replicated.
rrSourceSelectionCriteria :: Lens' ReplicationRule (Maybe SourceSelectionCriteria)
rrSourceSelectionCriteria = lens _rrSourceSelectionCriteria (\ s a -> s{_rrSourceSelectionCriteria = a})

-- | Object keyname prefix identifying one or more objects to which the rule applies. Maximum prefix length can be up to 1,024 characters. Overlapping prefixes are not supported.
rrPrefix :: Lens' ReplicationRule Text
rrPrefix = lens _rrPrefix (\ s a -> s{_rrPrefix = a})

-- | The rule is ignored if status is not Enabled.
rrStatus :: Lens' ReplicationRule ReplicationRuleStatus
rrStatus = lens _rrStatus (\ s a -> s{_rrStatus = a})

-- | Container for replication destination information.
rrDestination :: Lens' ReplicationRule Destination
rrDestination = lens _rrDestination (\ s a -> s{_rrDestination = a})

instance FromXML ReplicationRule where
        parseXML x
          = ReplicationRule' <$>
              (x .@? "ID") <*> (x .@? "SourceSelectionCriteria")
                <*> (x .@ "Prefix")
                <*> (x .@ "Status")
                <*> (x .@ "Destination")

instance Hashable ReplicationRule where

instance NFData ReplicationRule where

instance ToXML ReplicationRule where
        toXML ReplicationRule'{..}
          = mconcat
              ["ID" @= _rrId,
               "SourceSelectionCriteria" @=
                 _rrSourceSelectionCriteria,
               "Prefix" @= _rrPrefix, "Status" @= _rrStatus,
               "Destination" @= _rrDestination]

-- | /See:/ 'requestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
  { _rpcPayer :: Payer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestPaymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcPayer' - Specifies who pays for the download and request fees.
requestPaymentConfiguration
    :: Payer -- ^ 'rpcPayer'
    -> RequestPaymentConfiguration
requestPaymentConfiguration pPayer_ =
  RequestPaymentConfiguration' {_rpcPayer = pPayer_}


-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\ s a -> s{_rpcPayer = a})

instance Hashable RequestPaymentConfiguration where

instance NFData RequestPaymentConfiguration where

instance ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration'{..}
          = mconcat ["Payer" @= _rpcPayer]

-- | /See:/ 'requestProgress' smart constructor.
newtype RequestProgress = RequestProgress'
  { _rpEnabled :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestProgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpEnabled' - Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
requestProgress
    :: RequestProgress
requestProgress = RequestProgress' {_rpEnabled = Nothing}


-- | Specifies whether periodic QueryProgress frames should be sent. Valid values: TRUE, FALSE. Default value: FALSE.
rpEnabled :: Lens' RequestProgress (Maybe Bool)
rpEnabled = lens _rpEnabled (\ s a -> s{_rpEnabled = a})

instance Hashable RequestProgress where

instance NFData RequestProgress where

instance ToXML RequestProgress where
        toXML RequestProgress'{..}
          = mconcat ["Enabled" @= _rpEnabled]

-- | Container for restore job parameters.
--
-- /See:/ 'restoreRequest' smart constructor.
data RestoreRequest = RestoreRequest'
  { _rrDays                 :: !(Maybe Int)
  , _rrSelectParameters     :: !(Maybe SelectParameters)
  , _rrOutputLocation       :: !(Maybe OutputLocation)
  , _rrTier                 :: !(Maybe Tier)
  , _rrGlacierJobParameters :: !(Maybe GlacierJobParameters)
  , _rrType                 :: !(Maybe RestoreRequestType)
  , _rrDescription          :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDays' - Lifetime of the active copy in days. Do not use with restores that specify OutputLocation.
--
-- * 'rrSelectParameters' - Describes the parameters for Select job types.
--
-- * 'rrOutputLocation' - Describes the location where the restore job's output is stored.
--
-- * 'rrTier' - Glacier retrieval tier at which the restore will be processed.
--
-- * 'rrGlacierJobParameters' - Glacier related parameters pertaining to this job. Do not use with restores that specify OutputLocation.
--
-- * 'rrType' - Type of restore request.
--
-- * 'rrDescription' - The optional description for the job.
restoreRequest
    :: RestoreRequest
restoreRequest =
  RestoreRequest'
    { _rrDays = Nothing
    , _rrSelectParameters = Nothing
    , _rrOutputLocation = Nothing
    , _rrTier = Nothing
    , _rrGlacierJobParameters = Nothing
    , _rrType = Nothing
    , _rrDescription = Nothing
    }


-- | Lifetime of the active copy in days. Do not use with restores that specify OutputLocation.
rrDays :: Lens' RestoreRequest (Maybe Int)
rrDays = lens _rrDays (\ s a -> s{_rrDays = a})

-- | Describes the parameters for Select job types.
rrSelectParameters :: Lens' RestoreRequest (Maybe SelectParameters)
rrSelectParameters = lens _rrSelectParameters (\ s a -> s{_rrSelectParameters = a})

-- | Describes the location where the restore job's output is stored.
rrOutputLocation :: Lens' RestoreRequest (Maybe OutputLocation)
rrOutputLocation = lens _rrOutputLocation (\ s a -> s{_rrOutputLocation = a})

-- | Glacier retrieval tier at which the restore will be processed.
rrTier :: Lens' RestoreRequest (Maybe Tier)
rrTier = lens _rrTier (\ s a -> s{_rrTier = a})

-- | Glacier related parameters pertaining to this job. Do not use with restores that specify OutputLocation.
rrGlacierJobParameters :: Lens' RestoreRequest (Maybe GlacierJobParameters)
rrGlacierJobParameters = lens _rrGlacierJobParameters (\ s a -> s{_rrGlacierJobParameters = a})

-- | Type of restore request.
rrType :: Lens' RestoreRequest (Maybe RestoreRequestType)
rrType = lens _rrType (\ s a -> s{_rrType = a})

-- | The optional description for the job.
rrDescription :: Lens' RestoreRequest (Maybe Text)
rrDescription = lens _rrDescription (\ s a -> s{_rrDescription = a})

instance Hashable RestoreRequest where

instance NFData RestoreRequest where

instance ToXML RestoreRequest where
        toXML RestoreRequest'{..}
          = mconcat
              ["Days" @= _rrDays,
               "SelectParameters" @= _rrSelectParameters,
               "OutputLocation" @= _rrOutputLocation,
               "Tier" @= _rrTier,
               "GlacierJobParameters" @= _rrGlacierJobParameters,
               "Type" @= _rrType, "Description" @= _rrDescription]

-- | /See:/ 'routingRule' smart constructor.
data RoutingRule = RoutingRule'
  { _rrCondition :: !(Maybe Condition)
  , _rrRedirect  :: !Redirect
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoutingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrCondition' - A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the /docs folder, redirect to the /documents folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- * 'rrRedirect' - Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can can specify a different error code to return.
routingRule
    :: Redirect -- ^ 'rrRedirect'
    -> RoutingRule
routingRule pRedirect_ =
  RoutingRule' {_rrCondition = Nothing, _rrRedirect = pRedirect_}


-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the /docs folder, redirect to the /documents folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\ s a -> s{_rrCondition = a})

-- | Container for redirect information. You can redirect requests to another host, to another page, or with another protocol. In the event of an error, you can can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\ s a -> s{_rrRedirect = a})

instance FromXML RoutingRule where
        parseXML x
          = RoutingRule' <$>
              (x .@? "Condition") <*> (x .@ "Redirect")

instance Hashable RoutingRule where

instance NFData RoutingRule where

instance ToXML RoutingRule where
        toXML RoutingRule'{..}
          = mconcat
              ["Condition" @= _rrCondition,
               "Redirect" @= _rrRedirect]

-- | Container for object key name prefix and suffix filtering rules.
--
-- /See:/ 's3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
  { _skfFilterRules :: Maybe [FilterRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3KeyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skfFilterRules' - Undocumented member.
s3KeyFilter
    :: S3KeyFilter
s3KeyFilter = S3KeyFilter' {_skfFilterRules = Nothing}


-- | Undocumented member.
skfFilterRules :: Lens' S3KeyFilter [FilterRule]
skfFilterRules = lens _skfFilterRules (\ s a -> s{_skfFilterRules = a}) . _Default . _Coerce

instance FromXML S3KeyFilter where
        parseXML x
          = S3KeyFilter' <$>
              (may (parseXMLList "FilterRule") x)

instance Hashable S3KeyFilter where

instance NFData S3KeyFilter where

instance ToXML S3KeyFilter where
        toXML S3KeyFilter'{..}
          = mconcat
              [toXML (toXMLList "FilterRule" <$> _skfFilterRules)]

-- | Describes an S3 location that will receive the results of the restore request.
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slCannedACL         :: !(Maybe ObjectCannedACL)
  , _slAccessControlList :: !(Maybe [Grant])
  , _slUserMetadata      :: !(Maybe [MetadataEntry])
  , _slEncryption        :: !(Maybe Encryption)
  , _slStorageClass      :: !(Maybe StorageClass)
  , _slTagging           :: !(Maybe Tagging)
  , _slBucketName        :: !BucketName
  , _slPrefix            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCannedACL' - The canned ACL to apply to the restore results.
--
-- * 'slAccessControlList' - A list of grants that control access to the staged results.
--
-- * 'slUserMetadata' - A list of metadata to store with the restore results in S3.
--
-- * 'slEncryption' - Undocumented member.
--
-- * 'slStorageClass' - The class of storage used to store the restore results.
--
-- * 'slTagging' - The tag-set that is applied to the restore results.
--
-- * 'slBucketName' - The name of the bucket where the restore results will be placed.
--
-- * 'slPrefix' - The prefix that is prepended to the restore results for this request.
s3Location
    :: BucketName -- ^ 'slBucketName'
    -> Text -- ^ 'slPrefix'
    -> S3Location
s3Location pBucketName_ pPrefix_ =
  S3Location'
    { _slCannedACL = Nothing
    , _slAccessControlList = Nothing
    , _slUserMetadata = Nothing
    , _slEncryption = Nothing
    , _slStorageClass = Nothing
    , _slTagging = Nothing
    , _slBucketName = pBucketName_
    , _slPrefix = pPrefix_
    }


-- | The canned ACL to apply to the restore results.
slCannedACL :: Lens' S3Location (Maybe ObjectCannedACL)
slCannedACL = lens _slCannedACL (\ s a -> s{_slCannedACL = a})

-- | A list of grants that control access to the staged results.
slAccessControlList :: Lens' S3Location [Grant]
slAccessControlList = lens _slAccessControlList (\ s a -> s{_slAccessControlList = a}) . _Default . _Coerce

-- | A list of metadata to store with the restore results in S3.
slUserMetadata :: Lens' S3Location [MetadataEntry]
slUserMetadata = lens _slUserMetadata (\ s a -> s{_slUserMetadata = a}) . _Default . _Coerce

-- | Undocumented member.
slEncryption :: Lens' S3Location (Maybe Encryption)
slEncryption = lens _slEncryption (\ s a -> s{_slEncryption = a})

-- | The class of storage used to store the restore results.
slStorageClass :: Lens' S3Location (Maybe StorageClass)
slStorageClass = lens _slStorageClass (\ s a -> s{_slStorageClass = a})

-- | The tag-set that is applied to the restore results.
slTagging :: Lens' S3Location (Maybe Tagging)
slTagging = lens _slTagging (\ s a -> s{_slTagging = a})

-- | The name of the bucket where the restore results will be placed.
slBucketName :: Lens' S3Location BucketName
slBucketName = lens _slBucketName (\ s a -> s{_slBucketName = a})

-- | The prefix that is prepended to the restore results for this request.
slPrefix :: Lens' S3Location Text
slPrefix = lens _slPrefix (\ s a -> s{_slPrefix = a})

instance Hashable S3Location where

instance NFData S3Location where

instance ToXML S3Location where
        toXML S3Location'{..}
          = mconcat
              ["CannedACL" @= _slCannedACL,
               "AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _slAccessControlList),
               "UserMetadata" @=
                 toXML
                   (toXMLList "MetadataEntry" <$> _slUserMetadata),
               "Encryption" @= _slEncryption,
               "StorageClass" @= _slStorageClass,
               "Tagging" @= _slTagging,
               "BucketName" @= _slBucketName, "Prefix" @= _slPrefix]

-- | /See:/ 's3ServiceError' smart constructor.
data S3ServiceError = S3ServiceError'
  { _sseVersionId :: !(Maybe ObjectVersionId)
  , _sseKey       :: !(Maybe ObjectKey)
  , _sseCode      :: !(Maybe Text)
  , _sseMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ServiceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseVersionId' - Undocumented member.
--
-- * 'sseKey' - Undocumented member.
--
-- * 'sseCode' - Undocumented member.
--
-- * 'sseMessage' - Undocumented member.
s3ServiceError
    :: S3ServiceError
s3ServiceError =
  S3ServiceError'
    { _sseVersionId = Nothing
    , _sseKey = Nothing
    , _sseCode = Nothing
    , _sseMessage = Nothing
    }


-- | Undocumented member.
sseVersionId :: Lens' S3ServiceError (Maybe ObjectVersionId)
sseVersionId = lens _sseVersionId (\ s a -> s{_sseVersionId = a})

-- | Undocumented member.
sseKey :: Lens' S3ServiceError (Maybe ObjectKey)
sseKey = lens _sseKey (\ s a -> s{_sseKey = a})

-- | Undocumented member.
sseCode :: Lens' S3ServiceError (Maybe Text)
sseCode = lens _sseCode (\ s a -> s{_sseCode = a})

-- | Undocumented member.
sseMessage :: Lens' S3ServiceError (Maybe Text)
sseMessage = lens _sseMessage (\ s a -> s{_sseMessage = a})

instance FromXML S3ServiceError where
        parseXML x
          = S3ServiceError' <$>
              (x .@? "VersionId") <*> (x .@? "Key") <*>
                (x .@? "Code")
                <*> (x .@? "Message")

instance Hashable S3ServiceError where

instance NFData S3ServiceError where

-- | Specifies the use of SSE-KMS to encrypt delievered Inventory reports.
--
-- /See:/ 'sSEKMS' smart constructor.
newtype SSEKMS = SSEKMS'
  { _ssekKeyId :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSEKMS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssekKeyId' - Specifies the ID of the AWS Key Management Service (KMS) master encryption key to use for encrypting Inventory reports.
sSEKMS
    :: Text -- ^ 'ssekKeyId'
    -> SSEKMS
sSEKMS pKeyId_ = SSEKMS' {_ssekKeyId = _Sensitive # pKeyId_}


-- | Specifies the ID of the AWS Key Management Service (KMS) master encryption key to use for encrypting Inventory reports.
ssekKeyId :: Lens' SSEKMS Text
ssekKeyId = lens _ssekKeyId (\ s a -> s{_ssekKeyId = a}) . _Sensitive

instance FromXML SSEKMS where
        parseXML x = SSEKMS' <$> (x .@ "KeyId")

instance Hashable SSEKMS where

instance NFData SSEKMS where

instance ToXML SSEKMS where
        toXML SSEKMS'{..} = mconcat ["KeyId" @= _ssekKeyId]

-- | Specifies the use of SSE-S3 to encrypt delievered Inventory reports.
--
-- /See:/ 'sSES3' smart constructor.
data SSES3 =
  SSES3'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSES3' with the minimum fields required to make a request.
--
sSES3
    :: SSES3
sSES3 = SSES3'


instance FromXML SSES3 where
        parseXML = const (pure SSES3')

instance Hashable SSES3 where

instance NFData SSES3 where

instance ToXML SSES3 where
        toXML = const mempty

-- | /See:/ 'selectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { _socesProgress :: !(Maybe ProgressEvent)
  , _socesRecords  :: !(Maybe RecordsEvent)
  , _socesCont     :: !(Maybe ContinuationEvent)
  , _socesStats    :: !(Maybe StatsEvent)
  , _socesEnd      :: !(Maybe EndEvent)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectObjectContentEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socesProgress' - The Progress Event.
--
-- * 'socesRecords' - The Records Event.
--
-- * 'socesCont' - The Continuation Event.
--
-- * 'socesStats' - The Stats Event.
--
-- * 'socesEnd' - The End Event.
selectObjectContentEventStream
    :: SelectObjectContentEventStream
selectObjectContentEventStream =
  SelectObjectContentEventStream'
    { _socesProgress = Nothing
    , _socesRecords = Nothing
    , _socesCont = Nothing
    , _socesStats = Nothing
    , _socesEnd = Nothing
    }


-- | The Progress Event.
socesProgress :: Lens' SelectObjectContentEventStream (Maybe ProgressEvent)
socesProgress = lens _socesProgress (\ s a -> s{_socesProgress = a})

-- | The Records Event.
socesRecords :: Lens' SelectObjectContentEventStream (Maybe RecordsEvent)
socesRecords = lens _socesRecords (\ s a -> s{_socesRecords = a})

-- | The Continuation Event.
socesCont :: Lens' SelectObjectContentEventStream (Maybe ContinuationEvent)
socesCont = lens _socesCont (\ s a -> s{_socesCont = a})

-- | The Stats Event.
socesStats :: Lens' SelectObjectContentEventStream (Maybe StatsEvent)
socesStats = lens _socesStats (\ s a -> s{_socesStats = a})

-- | The End Event.
socesEnd :: Lens' SelectObjectContentEventStream (Maybe EndEvent)
socesEnd = lens _socesEnd (\ s a -> s{_socesEnd = a})

instance FromXML SelectObjectContentEventStream where
        parseXML x
          = SelectObjectContentEventStream' <$>
              (x .@? "Progress") <*> (x .@? "Records") <*>
                (x .@? "Cont")
                <*> (x .@? "Stats")
                <*> (x .@? "End")

instance Hashable SelectObjectContentEventStream
         where

instance NFData SelectObjectContentEventStream where

-- | Describes the parameters for Select job types.
--
-- /See:/ 'selectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { _spInputSerialization  :: !InputSerialization
  , _spExpressionType      :: !ExpressionType
  , _spExpression          :: !Text
  , _spOutputSerialization :: !OutputSerialization
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spInputSerialization' - Describes the serialization format of the object.
--
-- * 'spExpressionType' - The type of the provided expression (e.g., SQL).
--
-- * 'spExpression' - The expression that is used to query the object.
--
-- * 'spOutputSerialization' - Describes how the results of the Select job are serialized.
selectParameters
    :: InputSerialization -- ^ 'spInputSerialization'
    -> ExpressionType -- ^ 'spExpressionType'
    -> Text -- ^ 'spExpression'
    -> OutputSerialization -- ^ 'spOutputSerialization'
    -> SelectParameters
selectParameters pInputSerialization_ pExpressionType_ pExpression_ pOutputSerialization_ =
  SelectParameters'
    { _spInputSerialization = pInputSerialization_
    , _spExpressionType = pExpressionType_
    , _spExpression = pExpression_
    , _spOutputSerialization = pOutputSerialization_
    }


-- | Describes the serialization format of the object.
spInputSerialization :: Lens' SelectParameters InputSerialization
spInputSerialization = lens _spInputSerialization (\ s a -> s{_spInputSerialization = a})

-- | The type of the provided expression (e.g., SQL).
spExpressionType :: Lens' SelectParameters ExpressionType
spExpressionType = lens _spExpressionType (\ s a -> s{_spExpressionType = a})

-- | The expression that is used to query the object.
spExpression :: Lens' SelectParameters Text
spExpression = lens _spExpression (\ s a -> s{_spExpression = a})

-- | Describes how the results of the Select job are serialized.
spOutputSerialization :: Lens' SelectParameters OutputSerialization
spOutputSerialization = lens _spOutputSerialization (\ s a -> s{_spOutputSerialization = a})

instance Hashable SelectParameters where

instance NFData SelectParameters where

instance ToXML SelectParameters where
        toXML SelectParameters'{..}
          = mconcat
              ["InputSerialization" @= _spInputSerialization,
               "ExpressionType" @= _spExpressionType,
               "Expression" @= _spExpression,
               "OutputSerialization" @= _spOutputSerialization]

-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
--
-- /See:/ 'serverSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'
  { _ssebdKMSMasterKeyId :: !(Maybe (Sensitive Text))
  , _ssebdSSEAlgorithm   :: !ServerSideEncryption
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssebdKMSMasterKeyId' - KMS master key ID to use for the default encryption. This parameter is allowed if SSEAlgorithm is aws:kms.
--
-- * 'ssebdSSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
serverSideEncryptionByDefault
    :: ServerSideEncryption -- ^ 'ssebdSSEAlgorithm'
    -> ServerSideEncryptionByDefault
serverSideEncryptionByDefault pSSEAlgorithm_ =
  ServerSideEncryptionByDefault'
    {_ssebdKMSMasterKeyId = Nothing, _ssebdSSEAlgorithm = pSSEAlgorithm_}


-- | KMS master key ID to use for the default encryption. This parameter is allowed if SSEAlgorithm is aws:kms.
ssebdKMSMasterKeyId :: Lens' ServerSideEncryptionByDefault (Maybe Text)
ssebdKMSMasterKeyId = lens _ssebdKMSMasterKeyId (\ s a -> s{_ssebdKMSMasterKeyId = a}) . mapping _Sensitive

-- | Server-side encryption algorithm to use for the default encryption.
ssebdSSEAlgorithm :: Lens' ServerSideEncryptionByDefault ServerSideEncryption
ssebdSSEAlgorithm = lens _ssebdSSEAlgorithm (\ s a -> s{_ssebdSSEAlgorithm = a})

instance FromXML ServerSideEncryptionByDefault where
        parseXML x
          = ServerSideEncryptionByDefault' <$>
              (x .@? "KMSMasterKeyID") <*> (x .@ "SSEAlgorithm")

instance Hashable ServerSideEncryptionByDefault where

instance NFData ServerSideEncryptionByDefault where

instance ToXML ServerSideEncryptionByDefault where
        toXML ServerSideEncryptionByDefault'{..}
          = mconcat
              ["KMSMasterKeyID" @= _ssebdKMSMasterKeyId,
               "SSEAlgorithm" @= _ssebdSSEAlgorithm]

-- | Container for server-side encryption configuration rules. Currently S3 supports one rule only.
--
-- /See:/ 'serverSideEncryptionConfiguration' smart constructor.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { _ssecRules :: [ServerSideEncryptionRule]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssecRules' - Container for information about a particular server-side encryption configuration rule.
serverSideEncryptionConfiguration
    :: ServerSideEncryptionConfiguration
serverSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration' {_ssecRules = mempty}


-- | Container for information about a particular server-side encryption configuration rule.
ssecRules :: Lens' ServerSideEncryptionConfiguration [ServerSideEncryptionRule]
ssecRules = lens _ssecRules (\ s a -> s{_ssecRules = a}) . _Coerce

instance FromXML ServerSideEncryptionConfiguration
         where
        parseXML x
          = ServerSideEncryptionConfiguration' <$>
              (parseXMLList "Rule" x)

instance Hashable ServerSideEncryptionConfiguration
         where

instance NFData ServerSideEncryptionConfiguration
         where

instance ToXML ServerSideEncryptionConfiguration
         where
        toXML ServerSideEncryptionConfiguration'{..}
          = mconcat [toXMLList "Rule" _ssecRules]

-- | Container for information about a particular server-side encryption configuration rule.
--
-- /See:/ 'serverSideEncryptionRule' smart constructor.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule'
  { _sserApplyServerSideEncryptionByDefault :: Maybe ServerSideEncryptionByDefault
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerSideEncryptionRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sserApplyServerSideEncryptionByDefault' - Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
serverSideEncryptionRule
    :: ServerSideEncryptionRule
serverSideEncryptionRule =
  ServerSideEncryptionRule' {_sserApplyServerSideEncryptionByDefault = Nothing}


-- | Describes the default server-side encryption to apply to new objects in the bucket. If Put Object request does not specify any server-side encryption, this default encryption will be applied.
sserApplyServerSideEncryptionByDefault :: Lens' ServerSideEncryptionRule (Maybe ServerSideEncryptionByDefault)
sserApplyServerSideEncryptionByDefault = lens _sserApplyServerSideEncryptionByDefault (\ s a -> s{_sserApplyServerSideEncryptionByDefault = a})

instance FromXML ServerSideEncryptionRule where
        parseXML x
          = ServerSideEncryptionRule' <$>
              (x .@? "ApplyServerSideEncryptionByDefault")

instance Hashable ServerSideEncryptionRule where

instance NFData ServerSideEncryptionRule where

instance ToXML ServerSideEncryptionRule where
        toXML ServerSideEncryptionRule'{..}
          = mconcat
              ["ApplyServerSideEncryptionByDefault" @=
                 _sserApplyServerSideEncryptionByDefault]

-- | Container for filters that define which source objects should be replicated.
--
-- /See:/ 'sourceSelectionCriteria' smart constructor.
newtype SourceSelectionCriteria = SourceSelectionCriteria'
  { _sscSseKMSEncryptedObjects :: Maybe SseKMSEncryptedObjects
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceSelectionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscSseKMSEncryptedObjects' - Container for filter information of selection of KMS Encrypted S3 objects.
sourceSelectionCriteria
    :: SourceSelectionCriteria
sourceSelectionCriteria =
  SourceSelectionCriteria' {_sscSseKMSEncryptedObjects = Nothing}


-- | Container for filter information of selection of KMS Encrypted S3 objects.
sscSseKMSEncryptedObjects :: Lens' SourceSelectionCriteria (Maybe SseKMSEncryptedObjects)
sscSseKMSEncryptedObjects = lens _sscSseKMSEncryptedObjects (\ s a -> s{_sscSseKMSEncryptedObjects = a})

instance FromXML SourceSelectionCriteria where
        parseXML x
          = SourceSelectionCriteria' <$>
              (x .@? "SseKmsEncryptedObjects")

instance Hashable SourceSelectionCriteria where

instance NFData SourceSelectionCriteria where

instance ToXML SourceSelectionCriteria where
        toXML SourceSelectionCriteria'{..}
          = mconcat
              ["SseKmsEncryptedObjects" @=
                 _sscSseKMSEncryptedObjects]

-- | Container for filter information of selection of KMS Encrypted S3 objects.
--
-- /See:/ 'sseKMSEncryptedObjects' smart constructor.
newtype SseKMSEncryptedObjects = SseKMSEncryptedObjects'
  { _skeoStatus :: SseKMSEncryptedObjectsStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SseKMSEncryptedObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skeoStatus' - The replication for KMS encrypted S3 objects is disabled if status is not Enabled.
sseKMSEncryptedObjects
    :: SseKMSEncryptedObjectsStatus -- ^ 'skeoStatus'
    -> SseKMSEncryptedObjects
sseKMSEncryptedObjects pStatus_ =
  SseKMSEncryptedObjects' {_skeoStatus = pStatus_}


-- | The replication for KMS encrypted S3 objects is disabled if status is not Enabled.
skeoStatus :: Lens' SseKMSEncryptedObjects SseKMSEncryptedObjectsStatus
skeoStatus = lens _skeoStatus (\ s a -> s{_skeoStatus = a})

instance FromXML SseKMSEncryptedObjects where
        parseXML x
          = SseKMSEncryptedObjects' <$> (x .@ "Status")

instance Hashable SseKMSEncryptedObjects where

instance NFData SseKMSEncryptedObjects where

instance ToXML SseKMSEncryptedObjects where
        toXML SseKMSEncryptedObjects'{..}
          = mconcat ["Status" @= _skeoStatus]

-- | /See:/ 'stats' smart constructor.
data Stats = Stats'
  { _sBytesReturned  :: !(Maybe Integer)
  , _sBytesScanned   :: !(Maybe Integer)
  , _sBytesProcessed :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sBytesReturned' - Total number of bytes of records payload data returned.
--
-- * 'sBytesScanned' - Total number of object bytes scanned.
--
-- * 'sBytesProcessed' - Total number of uncompressed object bytes processed.
stats
    :: Stats
stats =
  Stats'
    { _sBytesReturned = Nothing
    , _sBytesScanned = Nothing
    , _sBytesProcessed = Nothing
    }


-- | Total number of bytes of records payload data returned.
sBytesReturned :: Lens' Stats (Maybe Integer)
sBytesReturned = lens _sBytesReturned (\ s a -> s{_sBytesReturned = a})

-- | Total number of object bytes scanned.
sBytesScanned :: Lens' Stats (Maybe Integer)
sBytesScanned = lens _sBytesScanned (\ s a -> s{_sBytesScanned = a})

-- | Total number of uncompressed object bytes processed.
sBytesProcessed :: Lens' Stats (Maybe Integer)
sBytesProcessed = lens _sBytesProcessed (\ s a -> s{_sBytesProcessed = a})

instance FromXML Stats where
        parseXML x
          = Stats' <$>
              (x .@? "BytesReturned") <*> (x .@? "BytesScanned")
                <*> (x .@? "BytesProcessed")

instance Hashable Stats where

instance NFData Stats where

-- | /See:/ 'statsEvent' smart constructor.
newtype StatsEvent = StatsEvent'
  { _seDetails :: Maybe Stats
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StatsEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seDetails' - The Stats event details.
statsEvent
    :: StatsEvent
statsEvent = StatsEvent' {_seDetails = Nothing}


-- | The Stats event details.
seDetails :: Lens' StatsEvent (Maybe Stats)
seDetails = lens _seDetails (\ s a -> s{_seDetails = a})

instance FromXML StatsEvent where
        parseXML x = StatsEvent' <$> (x .@? "Details")

instance Hashable StatsEvent where

instance NFData StatsEvent where

-- | /See:/ 'storageClassAnalysis' smart constructor.
newtype StorageClassAnalysis = StorageClassAnalysis'
  { _scaDataExport :: Maybe StorageClassAnalysisDataExport
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageClassAnalysis' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scaDataExport' - A container used to describe how data related to the storage class analysis should be exported.
storageClassAnalysis
    :: StorageClassAnalysis
storageClassAnalysis = StorageClassAnalysis' {_scaDataExport = Nothing}


-- | A container used to describe how data related to the storage class analysis should be exported.
scaDataExport :: Lens' StorageClassAnalysis (Maybe StorageClassAnalysisDataExport)
scaDataExport = lens _scaDataExport (\ s a -> s{_scaDataExport = a})

instance FromXML StorageClassAnalysis where
        parseXML x
          = StorageClassAnalysis' <$> (x .@? "DataExport")

instance Hashable StorageClassAnalysis where

instance NFData StorageClassAnalysis where

instance ToXML StorageClassAnalysis where
        toXML StorageClassAnalysis'{..}
          = mconcat ["DataExport" @= _scaDataExport]

-- | /See:/ 'storageClassAnalysisDataExport' smart constructor.
data StorageClassAnalysisDataExport = StorageClassAnalysisDataExport'
  { _scadeOutputSchemaVersion :: !StorageClassAnalysisSchemaVersion
  , _scadeDestination         :: !AnalyticsExportDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageClassAnalysisDataExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scadeOutputSchemaVersion' - The version of the output schema to use when exporting data. Must be V_1.
--
-- * 'scadeDestination' - The place to store the data for an analysis.
storageClassAnalysisDataExport
    :: StorageClassAnalysisSchemaVersion -- ^ 'scadeOutputSchemaVersion'
    -> AnalyticsExportDestination -- ^ 'scadeDestination'
    -> StorageClassAnalysisDataExport
storageClassAnalysisDataExport pOutputSchemaVersion_ pDestination_ =
  StorageClassAnalysisDataExport'
    { _scadeOutputSchemaVersion = pOutputSchemaVersion_
    , _scadeDestination = pDestination_
    }


-- | The version of the output schema to use when exporting data. Must be V_1.
scadeOutputSchemaVersion :: Lens' StorageClassAnalysisDataExport StorageClassAnalysisSchemaVersion
scadeOutputSchemaVersion = lens _scadeOutputSchemaVersion (\ s a -> s{_scadeOutputSchemaVersion = a})

-- | The place to store the data for an analysis.
scadeDestination :: Lens' StorageClassAnalysisDataExport AnalyticsExportDestination
scadeDestination = lens _scadeDestination (\ s a -> s{_scadeDestination = a})

instance FromXML StorageClassAnalysisDataExport where
        parseXML x
          = StorageClassAnalysisDataExport' <$>
              (x .@ "OutputSchemaVersion") <*> (x .@ "Destination")

instance Hashable StorageClassAnalysisDataExport
         where

instance NFData StorageClassAnalysisDataExport where

instance ToXML StorageClassAnalysisDataExport where
        toXML StorageClassAnalysisDataExport'{..}
          = mconcat
              ["OutputSchemaVersion" @= _scadeOutputSchemaVersion,
               "Destination" @= _scadeDestination]

-- | /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !ObjectKey
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Name of the tag.
--
-- * 'tagValue' - Value of the tag.
tag
    :: ObjectKey -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | Name of the tag.
tagKey :: Lens' Tag ObjectKey
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | Value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag where

instance NFData Tag where

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Key" @= _tagKey, "Value" @= _tagValue]

-- | /See:/ 'tagging' smart constructor.
newtype Tagging = Tagging'
  { _tTagSet :: [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTagSet' - Undocumented member.
tagging
    :: Tagging
tagging = Tagging' {_tTagSet = mempty}


-- | Undocumented member.
tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\ s a -> s{_tTagSet = a}) . _Coerce

instance Hashable Tagging where

instance NFData Tagging where

instance ToXML Tagging where
        toXML Tagging'{..}
          = mconcat ["TagSet" @= toXMLList "Tag" _tTagSet]

-- | /See:/ 'targetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { _tgPermission :: !(Maybe BucketLogsPermission)
  , _tgGrantee    :: !(Maybe Grantee)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgPermission' - Logging permissions assigned to the Grantee for the bucket.
--
-- * 'tgGrantee' - Undocumented member.
targetGrant
    :: TargetGrant
targetGrant = TargetGrant' {_tgPermission = Nothing, _tgGrantee = Nothing}


-- | Logging permissions assigned to the Grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\ s a -> s{_tgPermission = a})

-- | Undocumented member.
tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\ s a -> s{_tgGrantee = a})

instance FromXML TargetGrant where
        parseXML x
          = TargetGrant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance Hashable TargetGrant where

instance NFData TargetGrant where

instance ToXML TargetGrant where
        toXML TargetGrant'{..}
          = mconcat
              ["Permission" @= _tgPermission,
               "Grantee" @= _tgGrantee]

-- | Container for specifying the configuration when you want Amazon S3 to publish events to an Amazon Simple Notification Service (Amazon SNS) topic.
--
-- /See:/ 'topicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { _tcId       :: !(Maybe Text)
  , _tcFilter   :: !(Maybe NotificationConfigurationFilter)
  , _tcTopicARN :: !Text
  , _tcEvents   :: ![Event]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TopicConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcId' - Undocumented member.
--
-- * 'tcFilter' - Undocumented member.
--
-- * 'tcTopicARN' - Amazon SNS topic ARN to which Amazon S3 will publish a message when it detects events of specified type.
--
-- * 'tcEvents' - Undocumented member.
topicConfiguration
    :: Text -- ^ 'tcTopicARN'
    -> TopicConfiguration
topicConfiguration pTopicARN_ =
  TopicConfiguration'
    { _tcId = Nothing
    , _tcFilter = Nothing
    , _tcTopicARN = pTopicARN_
    , _tcEvents = mempty
    }


-- | Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\ s a -> s{_tcId = a})

-- | Undocumented member.
tcFilter :: Lens' TopicConfiguration (Maybe NotificationConfigurationFilter)
tcFilter = lens _tcFilter (\ s a -> s{_tcFilter = a})

-- | Amazon SNS topic ARN to which Amazon S3 will publish a message when it detects events of specified type.
tcTopicARN :: Lens' TopicConfiguration Text
tcTopicARN = lens _tcTopicARN (\ s a -> s{_tcTopicARN = a})

-- | Undocumented member.
tcEvents :: Lens' TopicConfiguration [Event]
tcEvents = lens _tcEvents (\ s a -> s{_tcEvents = a}) . _Coerce

instance FromXML TopicConfiguration where
        parseXML x
          = TopicConfiguration' <$>
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Topic")
                <*> (parseXMLList "Event" x)

instance Hashable TopicConfiguration where

instance NFData TopicConfiguration where

instance ToXML TopicConfiguration where
        toXML TopicConfiguration'{..}
          = mconcat
              ["Id" @= _tcId, "Filter" @= _tcFilter,
               "Topic" @= _tcTopicARN, toXMLList "Event" _tcEvents]

-- | /See:/ 'transition' smart constructor.
data Transition = Transition'
  { _tDays         :: !(Maybe Int)
  , _tDate         :: !(Maybe RFC822)
  , _tStorageClass :: !(Maybe TransitionStorageClass)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Transition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDays' - Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
--
-- * 'tDate' - Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
--
-- * 'tStorageClass' - The class of storage used to store the object.
transition
    :: Transition
transition =
  Transition' {_tDays = Nothing, _tDate = Nothing, _tStorageClass = Nothing}


-- | Indicates the lifetime, in days, of the objects that are subject to the rule. The value must be a non-zero positive integer.
tDays :: Lens' Transition (Maybe Int)
tDays = lens _tDays (\ s a -> s{_tDays = a})

-- | Indicates at what date the object is to be moved or deleted. Should be in GMT ISO 8601 Format.
tDate :: Lens' Transition (Maybe UTCTime)
tDate = lens _tDate (\ s a -> s{_tDate = a}) . mapping _Time

-- | The class of storage used to store the object.
tStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
tStorageClass = lens _tStorageClass (\ s a -> s{_tStorageClass = a})

instance FromXML Transition where
        parseXML x
          = Transition' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "StorageClass")

instance Hashable Transition where

instance NFData Transition where

instance ToXML Transition where
        toXML Transition'{..}
          = mconcat
              ["Days" @= _tDays, "Date" @= _tDate,
               "StorageClass" @= _tStorageClass]

-- | /See:/ 'versioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { _vcStatus    :: !(Maybe BucketVersioningStatus)
  , _vcMFADelete :: !(Maybe MFADelete)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcStatus' - The versioning state of the bucket.
--
-- * 'vcMFADelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
versioningConfiguration
    :: VersioningConfiguration
versioningConfiguration =
  VersioningConfiguration' {_vcStatus = Nothing, _vcMFADelete = Nothing}


-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe BucketVersioningStatus)
vcStatus = lens _vcStatus (\ s a -> s{_vcStatus = a})

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe MFADelete)
vcMFADelete = lens _vcMFADelete (\ s a -> s{_vcMFADelete = a})

instance Hashable VersioningConfiguration where

instance NFData VersioningConfiguration where

instance ToXML VersioningConfiguration where
        toXML VersioningConfiguration'{..}
          = mconcat
              ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]

-- | /See:/ 'websiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { _wcRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
  , _wcErrorDocument         :: !(Maybe ErrorDocument)
  , _wcIndexDocument         :: !(Maybe IndexDocument)
  , _wcRoutingRules          :: !(Maybe [RoutingRule])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WebsiteConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcRedirectAllRequestsTo' - Undocumented member.
--
-- * 'wcErrorDocument' - Undocumented member.
--
-- * 'wcIndexDocument' - Undocumented member.
--
-- * 'wcRoutingRules' - Undocumented member.
websiteConfiguration
    :: WebsiteConfiguration
websiteConfiguration =
  WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing
    , _wcErrorDocument = Nothing
    , _wcIndexDocument = Nothing
    , _wcRoutingRules = Nothing
    }


-- | Undocumented member.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\ s a -> s{_wcRedirectAllRequestsTo = a})

-- | Undocumented member.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\ s a -> s{_wcErrorDocument = a})

-- | Undocumented member.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\ s a -> s{_wcIndexDocument = a})

-- | Undocumented member.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\ s a -> s{_wcRoutingRules = a}) . _Default . _Coerce

instance Hashable WebsiteConfiguration where

instance NFData WebsiteConfiguration where

instance ToXML WebsiteConfiguration where
        toXML WebsiteConfiguration'{..}
          = mconcat
              ["RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
               "ErrorDocument" @= _wcErrorDocument,
               "IndexDocument" @= _wcIndexDocument,
               "RoutingRules" @=
                 toXML (toXMLList "RoutingRule" <$> _wcRoutingRules)]

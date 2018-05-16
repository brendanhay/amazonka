{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Product where

import Network.AWS.Glacier.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the Amazon Glacier response to your request.
--
--
-- For information about the underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> . For conceptual information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon Glacier> .
--
--
-- /See:/ 'archiveCreationOutput' smart constructor.
data ArchiveCreationOutput = ArchiveCreationOutput'
  { _acoArchiveId :: !(Maybe Text)
  , _acoChecksum  :: !(Maybe Text)
  , _acoLocation  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ArchiveCreationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acoArchiveId' - The ID of the archive. This value is also included as part of the location.
--
-- * 'acoChecksum' - The checksum of the archive computed by Amazon Glacier.
--
-- * 'acoLocation' - The relative URI path of the newly added archive resource.
archiveCreationOutput
    :: ArchiveCreationOutput
archiveCreationOutput =
  ArchiveCreationOutput'
    {_acoArchiveId = Nothing, _acoChecksum = Nothing, _acoLocation = Nothing}


-- | The ID of the archive. This value is also included as part of the location.
acoArchiveId :: Lens' ArchiveCreationOutput (Maybe Text)
acoArchiveId = lens _acoArchiveId (\ s a -> s{_acoArchiveId = a})

-- | The checksum of the archive computed by Amazon Glacier.
acoChecksum :: Lens' ArchiveCreationOutput (Maybe Text)
acoChecksum = lens _acoChecksum (\ s a -> s{_acoChecksum = a})

-- | The relative URI path of the newly added archive resource.
acoLocation :: Lens' ArchiveCreationOutput (Maybe Text)
acoLocation = lens _acoLocation (\ s a -> s{_acoLocation = a})

instance FromJSON ArchiveCreationOutput where
        parseJSON
          = withObject "ArchiveCreationOutput"
              (\ x ->
                 ArchiveCreationOutput' <$>
                   (x .:? "x-amz-archive-id") <*>
                     (x .:? "x-amz-sha256-tree-hash")
                     <*> (x .:? "Location"))

instance Hashable ArchiveCreationOutput where

instance NFData ArchiveCreationOutput where

-- | Contains information about the comma-separated value (CSV) file to select from.
--
--
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
-- * 'ciQuoteCharacter' - A value used as an escape character where the field delimiter is part of the value.
--
-- * 'ciRecordDelimiter' - A value used to separate individual records from each other.
--
-- * 'ciFileHeaderInfo' - Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
--
-- * 'ciQuoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- * 'ciComments' - A single character used to indicate that a row should be ignored when the character is present at the start of that row.
--
-- * 'ciFieldDelimiter' - A value used to separate individual fields from each other within a record.
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


-- | A value used as an escape character where the field delimiter is part of the value.
ciQuoteCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteCharacter = lens _ciQuoteCharacter (\ s a -> s{_ciQuoteCharacter = a})

-- | A value used to separate individual records from each other.
ciRecordDelimiter :: Lens' CSVInput (Maybe Text)
ciRecordDelimiter = lens _ciRecordDelimiter (\ s a -> s{_ciRecordDelimiter = a})

-- | Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
ciFileHeaderInfo :: Lens' CSVInput (Maybe FileHeaderInfo)
ciFileHeaderInfo = lens _ciFileHeaderInfo (\ s a -> s{_ciFileHeaderInfo = a})

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
ciQuoteEscapeCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteEscapeCharacter = lens _ciQuoteEscapeCharacter (\ s a -> s{_ciQuoteEscapeCharacter = a})

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row.
ciComments :: Lens' CSVInput (Maybe Text)
ciComments = lens _ciComments (\ s a -> s{_ciComments = a})

-- | A value used to separate individual fields from each other within a record.
ciFieldDelimiter :: Lens' CSVInput (Maybe Text)
ciFieldDelimiter = lens _ciFieldDelimiter (\ s a -> s{_ciFieldDelimiter = a})

instance FromJSON CSVInput where
        parseJSON
          = withObject "CSVInput"
              (\ x ->
                 CSVInput' <$>
                   (x .:? "QuoteCharacter") <*>
                     (x .:? "RecordDelimiter")
                     <*> (x .:? "FileHeaderInfo")
                     <*> (x .:? "QuoteEscapeCharacter")
                     <*> (x .:? "Comments")
                     <*> (x .:? "FieldDelimiter"))

instance Hashable CSVInput where

instance NFData CSVInput where

instance ToJSON CSVInput where
        toJSON CSVInput'{..}
          = object
              (catMaybes
                 [("QuoteCharacter" .=) <$> _ciQuoteCharacter,
                  ("RecordDelimiter" .=) <$> _ciRecordDelimiter,
                  ("FileHeaderInfo" .=) <$> _ciFileHeaderInfo,
                  ("QuoteEscapeCharacter" .=) <$>
                    _ciQuoteEscapeCharacter,
                  ("Comments" .=) <$> _ciComments,
                  ("FieldDelimiter" .=) <$> _ciFieldDelimiter])

-- | Contains information about the comma-separated value (CSV) file that the job results are stored in.
--
--
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
-- * 'coQuoteCharacter' - A value used as an escape character where the field delimiter is part of the value.
--
-- * 'coQuoteFields' - A value that indicates whether all output fields should be contained within quotation marks.
--
-- * 'coRecordDelimiter' - A value used to separate individual records from each other.
--
-- * 'coQuoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- * 'coFieldDelimiter' - A value used to separate individual fields from each other within a record.
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


-- | A value used as an escape character where the field delimiter is part of the value.
coQuoteCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteCharacter = lens _coQuoteCharacter (\ s a -> s{_coQuoteCharacter = a})

-- | A value that indicates whether all output fields should be contained within quotation marks.
coQuoteFields :: Lens' CSVOutput (Maybe QuoteFields)
coQuoteFields = lens _coQuoteFields (\ s a -> s{_coQuoteFields = a})

-- | A value used to separate individual records from each other.
coRecordDelimiter :: Lens' CSVOutput (Maybe Text)
coRecordDelimiter = lens _coRecordDelimiter (\ s a -> s{_coRecordDelimiter = a})

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
coQuoteEscapeCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteEscapeCharacter = lens _coQuoteEscapeCharacter (\ s a -> s{_coQuoteEscapeCharacter = a})

-- | A value used to separate individual fields from each other within a record.
coFieldDelimiter :: Lens' CSVOutput (Maybe Text)
coFieldDelimiter = lens _coFieldDelimiter (\ s a -> s{_coFieldDelimiter = a})

instance FromJSON CSVOutput where
        parseJSON
          = withObject "CSVOutput"
              (\ x ->
                 CSVOutput' <$>
                   (x .:? "QuoteCharacter") <*> (x .:? "QuoteFields")
                     <*> (x .:? "RecordDelimiter")
                     <*> (x .:? "QuoteEscapeCharacter")
                     <*> (x .:? "FieldDelimiter"))

instance Hashable CSVOutput where

instance NFData CSVOutput where

instance ToJSON CSVOutput where
        toJSON CSVOutput'{..}
          = object
              (catMaybes
                 [("QuoteCharacter" .=) <$> _coQuoteCharacter,
                  ("QuoteFields" .=) <$> _coQuoteFields,
                  ("RecordDelimiter" .=) <$> _coRecordDelimiter,
                  ("QuoteEscapeCharacter" .=) <$>
                    _coQuoteEscapeCharacter,
                  ("FieldDelimiter" .=) <$> _coFieldDelimiter])

-- | Data retrieval policy.
--
--
--
-- /See:/ 'dataRetrievalPolicy' smart constructor.
newtype DataRetrievalPolicy = DataRetrievalPolicy'
  { _drpRules :: Maybe [DataRetrievalRule]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataRetrievalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRules' - The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
dataRetrievalPolicy
    :: DataRetrievalPolicy
dataRetrievalPolicy = DataRetrievalPolicy' {_drpRules = Nothing}


-- | The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
drpRules :: Lens' DataRetrievalPolicy [DataRetrievalRule]
drpRules = lens _drpRules (\ s a -> s{_drpRules = a}) . _Default . _Coerce

instance FromJSON DataRetrievalPolicy where
        parseJSON
          = withObject "DataRetrievalPolicy"
              (\ x ->
                 DataRetrievalPolicy' <$> (x .:? "Rules" .!= mempty))

instance Hashable DataRetrievalPolicy where

instance NFData DataRetrievalPolicy where

instance ToJSON DataRetrievalPolicy where
        toJSON DataRetrievalPolicy'{..}
          = object (catMaybes [("Rules" .=) <$> _drpRules])

-- | Data retrieval policy rule.
--
--
--
-- /See:/ 'dataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { _drrStrategy     :: !(Maybe Text)
  , _drrBytesPerHour :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataRetrievalRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrStrategy' - The type of data retrieval policy to set. Valid values: BytesPerHour|FreeTier|None
--
-- * 'drrBytesPerHour' - The maximum number of bytes that can be retrieved in an hour. This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
dataRetrievalRule
    :: DataRetrievalRule
dataRetrievalRule =
  DataRetrievalRule' {_drrStrategy = Nothing, _drrBytesPerHour = Nothing}


-- | The type of data retrieval policy to set. Valid values: BytesPerHour|FreeTier|None
drrStrategy :: Lens' DataRetrievalRule (Maybe Text)
drrStrategy = lens _drrStrategy (\ s a -> s{_drrStrategy = a})

-- | The maximum number of bytes that can be retrieved in an hour. This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
drrBytesPerHour :: Lens' DataRetrievalRule (Maybe Integer)
drrBytesPerHour = lens _drrBytesPerHour (\ s a -> s{_drrBytesPerHour = a})

instance FromJSON DataRetrievalRule where
        parseJSON
          = withObject "DataRetrievalRule"
              (\ x ->
                 DataRetrievalRule' <$>
                   (x .:? "Strategy") <*> (x .:? "BytesPerHour"))

instance Hashable DataRetrievalRule where

instance NFData DataRetrievalRule where

instance ToJSON DataRetrievalRule where
        toJSON DataRetrievalRule'{..}
          = object
              (catMaybes
                 [("Strategy" .=) <$> _drrStrategy,
                  ("BytesPerHour" .=) <$> _drrBytesPerHour])

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'describeVaultOutput' smart constructor.
data DescribeVaultOutput = DescribeVaultOutput'
  { _dvoVaultName         :: !(Maybe Text)
  , _dvoSizeInBytes       :: !(Maybe Integer)
  , _dvoLastInventoryDate :: !(Maybe Text)
  , _dvoVaultARN          :: !(Maybe Text)
  , _dvoCreationDate      :: !(Maybe Text)
  , _dvoNumberOfArchives  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVaultOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvoVaultName' - The name of the vault.
--
-- * 'dvoSizeInBytes' - Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
--
-- * 'dvoLastInventoryDate' - The Universal Coordinated Time (UTC) date when Amazon Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- * 'dvoVaultARN' - The Amazon Resource Name (ARN) of the vault.
--
-- * 'dvoCreationDate' - The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- * 'dvoNumberOfArchives' - The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
describeVaultOutput
    :: DescribeVaultOutput
describeVaultOutput =
  DescribeVaultOutput'
    { _dvoVaultName = Nothing
    , _dvoSizeInBytes = Nothing
    , _dvoLastInventoryDate = Nothing
    , _dvoVaultARN = Nothing
    , _dvoCreationDate = Nothing
    , _dvoNumberOfArchives = Nothing
    }


-- | The name of the vault.
dvoVaultName :: Lens' DescribeVaultOutput (Maybe Text)
dvoVaultName = lens _dvoVaultName (\ s a -> s{_dvoVaultName = a})

-- | Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
dvoSizeInBytes :: Lens' DescribeVaultOutput (Maybe Integer)
dvoSizeInBytes = lens _dvoSizeInBytes (\ s a -> s{_dvoSizeInBytes = a})

-- | The Universal Coordinated Time (UTC) date when Amazon Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
dvoLastInventoryDate :: Lens' DescribeVaultOutput (Maybe Text)
dvoLastInventoryDate = lens _dvoLastInventoryDate (\ s a -> s{_dvoLastInventoryDate = a})

-- | The Amazon Resource Name (ARN) of the vault.
dvoVaultARN :: Lens' DescribeVaultOutput (Maybe Text)
dvoVaultARN = lens _dvoVaultARN (\ s a -> s{_dvoVaultARN = a})

-- | The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
dvoCreationDate :: Lens' DescribeVaultOutput (Maybe Text)
dvoCreationDate = lens _dvoCreationDate (\ s a -> s{_dvoCreationDate = a})

-- | The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
dvoNumberOfArchives :: Lens' DescribeVaultOutput (Maybe Integer)
dvoNumberOfArchives = lens _dvoNumberOfArchives (\ s a -> s{_dvoNumberOfArchives = a})

instance FromJSON DescribeVaultOutput where
        parseJSON
          = withObject "DescribeVaultOutput"
              (\ x ->
                 DescribeVaultOutput' <$>
                   (x .:? "VaultName") <*> (x .:? "SizeInBytes") <*>
                     (x .:? "LastInventoryDate")
                     <*> (x .:? "VaultARN")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "NumberOfArchives"))

instance Hashable DescribeVaultOutput where

instance NFData DescribeVaultOutput where

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eEncryptionType :: !(Maybe EncryptionType)
  , _eKMSKeyId       :: !(Maybe Text)
  , _eKMSContext     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eEncryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
--
-- * 'eKMSKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
--
-- * 'eKMSContext' - Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
encryption
    :: Encryption
encryption =
  Encryption'
    {_eEncryptionType = Nothing, _eKMSKeyId = Nothing, _eKMSContext = Nothing}


-- | The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
eEncryptionType :: Lens' Encryption (Maybe EncryptionType)
eEncryptionType = lens _eEncryptionType (\ s a -> s{_eEncryptionType = a})

-- | The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
eKMSKeyId :: Lens' Encryption (Maybe Text)
eKMSKeyId = lens _eKMSKeyId (\ s a -> s{_eKMSKeyId = a})

-- | Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
eKMSContext :: Lens' Encryption (Maybe Text)
eKMSContext = lens _eKMSContext (\ s a -> s{_eKMSContext = a})

instance FromJSON Encryption where
        parseJSON
          = withObject "Encryption"
              (\ x ->
                 Encryption' <$>
                   (x .:? "EncryptionType") <*> (x .:? "KMSKeyId") <*>
                     (x .:? "KMSContext"))

instance Hashable Encryption where

instance NFData Encryption where

instance ToJSON Encryption where
        toJSON Encryption'{..}
          = object
              (catMaybes
                 [("EncryptionType" .=) <$> _eEncryptionType,
                  ("KMSKeyId" .=) <$> _eKMSKeyId,
                  ("KMSContext" .=) <$> _eKMSContext])

-- | Contains the description of an Amazon Glacier job.
--
--
--
-- /See:/ 'glacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { _gjdSHA256TreeHash :: !(Maybe Text)
  , _gjdArchiveId :: !(Maybe Text)
  , _gjdSelectParameters :: !(Maybe SelectParameters)
  , _gjdJobId :: !(Maybe Text)
  , _gjdJobOutputPath :: !(Maybe Text)
  , _gjdRetrievalByteRange :: !(Maybe Text)
  , _gjdInventoryRetrievalParameters :: !(Maybe InventoryRetrievalJobDescription)
  , _gjdAction :: !(Maybe ActionCode)
  , _gjdJobDescription :: !(Maybe Text)
  , _gjdSNSTopic :: !(Maybe Text)
  , _gjdStatusMessage :: !(Maybe Text)
  , _gjdVaultARN :: !(Maybe Text)
  , _gjdOutputLocation :: !(Maybe OutputLocation)
  , _gjdTier :: !(Maybe Text)
  , _gjdArchiveSHA256TreeHash :: !(Maybe Text)
  , _gjdCreationDate :: !(Maybe Text)
  , _gjdCompleted :: !(Maybe Bool)
  , _gjdCompletionDate :: !(Maybe Text)
  , _gjdInventorySizeInBytes :: !(Maybe Integer)
  , _gjdArchiveSizeInBytes :: !(Maybe Integer)
  , _gjdStatusCode :: !(Maybe StatusCode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GlacierJobDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjdSHA256TreeHash' - For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null. The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value. If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value. This field is null for the following:     * Archive retrieval jobs that specify a range that is not tree-hash aligned     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@      * Inventory jobs     * Select jobs
--
-- * 'gjdArchiveId' - The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
--
-- * 'gjdSelectParameters' - Contains the parameters used for a select.
--
-- * 'gjdJobId' - An opaque string that identifies an Amazon Glacier job.
--
-- * 'gjdJobOutputPath' - Contains the job output location.
--
-- * 'gjdRetrievalByteRange' - The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
--
-- * 'gjdInventoryRetrievalParameters' - Parameters used for range inventory retrieval.
--
-- * 'gjdAction' - The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
--
-- * 'gjdJobDescription' - The job description provided when initiating the job.
--
-- * 'gjdSNSTopic' - An Amazon SNS topic that receives notification.
--
-- * 'gjdStatusMessage' - A friendly message that describes the job status.
--
-- * 'gjdVaultARN' - The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
--
-- * 'gjdOutputLocation' - Contains the location where the data from the select job is stored.
--
-- * 'gjdTier' - The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- * 'gjdArchiveSHA256TreeHash' - The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
--
-- * 'gjdCreationDate' - The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
--
-- * 'gjdCompleted' - The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
--
-- * 'gjdCompletionDate' - The UTC time that the job request completed. While the job is in progress, the value is null.
--
-- * 'gjdInventorySizeInBytes' - For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
--
-- * 'gjdArchiveSizeInBytes' - For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
--
-- * 'gjdStatusCode' - The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
glacierJobDescription
    :: GlacierJobDescription
glacierJobDescription =
  GlacierJobDescription'
    { _gjdSHA256TreeHash = Nothing
    , _gjdArchiveId = Nothing
    , _gjdSelectParameters = Nothing
    , _gjdJobId = Nothing
    , _gjdJobOutputPath = Nothing
    , _gjdRetrievalByteRange = Nothing
    , _gjdInventoryRetrievalParameters = Nothing
    , _gjdAction = Nothing
    , _gjdJobDescription = Nothing
    , _gjdSNSTopic = Nothing
    , _gjdStatusMessage = Nothing
    , _gjdVaultARN = Nothing
    , _gjdOutputLocation = Nothing
    , _gjdTier = Nothing
    , _gjdArchiveSHA256TreeHash = Nothing
    , _gjdCreationDate = Nothing
    , _gjdCompleted = Nothing
    , _gjdCompletionDate = Nothing
    , _gjdInventorySizeInBytes = Nothing
    , _gjdArchiveSizeInBytes = Nothing
    , _gjdStatusCode = Nothing
    }


-- | For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null. The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value. If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value. This field is null for the following:     * Archive retrieval jobs that specify a range that is not tree-hash aligned     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@      * Inventory jobs     * Select jobs
gjdSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdSHA256TreeHash = lens _gjdSHA256TreeHash (\ s a -> s{_gjdSHA256TreeHash = a})

-- | The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
gjdArchiveId :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveId = lens _gjdArchiveId (\ s a -> s{_gjdArchiveId = a})

-- | Contains the parameters used for a select.
gjdSelectParameters :: Lens' GlacierJobDescription (Maybe SelectParameters)
gjdSelectParameters = lens _gjdSelectParameters (\ s a -> s{_gjdSelectParameters = a})

-- | An opaque string that identifies an Amazon Glacier job.
gjdJobId :: Lens' GlacierJobDescription (Maybe Text)
gjdJobId = lens _gjdJobId (\ s a -> s{_gjdJobId = a})

-- | Contains the job output location.
gjdJobOutputPath :: Lens' GlacierJobDescription (Maybe Text)
gjdJobOutputPath = lens _gjdJobOutputPath (\ s a -> s{_gjdJobOutputPath = a})

-- | The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
gjdRetrievalByteRange :: Lens' GlacierJobDescription (Maybe Text)
gjdRetrievalByteRange = lens _gjdRetrievalByteRange (\ s a -> s{_gjdRetrievalByteRange = a})

-- | Parameters used for range inventory retrieval.
gjdInventoryRetrievalParameters :: Lens' GlacierJobDescription (Maybe InventoryRetrievalJobDescription)
gjdInventoryRetrievalParameters = lens _gjdInventoryRetrievalParameters (\ s a -> s{_gjdInventoryRetrievalParameters = a})

-- | The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
gjdAction :: Lens' GlacierJobDescription (Maybe ActionCode)
gjdAction = lens _gjdAction (\ s a -> s{_gjdAction = a})

-- | The job description provided when initiating the job.
gjdJobDescription :: Lens' GlacierJobDescription (Maybe Text)
gjdJobDescription = lens _gjdJobDescription (\ s a -> s{_gjdJobDescription = a})

-- | An Amazon SNS topic that receives notification.
gjdSNSTopic :: Lens' GlacierJobDescription (Maybe Text)
gjdSNSTopic = lens _gjdSNSTopic (\ s a -> s{_gjdSNSTopic = a})

-- | A friendly message that describes the job status.
gjdStatusMessage :: Lens' GlacierJobDescription (Maybe Text)
gjdStatusMessage = lens _gjdStatusMessage (\ s a -> s{_gjdStatusMessage = a})

-- | The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
gjdVaultARN :: Lens' GlacierJobDescription (Maybe Text)
gjdVaultARN = lens _gjdVaultARN (\ s a -> s{_gjdVaultARN = a})

-- | Contains the location where the data from the select job is stored.
gjdOutputLocation :: Lens' GlacierJobDescription (Maybe OutputLocation)
gjdOutputLocation = lens _gjdOutputLocation (\ s a -> s{_gjdOutputLocation = a})

-- | The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
gjdTier :: Lens' GlacierJobDescription (Maybe Text)
gjdTier = lens _gjdTier (\ s a -> s{_gjdTier = a})

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
gjdArchiveSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveSHA256TreeHash = lens _gjdArchiveSHA256TreeHash (\ s a -> s{_gjdArchiveSHA256TreeHash = a})

-- | The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
gjdCreationDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCreationDate = lens _gjdCreationDate (\ s a -> s{_gjdCreationDate = a})

-- | The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
gjdCompleted :: Lens' GlacierJobDescription (Maybe Bool)
gjdCompleted = lens _gjdCompleted (\ s a -> s{_gjdCompleted = a})

-- | The UTC time that the job request completed. While the job is in progress, the value is null.
gjdCompletionDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCompletionDate = lens _gjdCompletionDate (\ s a -> s{_gjdCompletionDate = a})

-- | For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
gjdInventorySizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdInventorySizeInBytes = lens _gjdInventorySizeInBytes (\ s a -> s{_gjdInventorySizeInBytes = a})

-- | For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
gjdArchiveSizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdArchiveSizeInBytes = lens _gjdArchiveSizeInBytes (\ s a -> s{_gjdArchiveSizeInBytes = a})

-- | The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
gjdStatusCode :: Lens' GlacierJobDescription (Maybe StatusCode)
gjdStatusCode = lens _gjdStatusCode (\ s a -> s{_gjdStatusCode = a})

instance FromJSON GlacierJobDescription where
        parseJSON
          = withObject "GlacierJobDescription"
              (\ x ->
                 GlacierJobDescription' <$>
                   (x .:? "SHA256TreeHash") <*> (x .:? "ArchiveId") <*>
                     (x .:? "SelectParameters")
                     <*> (x .:? "JobId")
                     <*> (x .:? "JobOutputPath")
                     <*> (x .:? "RetrievalByteRange")
                     <*> (x .:? "InventoryRetrievalParameters")
                     <*> (x .:? "Action")
                     <*> (x .:? "JobDescription")
                     <*> (x .:? "SNSTopic")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "VaultARN")
                     <*> (x .:? "OutputLocation")
                     <*> (x .:? "Tier")
                     <*> (x .:? "ArchiveSHA256TreeHash")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Completed")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "InventorySizeInBytes")
                     <*> (x .:? "ArchiveSizeInBytes")
                     <*> (x .:? "StatusCode"))

instance Hashable GlacierJobDescription where

instance NFData GlacierJobDescription where

-- | Contains information about a grant.
--
--
--
-- /See:/ 'grant' smart constructor.
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
-- * 'gGrantee' - The grantee.
grant
    :: Grant
grant = Grant' {_gPermission = Nothing, _gGrantee = Nothing}


-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\ s a -> s{_gPermission = a})

-- | The grantee.
gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\ s a -> s{_gGrantee = a})

instance FromJSON Grant where
        parseJSON
          = withObject "Grant"
              (\ x ->
                 Grant' <$>
                   (x .:? "Permission") <*> (x .:? "Grantee"))

instance Hashable Grant where

instance NFData Grant where

instance ToJSON Grant where
        toJSON Grant'{..}
          = object
              (catMaybes
                 [("Permission" .=) <$> _gPermission,
                  ("Grantee" .=) <$> _gGrantee])

-- | Contains information about the grantee.
--
--
--
-- /See:/ 'grantee' smart constructor.
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

instance FromJSON Grantee where
        parseJSON
          = withObject "Grantee"
              (\ x ->
                 Grantee' <$>
                   (x .:? "URI") <*> (x .:? "EmailAddress") <*>
                     (x .:? "DisplayName")
                     <*> (x .:? "ID")
                     <*> (x .: "Type"))

instance Hashable Grantee where

instance NFData Grantee where

instance ToJSON Grantee where
        toJSON Grantee'{..}
          = object
              (catMaybes
                 [("URI" .=) <$> _gURI,
                  ("EmailAddress" .=) <$> _gEmailAddress,
                  ("DisplayName" .=) <$> _gDisplayName,
                  ("ID" .=) <$> _gId, Just ("Type" .= _gType)])

-- | Describes how the archive is serialized.
--
--
--
-- /See:/ 'inputSerialization' smart constructor.
newtype InputSerialization = InputSerialization'
  { _isCsv :: Maybe CSVInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isCsv' - Describes the serialization of a CSV-encoded object.
inputSerialization
    :: InputSerialization
inputSerialization = InputSerialization' {_isCsv = Nothing}


-- | Describes the serialization of a CSV-encoded object.
isCsv :: Lens' InputSerialization (Maybe CSVInput)
isCsv = lens _isCsv (\ s a -> s{_isCsv = a})

instance FromJSON InputSerialization where
        parseJSON
          = withObject "InputSerialization"
              (\ x -> InputSerialization' <$> (x .:? "csv"))

instance Hashable InputSerialization where

instance NFData InputSerialization where

instance ToJSON InputSerialization where
        toJSON InputSerialization'{..}
          = object (catMaybes [("csv" .=) <$> _isCsv])

-- | Describes the options for a range inventory retrieval job.
--
--
--
-- /See:/ 'inventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { _irjdFormat    :: !(Maybe Text)
  , _irjdEndDate   :: !(Maybe Text)
  , _irjdStartDate :: !(Maybe Text)
  , _irjdMarker    :: !(Maybe Text)
  , _irjdLimit     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryRetrievalJobDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irjdFormat' - The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
--
-- * 'irjdEndDate' - The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjdStartDate' - The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjdMarker' - An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
--
-- * 'irjdLimit' - The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
inventoryRetrievalJobDescription
    :: InventoryRetrievalJobDescription
inventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { _irjdFormat = Nothing
    , _irjdEndDate = Nothing
    , _irjdStartDate = Nothing
    , _irjdMarker = Nothing
    , _irjdLimit = Nothing
    }


-- | The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
irjdFormat :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdFormat = lens _irjdFormat (\ s a -> s{_irjdFormat = a})

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjdEndDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdEndDate = lens _irjdEndDate (\ s a -> s{_irjdEndDate = a})

-- | The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjdStartDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdStartDate = lens _irjdStartDate (\ s a -> s{_irjdStartDate = a})

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
irjdMarker :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdMarker = lens _irjdMarker (\ s a -> s{_irjdMarker = a})

-- | The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
irjdLimit :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdLimit = lens _irjdLimit (\ s a -> s{_irjdLimit = a})

instance FromJSON InventoryRetrievalJobDescription
         where
        parseJSON
          = withObject "InventoryRetrievalJobDescription"
              (\ x ->
                 InventoryRetrievalJobDescription' <$>
                   (x .:? "Format") <*> (x .:? "EndDate") <*>
                     (x .:? "StartDate")
                     <*> (x .:? "Marker")
                     <*> (x .:? "Limit"))

instance Hashable InventoryRetrievalJobDescription
         where

instance NFData InventoryRetrievalJobDescription
         where

-- | Provides options for specifying a range inventory retrieval job.
--
--
--
-- /See:/ 'inventoryRetrievalJobInput' smart constructor.
data InventoryRetrievalJobInput = InventoryRetrievalJobInput'
  { _irjiEndDate   :: !(Maybe Text)
  , _irjiStartDate :: !(Maybe Text)
  , _irjiMarker    :: !(Maybe Text)
  , _irjiLimit     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InventoryRetrievalJobInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irjiEndDate' - The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjiStartDate' - The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjiMarker' - An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
--
-- * 'irjiLimit' - Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
inventoryRetrievalJobInput
    :: InventoryRetrievalJobInput
inventoryRetrievalJobInput =
  InventoryRetrievalJobInput'
    { _irjiEndDate = Nothing
    , _irjiStartDate = Nothing
    , _irjiMarker = Nothing
    , _irjiLimit = Nothing
    }


-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjiEndDate :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiEndDate = lens _irjiEndDate (\ s a -> s{_irjiEndDate = a})

-- | The start of the date range in UTC for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjiStartDate :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiStartDate = lens _irjiStartDate (\ s a -> s{_irjiStartDate = a})

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ .
irjiMarker :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiMarker = lens _irjiMarker (\ s a -> s{_irjiMarker = a})

-- | Specifies the maximum number of inventory items returned per vault inventory retrieval request. Valid values are greater than or equal to 1.
irjiLimit :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiLimit = lens _irjiLimit (\ s a -> s{_irjiLimit = a})

instance Hashable InventoryRetrievalJobInput where

instance NFData InventoryRetrievalJobInput where

instance ToJSON InventoryRetrievalJobInput where
        toJSON InventoryRetrievalJobInput'{..}
          = object
              (catMaybes
                 [("EndDate" .=) <$> _irjiEndDate,
                  ("StartDate" .=) <$> _irjiStartDate,
                  ("Marker" .=) <$> _irjiMarker,
                  ("Limit" .=) <$> _irjiLimit])

-- | Provides options for defining a job.
--
--
--
-- /See:/ 'jobParameters' smart constructor.
data JobParameters = JobParameters'
  { _jpArchiveId                    :: !(Maybe Text)
  , _jpSelectParameters             :: !(Maybe SelectParameters)
  , _jpFormat                       :: !(Maybe Text)
  , _jpRetrievalByteRange           :: !(Maybe Text)
  , _jpInventoryRetrievalParameters :: !(Maybe InventoryRetrievalJobInput)
  , _jpSNSTopic                     :: !(Maybe Text)
  , _jpOutputLocation               :: !(Maybe OutputLocation)
  , _jpTier                         :: !(Maybe Text)
  , _jpType                         :: !(Maybe Text)
  , _jpDescription                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jpArchiveId' - The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
--
-- * 'jpSelectParameters' - Contains the parameters that define a job.
--
-- * 'jpFormat' - When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
--
-- * 'jpRetrievalByteRange' - The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.  An error occurs if you specify this field for an inventory retrieval job request.
--
-- * 'jpInventoryRetrievalParameters' - Input parameters used for range inventory retrieval.
--
-- * 'jpSNSTopic' - The Amazon SNS topic ARN to which Amazon Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
--
-- * 'jpOutputLocation' - Contains information about the location where the select job results are stored.
--
-- * 'jpTier' - The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- * 'jpType' - The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
--
-- * 'jpDescription' - The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
jobParameters
    :: JobParameters
jobParameters =
  JobParameters'
    { _jpArchiveId = Nothing
    , _jpSelectParameters = Nothing
    , _jpFormat = Nothing
    , _jpRetrievalByteRange = Nothing
    , _jpInventoryRetrievalParameters = Nothing
    , _jpSNSTopic = Nothing
    , _jpOutputLocation = Nothing
    , _jpTier = Nothing
    , _jpType = Nothing
    , _jpDescription = Nothing
    }


-- | The ID of the archive that you want to retrieve. This field is required only if @Type@ is set to @select@ or @archive-retrieval@ code>. An error occurs if you specify this request parameter for an inventory retrieval job request.
jpArchiveId :: Lens' JobParameters (Maybe Text)
jpArchiveId = lens _jpArchiveId (\ s a -> s{_jpArchiveId = a})

-- | Contains the parameters that define a job.
jpSelectParameters :: Lens' JobParameters (Maybe SelectParameters)
jpSelectParameters = lens _jpSelectParameters (\ s a -> s{_jpSelectParameters = a})

-- | When initiating a job to retrieve a vault inventory, you can optionally add this parameter to your request to specify the output format. If you are initiating an inventory job and do not specify a Format field, JSON is the default format. Valid values are "CSV" and "JSON".
jpFormat :: Lens' JobParameters (Maybe Text)
jpFormat = lens _jpFormat (\ s a -> s{_jpFormat = a})

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/ -/EndByteValue/ " If not specified, the whole archive is retrieved. If specified, the byte range must be megabyte (1024*1024) aligned which means that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be divisible by 1 MB or be the end of the archive specified as the archive byte size value minus 1. If RetrievalByteRange is not megabyte aligned, this operation returns a 400 response.  An error occurs if you specify this field for an inventory retrieval job request.
jpRetrievalByteRange :: Lens' JobParameters (Maybe Text)
jpRetrievalByteRange = lens _jpRetrievalByteRange (\ s a -> s{_jpRetrievalByteRange = a})

-- | Input parameters used for range inventory retrieval.
jpInventoryRetrievalParameters :: Lens' JobParameters (Maybe InventoryRetrievalJobInput)
jpInventoryRetrievalParameters = lens _jpInventoryRetrievalParameters (\ s a -> s{_jpInventoryRetrievalParameters = a})

-- | The Amazon SNS topic ARN to which Amazon Glacier sends a notification when the job is completed and the output is ready for you to download. The specified topic publishes the notification to its subscribers. The SNS topic must exist.
jpSNSTopic :: Lens' JobParameters (Maybe Text)
jpSNSTopic = lens _jpSNSTopic (\ s a -> s{_jpSNSTopic = a})

-- | Contains information about the location where the select job results are stored.
jpOutputLocation :: Lens' JobParameters (Maybe OutputLocation)
jpOutputLocation = lens _jpOutputLocation (\ s a -> s{_jpOutputLocation = a})

-- | The tier to use for a select or an archive retrieval job. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
jpTier :: Lens' JobParameters (Maybe Text)
jpTier = lens _jpTier (\ s a -> s{_jpTier = a})

-- | The job type. You can initiate a job to perform a select query on an archive, retrieve an archive, or get an inventory of a vault. Valid values are "select", "archive-retrieval" and "inventory-retrieval".
jpType :: Lens' JobParameters (Maybe Text)
jpType = lens _jpType (\ s a -> s{_jpType = a})

-- | The optional description for the job. The description must be less than or equal to 1,024 bytes. The allowable characters are 7-bit ASCII without control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E hexadecimal.
jpDescription :: Lens' JobParameters (Maybe Text)
jpDescription = lens _jpDescription (\ s a -> s{_jpDescription = a})

instance Hashable JobParameters where

instance NFData JobParameters where

instance ToJSON JobParameters where
        toJSON JobParameters'{..}
          = object
              (catMaybes
                 [("ArchiveId" .=) <$> _jpArchiveId,
                  ("SelectParameters" .=) <$> _jpSelectParameters,
                  ("Format" .=) <$> _jpFormat,
                  ("RetrievalByteRange" .=) <$> _jpRetrievalByteRange,
                  ("InventoryRetrievalParameters" .=) <$>
                    _jpInventoryRetrievalParameters,
                  ("SNSTopic" .=) <$> _jpSNSTopic,
                  ("OutputLocation" .=) <$> _jpOutputLocation,
                  ("Tier" .=) <$> _jpTier, ("Type" .=) <$> _jpType,
                  ("Description" .=) <$> _jpDescription])

-- | Contains information about the location where the select job results are stored.
--
--
--
-- /See:/ 'outputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { _olS3 :: Maybe S3Location
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olS3' - Describes an S3 location that will receive the results of the job request.
outputLocation
    :: OutputLocation
outputLocation = OutputLocation' {_olS3 = Nothing}


-- | Describes an S3 location that will receive the results of the job request.
olS3 :: Lens' OutputLocation (Maybe S3Location)
olS3 = lens _olS3 (\ s a -> s{_olS3 = a})

instance FromJSON OutputLocation where
        parseJSON
          = withObject "OutputLocation"
              (\ x -> OutputLocation' <$> (x .:? "S3"))

instance Hashable OutputLocation where

instance NFData OutputLocation where

instance ToJSON OutputLocation where
        toJSON OutputLocation'{..}
          = object (catMaybes [("S3" .=) <$> _olS3])

-- | Describes how the select output is serialized.
--
--
--
-- /See:/ 'outputSerialization' smart constructor.
newtype OutputSerialization = OutputSerialization'
  { _osCsv :: Maybe CSVOutput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osCsv' - Describes the serialization of CSV-encoded query results.
outputSerialization
    :: OutputSerialization
outputSerialization = OutputSerialization' {_osCsv = Nothing}


-- | Describes the serialization of CSV-encoded query results.
osCsv :: Lens' OutputSerialization (Maybe CSVOutput)
osCsv = lens _osCsv (\ s a -> s{_osCsv = a})

instance FromJSON OutputSerialization where
        parseJSON
          = withObject "OutputSerialization"
              (\ x -> OutputSerialization' <$> (x .:? "csv"))

instance Hashable OutputSerialization where

instance NFData OutputSerialization where

instance ToJSON OutputSerialization where
        toJSON OutputSerialization'{..}
          = object (catMaybes [("csv" .=) <$> _osCsv])

-- | A list of the part sizes of the multipart upload.
--
--
--
-- /See:/ 'partListElement' smart constructor.
data PartListElement = PartListElement'
  { _pleSHA256TreeHash :: !(Maybe Text)
  , _pleRangeInBytes   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PartListElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pleSHA256TreeHash' - The SHA256 tree hash value that Amazon Glacier calculated for the part. This field is never @null@ .
--
-- * 'pleRangeInBytes' - The byte range of a part, inclusive of the upper value of the range.
partListElement
    :: PartListElement
partListElement =
  PartListElement' {_pleSHA256TreeHash = Nothing, _pleRangeInBytes = Nothing}


-- | The SHA256 tree hash value that Amazon Glacier calculated for the part. This field is never @null@ .
pleSHA256TreeHash :: Lens' PartListElement (Maybe Text)
pleSHA256TreeHash = lens _pleSHA256TreeHash (\ s a -> s{_pleSHA256TreeHash = a})

-- | The byte range of a part, inclusive of the upper value of the range.
pleRangeInBytes :: Lens' PartListElement (Maybe Text)
pleRangeInBytes = lens _pleRangeInBytes (\ s a -> s{_pleRangeInBytes = a})

instance FromJSON PartListElement where
        parseJSON
          = withObject "PartListElement"
              (\ x ->
                 PartListElement' <$>
                   (x .:? "SHA256TreeHash") <*> (x .:? "RangeInBytes"))

instance Hashable PartListElement where

instance NFData PartListElement where

-- | The definition for a provisioned capacity unit.
--
--
--
-- /See:/ 'provisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { _pcdCapacityId     :: !(Maybe Text)
  , _pcdStartDate      :: !(Maybe Text)
  , _pcdExpirationDate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedCapacityDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdCapacityId' - The ID that identifies the provisioned capacity unit.
--
-- * 'pcdStartDate' - The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
--
-- * 'pcdExpirationDate' - The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
provisionedCapacityDescription
    :: ProvisionedCapacityDescription
provisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { _pcdCapacityId = Nothing
    , _pcdStartDate = Nothing
    , _pcdExpirationDate = Nothing
    }


-- | The ID that identifies the provisioned capacity unit.
pcdCapacityId :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdCapacityId = lens _pcdCapacityId (\ s a -> s{_pcdCapacityId = a})

-- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
pcdStartDate :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdStartDate = lens _pcdStartDate (\ s a -> s{_pcdStartDate = a})

-- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
pcdExpirationDate :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdExpirationDate = lens _pcdExpirationDate (\ s a -> s{_pcdExpirationDate = a})

instance FromJSON ProvisionedCapacityDescription
         where
        parseJSON
          = withObject "ProvisionedCapacityDescription"
              (\ x ->
                 ProvisionedCapacityDescription' <$>
                   (x .:? "CapacityId") <*> (x .:? "StartDate") <*>
                     (x .:? "ExpirationDate"))

instance Hashable ProvisionedCapacityDescription
         where

instance NFData ProvisionedCapacityDescription where

-- | Contains information about the location in Amazon S3 where the select job results are stored.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slCannedACL         :: !(Maybe CannedACL)
  , _slPrefix            :: !(Maybe Text)
  , _slBucketName        :: !(Maybe Text)
  , _slAccessControlList :: !(Maybe [Grant])
  , _slUserMetadata      :: !(Maybe (Map Text Text))
  , _slEncryption        :: !(Maybe Encryption)
  , _slStorageClass      :: !(Maybe StorageClass)
  , _slTagging           :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCannedACL' - The canned access control list (ACL) to apply to the job results.
--
-- * 'slPrefix' - The prefix that is prepended to the results for this request.
--
-- * 'slBucketName' - The name of the Amazon S3 bucket where the job results are stored.
--
-- * 'slAccessControlList' - A list of grants that control access to the staged results.
--
-- * 'slUserMetadata' - A map of metadata to store with the job results in Amazon S3.
--
-- * 'slEncryption' - Contains information about the encryption used to store the job results in Amazon S3.
--
-- * 'slStorageClass' - The storage class used to store the job results.
--
-- * 'slTagging' - The tag-set that is applied to the job results.
s3Location
    :: S3Location
s3Location =
  S3Location'
    { _slCannedACL = Nothing
    , _slPrefix = Nothing
    , _slBucketName = Nothing
    , _slAccessControlList = Nothing
    , _slUserMetadata = Nothing
    , _slEncryption = Nothing
    , _slStorageClass = Nothing
    , _slTagging = Nothing
    }


-- | The canned access control list (ACL) to apply to the job results.
slCannedACL :: Lens' S3Location (Maybe CannedACL)
slCannedACL = lens _slCannedACL (\ s a -> s{_slCannedACL = a})

-- | The prefix that is prepended to the results for this request.
slPrefix :: Lens' S3Location (Maybe Text)
slPrefix = lens _slPrefix (\ s a -> s{_slPrefix = a})

-- | The name of the Amazon S3 bucket where the job results are stored.
slBucketName :: Lens' S3Location (Maybe Text)
slBucketName = lens _slBucketName (\ s a -> s{_slBucketName = a})

-- | A list of grants that control access to the staged results.
slAccessControlList :: Lens' S3Location [Grant]
slAccessControlList = lens _slAccessControlList (\ s a -> s{_slAccessControlList = a}) . _Default . _Coerce

-- | A map of metadata to store with the job results in Amazon S3.
slUserMetadata :: Lens' S3Location (HashMap Text Text)
slUserMetadata = lens _slUserMetadata (\ s a -> s{_slUserMetadata = a}) . _Default . _Map

-- | Contains information about the encryption used to store the job results in Amazon S3.
slEncryption :: Lens' S3Location (Maybe Encryption)
slEncryption = lens _slEncryption (\ s a -> s{_slEncryption = a})

-- | The storage class used to store the job results.
slStorageClass :: Lens' S3Location (Maybe StorageClass)
slStorageClass = lens _slStorageClass (\ s a -> s{_slStorageClass = a})

-- | The tag-set that is applied to the job results.
slTagging :: Lens' S3Location (HashMap Text Text)
slTagging = lens _slTagging (\ s a -> s{_slTagging = a}) . _Default . _Map

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "CannedACL") <*> (x .:? "Prefix") <*>
                     (x .:? "BucketName")
                     <*> (x .:? "AccessControlList" .!= mempty)
                     <*> (x .:? "UserMetadata" .!= mempty)
                     <*> (x .:? "Encryption")
                     <*> (x .:? "StorageClass")
                     <*> (x .:? "Tagging" .!= mempty))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("CannedACL" .=) <$> _slCannedACL,
                  ("Prefix" .=) <$> _slPrefix,
                  ("BucketName" .=) <$> _slBucketName,
                  ("AccessControlList" .=) <$> _slAccessControlList,
                  ("UserMetadata" .=) <$> _slUserMetadata,
                  ("Encryption" .=) <$> _slEncryption,
                  ("StorageClass" .=) <$> _slStorageClass,
                  ("Tagging" .=) <$> _slTagging])

-- | Contains information about the parameters used for a select.
--
--
--
-- /See:/ 'selectParameters' smart constructor.
data SelectParameters = SelectParameters'
  { _spExpressionType      :: !(Maybe ExpressionType)
  , _spOutputSerialization :: !(Maybe OutputSerialization)
  , _spExpression          :: !(Maybe Text)
  , _spInputSerialization  :: !(Maybe InputSerialization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spExpressionType' - The type of the provided expression, for example @SQL@ .
--
-- * 'spOutputSerialization' - Describes how the results of the select job are serialized.
--
-- * 'spExpression' - The expression that is used to select the object.
--
-- * 'spInputSerialization' - Describes the serialization format of the object.
selectParameters
    :: SelectParameters
selectParameters =
  SelectParameters'
    { _spExpressionType = Nothing
    , _spOutputSerialization = Nothing
    , _spExpression = Nothing
    , _spInputSerialization = Nothing
    }


-- | The type of the provided expression, for example @SQL@ .
spExpressionType :: Lens' SelectParameters (Maybe ExpressionType)
spExpressionType = lens _spExpressionType (\ s a -> s{_spExpressionType = a})

-- | Describes how the results of the select job are serialized.
spOutputSerialization :: Lens' SelectParameters (Maybe OutputSerialization)
spOutputSerialization = lens _spOutputSerialization (\ s a -> s{_spOutputSerialization = a})

-- | The expression that is used to select the object.
spExpression :: Lens' SelectParameters (Maybe Text)
spExpression = lens _spExpression (\ s a -> s{_spExpression = a})

-- | Describes the serialization format of the object.
spInputSerialization :: Lens' SelectParameters (Maybe InputSerialization)
spInputSerialization = lens _spInputSerialization (\ s a -> s{_spInputSerialization = a})

instance FromJSON SelectParameters where
        parseJSON
          = withObject "SelectParameters"
              (\ x ->
                 SelectParameters' <$>
                   (x .:? "ExpressionType") <*>
                     (x .:? "OutputSerialization")
                     <*> (x .:? "Expression")
                     <*> (x .:? "InputSerialization"))

instance Hashable SelectParameters where

instance NFData SelectParameters where

instance ToJSON SelectParameters where
        toJSON SelectParameters'{..}
          = object
              (catMaybes
                 [("ExpressionType" .=) <$> _spExpressionType,
                  ("OutputSerialization" .=) <$>
                    _spOutputSerialization,
                  ("Expression" .=) <$> _spExpression,
                  ("InputSerialization" .=) <$> _spInputSerialization])

-- | A list of in-progress multipart uploads for a vault.
--
--
--
-- /See:/ 'uploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { _uleMultipartUploadId  :: !(Maybe Text)
  , _ulePartSizeInBytes    :: !(Maybe Integer)
  , _uleArchiveDescription :: !(Maybe Text)
  , _uleVaultARN           :: !(Maybe Text)
  , _uleCreationDate       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadListElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uleMultipartUploadId' - The ID of a multipart upload.
--
-- * 'ulePartSizeInBytes' - The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
--
-- * 'uleArchiveDescription' - The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- * 'uleVaultARN' - The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- * 'uleCreationDate' - The UTC time at which the multipart upload was initiated.
uploadListElement
    :: UploadListElement
uploadListElement =
  UploadListElement'
    { _uleMultipartUploadId = Nothing
    , _ulePartSizeInBytes = Nothing
    , _uleArchiveDescription = Nothing
    , _uleVaultARN = Nothing
    , _uleCreationDate = Nothing
    }


-- | The ID of a multipart upload.
uleMultipartUploadId :: Lens' UploadListElement (Maybe Text)
uleMultipartUploadId = lens _uleMultipartUploadId (\ s a -> s{_uleMultipartUploadId = a})

-- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
ulePartSizeInBytes :: Lens' UploadListElement (Maybe Integer)
ulePartSizeInBytes = lens _ulePartSizeInBytes (\ s a -> s{_ulePartSizeInBytes = a})

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
uleArchiveDescription :: Lens' UploadListElement (Maybe Text)
uleArchiveDescription = lens _uleArchiveDescription (\ s a -> s{_uleArchiveDescription = a})

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uleVaultARN :: Lens' UploadListElement (Maybe Text)
uleVaultARN = lens _uleVaultARN (\ s a -> s{_uleVaultARN = a})

-- | The UTC time at which the multipart upload was initiated.
uleCreationDate :: Lens' UploadListElement (Maybe Text)
uleCreationDate = lens _uleCreationDate (\ s a -> s{_uleCreationDate = a})

instance FromJSON UploadListElement where
        parseJSON
          = withObject "UploadListElement"
              (\ x ->
                 UploadListElement' <$>
                   (x .:? "MultipartUploadId") <*>
                     (x .:? "PartSizeInBytes")
                     <*> (x .:? "ArchiveDescription")
                     <*> (x .:? "VaultARN")
                     <*> (x .:? "CreationDate"))

instance Hashable UploadListElement where

instance NFData UploadListElement where

-- | Contains the vault access policy.
--
--
--
-- /See:/ 'vaultAccessPolicy' smart constructor.
newtype VaultAccessPolicy = VaultAccessPolicy'
  { _vapPolicy :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vapPolicy' - The vault access policy.
vaultAccessPolicy
    :: VaultAccessPolicy
vaultAccessPolicy = VaultAccessPolicy' {_vapPolicy = Nothing}


-- | The vault access policy.
vapPolicy :: Lens' VaultAccessPolicy (Maybe Text)
vapPolicy = lens _vapPolicy (\ s a -> s{_vapPolicy = a})

instance FromJSON VaultAccessPolicy where
        parseJSON
          = withObject "VaultAccessPolicy"
              (\ x -> VaultAccessPolicy' <$> (x .:? "Policy"))

instance Hashable VaultAccessPolicy where

instance NFData VaultAccessPolicy where

instance ToJSON VaultAccessPolicy where
        toJSON VaultAccessPolicy'{..}
          = object (catMaybes [("Policy" .=) <$> _vapPolicy])

-- | Contains the vault lock policy.
--
--
--
-- /See:/ 'vaultLockPolicy' smart constructor.
newtype VaultLockPolicy = VaultLockPolicy'
  { _vlpPolicy :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VaultLockPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vlpPolicy' - The vault lock policy.
vaultLockPolicy
    :: VaultLockPolicy
vaultLockPolicy = VaultLockPolicy' {_vlpPolicy = Nothing}


-- | The vault lock policy.
vlpPolicy :: Lens' VaultLockPolicy (Maybe Text)
vlpPolicy = lens _vlpPolicy (\ s a -> s{_vlpPolicy = a})

instance Hashable VaultLockPolicy where

instance NFData VaultLockPolicy where

instance ToJSON VaultLockPolicy where
        toJSON VaultLockPolicy'{..}
          = object (catMaybes [("Policy" .=) <$> _vlpPolicy])

-- | Represents a vault's notification configuration.
--
--
--
-- /See:/ 'vaultNotificationConfig' smart constructor.
data VaultNotificationConfig = VaultNotificationConfig'
  { _vncSNSTopic :: !(Maybe Text)
  , _vncEvents   :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VaultNotificationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vncSNSTopic' - The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
--
-- * 'vncEvents' - A list of one or more events for which Amazon Glacier will send a notification to the specified Amazon SNS topic.
vaultNotificationConfig
    :: VaultNotificationConfig
vaultNotificationConfig =
  VaultNotificationConfig' {_vncSNSTopic = Nothing, _vncEvents = Nothing}


-- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource Name (ARN).
vncSNSTopic :: Lens' VaultNotificationConfig (Maybe Text)
vncSNSTopic = lens _vncSNSTopic (\ s a -> s{_vncSNSTopic = a})

-- | A list of one or more events for which Amazon Glacier will send a notification to the specified Amazon SNS topic.
vncEvents :: Lens' VaultNotificationConfig [Text]
vncEvents = lens _vncEvents (\ s a -> s{_vncEvents = a}) . _Default . _Coerce

instance FromJSON VaultNotificationConfig where
        parseJSON
          = withObject "VaultNotificationConfig"
              (\ x ->
                 VaultNotificationConfig' <$>
                   (x .:? "SNSTopic") <*> (x .:? "Events" .!= mempty))

instance Hashable VaultNotificationConfig where

instance NFData VaultNotificationConfig where

instance ToJSON VaultNotificationConfig where
        toJSON VaultNotificationConfig'{..}
          = object
              (catMaybes
                 [("SNSTopic" .=) <$> _vncSNSTopic,
                  ("Events" .=) <$> _vncEvents])

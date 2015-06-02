{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Glacier.Types
    (
    -- * Service
      Glacier
    -- ** Error
    , JSONError

    -- * ArchiveCreationOutput
    , ArchiveCreationOutput
    , archiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

    -- * UploadListElement
    , UploadListElement
    , uploadListElement
    , uleArchiveDescription
    , uleCreationDate
    , uleMultipartUploadId
    , ulePartSizeInBytes
    , uleVaultARN

    -- * InventoryRetrievalJobDescription
    , InventoryRetrievalJobDescription
    , inventoryRetrievalJobDescription
    , irjdEndDate
    , irjdFormat
    , irjdLimit
    , irjdMarker
    , irjdStartDate

    -- * JobParameters
    , JobParameters
    , jobParameters
    , jpArchiveId
    , jpDescription
    , jpFormat
    , jpInventoryRetrievalParameters
    , jpRetrievalByteRange
    , jpSNSTopic
    , jpType

    -- * DescribeVaultOutput
    , DescribeVaultOutput
    , describeVaultOutput
    , dvoCreationDate
    , dvoLastInventoryDate
    , dvoNumberOfArchives
    , dvoSizeInBytes
    , dvoVaultARN
    , dvoVaultName

    -- * DataRetrievalRule
    , DataRetrievalRule
    , dataRetrievalRule
    , drrBytesPerHour
    , drrStrategy

    -- * ActionCode
    , ActionCode (..)

    -- * VaultNotificationConfig
    , VaultNotificationConfig
    , vaultNotificationConfig
    , vncEvents
    , vncSNSTopic

    -- * InventoryRetrievalJobInput
    , InventoryRetrievalJobInput
    , inventoryRetrievalJobInput
    , irjiEndDate
    , irjiLimit
    , irjiMarker
    , irjiStartDate

    -- * PartListElement
    , PartListElement
    , partListElement
    , pleRangeInBytes
    , pleSHA256TreeHash

    -- * DataRetrievalPolicy
    , DataRetrievalPolicy
    , dataRetrievalPolicy
    , drpRules

    -- * GlacierJobDescription
    , GlacierJobDescription
    , glacierJobDescription
    , gjdAction
    , gjdArchiveId
    , gjdArchiveSHA256TreeHash
    , gjdArchiveSizeInBytes
    , gjdCompleted
    , gjdCompletionDate
    , gjdCreationDate
    , gjdInventoryRetrievalParameters
    , gjdInventorySizeInBytes
    , gjdJobDescription
    , gjdJobId
    , gjdRetrievalByteRange
    , gjdSHA256TreeHash
    , gjdSNSTopic
    , gjdStatusCode
    , gjdStatusMessage
    , gjdVaultARN

    -- * VaultAccessPolicy
    , VaultAccessPolicy
    , vaultAccessPolicy
    , vapPolicy

    -- * StatusCode
    , StatusCode (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-06-01@ of the Amazon Glacier service.
data Glacier

instance AWSService Glacier where
    type Sg Glacier = V4
    type Er Glacier = JSONError

    service = service'
      where
        service' :: Service Glacier
        service' = Service
            { _svcAbbrev       = "Glacier"
            , _svcPrefix       = "glacier"
            , _svcVersion      = "2012-06-01"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry Glacier
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && (Just "ThrottlingException") == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data ArchiveCreationOutput = ArchiveCreationOutput
    { _acoArchiveId :: Maybe Text
    , _acoChecksum  :: Maybe Text
    , _acoLocation  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ArchiveCreationOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoArchiveId' @::@ 'Maybe' 'Text'
--
-- * 'acoChecksum' @::@ 'Maybe' 'Text'
--
-- * 'acoLocation' @::@ 'Maybe' 'Text'
--
archiveCreationOutput :: ArchiveCreationOutput
archiveCreationOutput = ArchiveCreationOutput
    { _acoLocation  = Nothing
    , _acoChecksum  = Nothing
    , _acoArchiveId = Nothing
    }

-- | The ID of the archive. This value is also included as part of the location.
acoArchiveId :: Lens' ArchiveCreationOutput (Maybe Text)
acoArchiveId = lens _acoArchiveId (\s a -> s { _acoArchiveId = a })

-- | The checksum of the archive computed by Amazon Glacier.
acoChecksum :: Lens' ArchiveCreationOutput (Maybe Text)
acoChecksum = lens _acoChecksum (\s a -> s { _acoChecksum = a })

-- | The relative URI path of the newly added archive resource.
acoLocation :: Lens' ArchiveCreationOutput (Maybe Text)
acoLocation = lens _acoLocation (\s a -> s { _acoLocation = a })

instance FromJSON ArchiveCreationOutput where
    parseJSON = withObject "ArchiveCreationOutput" $ \o -> ArchiveCreationOutput
        <$> o .:? "x-amz-archive-id"
        <*> o .:? "x-amz-sha256-tree-hash"
        <*> o .:? "Location"

instance ToJSON ArchiveCreationOutput where
    toJSON = const (toJSON Empty)

data UploadListElement = UploadListElement
    { _uleArchiveDescription :: Maybe Text
    , _uleCreationDate       :: Maybe Text
    , _uleMultipartUploadId  :: Maybe Text
    , _ulePartSizeInBytes    :: Maybe Integer
    , _uleVaultARN           :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UploadListElement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uleArchiveDescription' @::@ 'Maybe' 'Text'
--
-- * 'uleCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'uleMultipartUploadId' @::@ 'Maybe' 'Text'
--
-- * 'ulePartSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'uleVaultARN' @::@ 'Maybe' 'Text'
--
uploadListElement :: UploadListElement
uploadListElement = UploadListElement
    { _uleMultipartUploadId  = Nothing
    , _uleVaultARN           = Nothing
    , _uleArchiveDescription = Nothing
    , _ulePartSizeInBytes    = Nothing
    , _uleCreationDate       = Nothing
    }

-- | The description of the archive that was specified in the Initiate Multipart
-- Upload request.
uleArchiveDescription :: Lens' UploadListElement (Maybe Text)
uleArchiveDescription =
    lens _uleArchiveDescription (\s a -> s { _uleArchiveDescription = a })

-- | The UTC time at which the multipart upload was initiated.
uleCreationDate :: Lens' UploadListElement (Maybe Text)
uleCreationDate = lens _uleCreationDate (\s a -> s { _uleCreationDate = a })

-- | The ID of a multipart upload.
uleMultipartUploadId :: Lens' UploadListElement (Maybe Text)
uleMultipartUploadId =
    lens _uleMultipartUploadId (\s a -> s { _uleMultipartUploadId = a })

-- | The part size, in bytes, specified in the Initiate Multipart Upload request.
-- This is the size of all the parts in the upload except the last part, which
-- may be smaller than this size.
ulePartSizeInBytes :: Lens' UploadListElement (Maybe Integer)
ulePartSizeInBytes =
    lens _ulePartSizeInBytes (\s a -> s { _ulePartSizeInBytes = a })

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
uleVaultARN :: Lens' UploadListElement (Maybe Text)
uleVaultARN = lens _uleVaultARN (\s a -> s { _uleVaultARN = a })

instance FromJSON UploadListElement where
    parseJSON = withObject "UploadListElement" $ \o -> UploadListElement
        <$> o .:? "ArchiveDescription"
        <*> o .:? "CreationDate"
        <*> o .:? "MultipartUploadId"
        <*> o .:? "PartSizeInBytes"
        <*> o .:? "VaultARN"

instance ToJSON UploadListElement where
    toJSON UploadListElement{..} = object
        [ "MultipartUploadId"  .= _uleMultipartUploadId
        , "VaultARN"           .= _uleVaultARN
        , "ArchiveDescription" .= _uleArchiveDescription
        , "PartSizeInBytes"    .= _ulePartSizeInBytes
        , "CreationDate"       .= _uleCreationDate
        ]

data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription
    { _irjdEndDate   :: Maybe Text
    , _irjdFormat    :: Maybe Text
    , _irjdLimit     :: Maybe Text
    , _irjdMarker    :: Maybe Text
    , _irjdStartDate :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InventoryRetrievalJobDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irjdEndDate' @::@ 'Maybe' 'Text'
--
-- * 'irjdFormat' @::@ 'Maybe' 'Text'
--
-- * 'irjdLimit' @::@ 'Maybe' 'Text'
--
-- * 'irjdMarker' @::@ 'Maybe' 'Text'
--
-- * 'irjdStartDate' @::@ 'Maybe' 'Text'
--
inventoryRetrievalJobDescription :: InventoryRetrievalJobDescription
inventoryRetrievalJobDescription = InventoryRetrievalJobDescription
    { _irjdFormat    = Nothing
    , _irjdStartDate = Nothing
    , _irjdEndDate   = Nothing
    , _irjdLimit     = Nothing
    , _irjdMarker    = Nothing
    }

-- | The end of the date range in UTC for vault inventory retrieval that includes
-- archives created before this date. A string representation of ISO 8601 date
-- format, for example, 2013-03-20T17:03:43Z.
irjdEndDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdEndDate = lens _irjdEndDate (\s a -> s { _irjdEndDate = a })

-- | The output format for the vault inventory list, which is set by the InitiateJob
-- request when initiating a job to retrieve a vault inventory. Valid values
-- are "CSV" and "JSON".
irjdFormat :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdFormat = lens _irjdFormat (\s a -> s { _irjdFormat = a })

-- | Specifies the maximum number of inventory items returned per vault inventory
-- retrieval request. This limit is set when initiating the job with the a InitiateJob
-- request.
irjdLimit :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdLimit = lens _irjdLimit (\s a -> s { _irjdLimit = a })

-- | An opaque string that represents where to continue pagination of the vault
-- inventory retrieval results. You use the marker in a new InitiateJob request
-- to obtain additional inventory items. If there are no more inventory items,
-- this value is 'null'. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering  Range Inventory Retrieval>.
irjdMarker :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdMarker = lens _irjdMarker (\s a -> s { _irjdMarker = a })

-- | The start of the date range in UTC for vault inventory retrieval that
-- includes archives created on or after this date. A string representation of
-- ISO 8601 date format, for example, 2013-03-20T17:03:43Z.
irjdStartDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdStartDate = lens _irjdStartDate (\s a -> s { _irjdStartDate = a })

instance FromJSON InventoryRetrievalJobDescription where
    parseJSON = withObject "InventoryRetrievalJobDescription" $ \o -> InventoryRetrievalJobDescription
        <$> o .:? "EndDate"
        <*> o .:? "Format"
        <*> o .:? "Limit"
        <*> o .:? "Marker"
        <*> o .:? "StartDate"

instance ToJSON InventoryRetrievalJobDescription where
    toJSON InventoryRetrievalJobDescription{..} = object
        [ "Format"    .= _irjdFormat
        , "StartDate" .= _irjdStartDate
        , "EndDate"   .= _irjdEndDate
        , "Limit"     .= _irjdLimit
        , "Marker"    .= _irjdMarker
        ]

data JobParameters = JobParameters
    { _jpArchiveId                    :: Maybe Text
    , _jpDescription                  :: Maybe Text
    , _jpFormat                       :: Maybe Text
    , _jpInventoryRetrievalParameters :: Maybe InventoryRetrievalJobInput
    , _jpRetrievalByteRange           :: Maybe Text
    , _jpSNSTopic                     :: Maybe Text
    , _jpType                         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'JobParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'jpArchiveId' @::@ 'Maybe' 'Text'
--
-- * 'jpDescription' @::@ 'Maybe' 'Text'
--
-- * 'jpFormat' @::@ 'Maybe' 'Text'
--
-- * 'jpInventoryRetrievalParameters' @::@ 'Maybe' 'InventoryRetrievalJobInput'
--
-- * 'jpRetrievalByteRange' @::@ 'Maybe' 'Text'
--
-- * 'jpSNSTopic' @::@ 'Maybe' 'Text'
--
-- * 'jpType' @::@ 'Maybe' 'Text'
--
jobParameters :: JobParameters
jobParameters = JobParameters
    { _jpFormat                       = Nothing
    , _jpType                         = Nothing
    , _jpArchiveId                    = Nothing
    , _jpDescription                  = Nothing
    , _jpSNSTopic                     = Nothing
    , _jpRetrievalByteRange           = Nothing
    , _jpInventoryRetrievalParameters = Nothing
    }

-- | The ID of the archive that you want to retrieve. This field is required only
-- if 'Type' is set to archive-retrieval. An error occurs if you specify this
-- request parameter for an inventory retrieval job request.
jpArchiveId :: Lens' JobParameters (Maybe Text)
jpArchiveId = lens _jpArchiveId (\s a -> s { _jpArchiveId = a })

-- | The optional description for the job. The description must be less than or
-- equal to 1,024 bytes. The allowable characters are 7-bit ASCII without
-- control codes-specifically, ASCII values 32-126 decimal or 0x20-0x7E
-- hexadecimal.
jpDescription :: Lens' JobParameters (Maybe Text)
jpDescription = lens _jpDescription (\s a -> s { _jpDescription = a })

-- | When initiating a job to retrieve a vault inventory, you can optionally add
-- this parameter to your request to specify the output format. If you are
-- initiating an inventory job and do not specify a Format field, JSON is the
-- default format. Valid values are "CSV" and "JSON".
jpFormat :: Lens' JobParameters (Maybe Text)
jpFormat = lens _jpFormat (\s a -> s { _jpFormat = a })

-- | Input parameters used for range inventory retrieval.
jpInventoryRetrievalParameters :: Lens' JobParameters (Maybe InventoryRetrievalJobInput)
jpInventoryRetrievalParameters =
    lens _jpInventoryRetrievalParameters
        (\s a -> s { _jpInventoryRetrievalParameters = a })

-- | The byte range to retrieve for an archive retrieval. in the form "/StartByteValue/-/EndByteValue/" If not specified, the whole archive is retrieved. If
-- specified, the byte range must be megabyte (1024*1024) aligned which means
-- that /StartByteValue/ must be divisible by 1 MB and /EndByteValue/ plus 1 must be
-- divisible by 1 MB or be the end of the archive specified as the archive byte
-- size value minus 1. If RetrievalByteRange is not megabyte aligned, this
-- operation returns a 400 response.
--
-- An error occurs if you specify this field for an inventory retrieval job
-- request.
jpRetrievalByteRange :: Lens' JobParameters (Maybe Text)
jpRetrievalByteRange =
    lens _jpRetrievalByteRange (\s a -> s { _jpRetrievalByteRange = a })

-- | The Amazon SNS topic ARN to which Amazon Glacier sends a notification when
-- the job is completed and the output is ready for you to download. The
-- specified topic publishes the notification to its subscribers. The SNS topic
-- must exist.
jpSNSTopic :: Lens' JobParameters (Maybe Text)
jpSNSTopic = lens _jpSNSTopic (\s a -> s { _jpSNSTopic = a })

-- | The job type. You can initiate a job to retrieve an archive or get an
-- inventory of a vault. Valid values are "archive-retrieval" and
-- "inventory-retrieval".
jpType :: Lens' JobParameters (Maybe Text)
jpType = lens _jpType (\s a -> s { _jpType = a })

instance FromJSON JobParameters where
    parseJSON = withObject "JobParameters" $ \o -> JobParameters
        <$> o .:? "ArchiveId"
        <*> o .:? "Description"
        <*> o .:? "Format"
        <*> o .:? "InventoryRetrievalParameters"
        <*> o .:? "RetrievalByteRange"
        <*> o .:? "SNSTopic"
        <*> o .:? "Type"

instance ToJSON JobParameters where
    toJSON JobParameters{..} = object
        [ "Format"                       .= _jpFormat
        , "Type"                         .= _jpType
        , "ArchiveId"                    .= _jpArchiveId
        , "Description"                  .= _jpDescription
        , "SNSTopic"                     .= _jpSNSTopic
        , "RetrievalByteRange"           .= _jpRetrievalByteRange
        , "InventoryRetrievalParameters" .= _jpInventoryRetrievalParameters
        ]

data DescribeVaultOutput = DescribeVaultOutput
    { _dvoCreationDate      :: Maybe Text
    , _dvoLastInventoryDate :: Maybe Text
    , _dvoNumberOfArchives  :: Maybe Integer
    , _dvoSizeInBytes       :: Maybe Integer
    , _dvoVaultARN          :: Maybe Text
    , _dvoVaultName         :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeVaultOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvoCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'dvoLastInventoryDate' @::@ 'Maybe' 'Text'
--
-- * 'dvoNumberOfArchives' @::@ 'Maybe' 'Integer'
--
-- * 'dvoSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dvoVaultARN' @::@ 'Maybe' 'Text'
--
-- * 'dvoVaultName' @::@ 'Maybe' 'Text'
--
describeVaultOutput :: DescribeVaultOutput
describeVaultOutput = DescribeVaultOutput
    { _dvoVaultARN          = Nothing
    , _dvoVaultName         = Nothing
    , _dvoCreationDate      = Nothing
    , _dvoLastInventoryDate = Nothing
    , _dvoNumberOfArchives  = Nothing
    , _dvoSizeInBytes       = Nothing
    }

-- | The UTC date when the vault was created. A string representation of ISO 8601
-- date format, for example, "2012-03-20T17:03:43.221Z".
dvoCreationDate :: Lens' DescribeVaultOutput (Maybe Text)
dvoCreationDate = lens _dvoCreationDate (\s a -> s { _dvoCreationDate = a })

-- | The UTC date when Amazon Glacier completed the last vault inventory. A string
-- representation of ISO 8601 date format, for example,
-- "2012-03-20T17:03:43.221Z".
dvoLastInventoryDate :: Lens' DescribeVaultOutput (Maybe Text)
dvoLastInventoryDate =
    lens _dvoLastInventoryDate (\s a -> s { _dvoLastInventoryDate = a })

-- | The number of archives in the vault as of the last inventory date. This field
-- will return 'null' if an inventory has not yet run on the vault, for example,
-- if you just created the vault.
dvoNumberOfArchives :: Lens' DescribeVaultOutput (Maybe Integer)
dvoNumberOfArchives =
    lens _dvoNumberOfArchives (\s a -> s { _dvoNumberOfArchives = a })

-- | Total size, in bytes, of the archives in the vault as of the last inventory
-- date. This field will return null if an inventory has not yet run on the
-- vault, for example, if you just created the vault.
dvoSizeInBytes :: Lens' DescribeVaultOutput (Maybe Integer)
dvoSizeInBytes = lens _dvoSizeInBytes (\s a -> s { _dvoSizeInBytes = a })

-- | The Amazon Resource Name (ARN) of the vault.
dvoVaultARN :: Lens' DescribeVaultOutput (Maybe Text)
dvoVaultARN = lens _dvoVaultARN (\s a -> s { _dvoVaultARN = a })

-- | The name of the vault.
dvoVaultName :: Lens' DescribeVaultOutput (Maybe Text)
dvoVaultName = lens _dvoVaultName (\s a -> s { _dvoVaultName = a })

instance FromJSON DescribeVaultOutput where
    parseJSON = withObject "DescribeVaultOutput" $ \o -> DescribeVaultOutput
        <$> o .:? "CreationDate"
        <*> o .:? "LastInventoryDate"
        <*> o .:? "NumberOfArchives"
        <*> o .:? "SizeInBytes"
        <*> o .:? "VaultARN"
        <*> o .:? "VaultName"

instance ToJSON DescribeVaultOutput where
    toJSON DescribeVaultOutput{..} = object
        [ "VaultARN"          .= _dvoVaultARN
        , "VaultName"         .= _dvoVaultName
        , "CreationDate"      .= _dvoCreationDate
        , "LastInventoryDate" .= _dvoLastInventoryDate
        , "NumberOfArchives"  .= _dvoNumberOfArchives
        , "SizeInBytes"       .= _dvoSizeInBytes
        ]

data DataRetrievalRule = DataRetrievalRule
    { _drrBytesPerHour :: Maybe Integer
    , _drrStrategy     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DataRetrievalRule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrBytesPerHour' @::@ 'Maybe' 'Integer'
--
-- * 'drrStrategy' @::@ 'Maybe' 'Text'
--
dataRetrievalRule :: DataRetrievalRule
dataRetrievalRule = DataRetrievalRule
    { _drrStrategy     = Nothing
    , _drrBytesPerHour = Nothing
    }

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is 'BytesPerHour'. Your PUT operation will be rejected if the Strategy field is not set to 'BytesPerHour' and you set this field.
drrBytesPerHour :: Lens' DataRetrievalRule (Maybe Integer)
drrBytesPerHour = lens _drrBytesPerHour (\s a -> s { _drrBytesPerHour = a })

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
drrStrategy :: Lens' DataRetrievalRule (Maybe Text)
drrStrategy = lens _drrStrategy (\s a -> s { _drrStrategy = a })

instance FromJSON DataRetrievalRule where
    parseJSON = withObject "DataRetrievalRule" $ \o -> DataRetrievalRule
        <$> o .:? "BytesPerHour"
        <*> o .:? "Strategy"

instance ToJSON DataRetrievalRule where
    toJSON DataRetrievalRule{..} = object
        [ "Strategy"     .= _drrStrategy
        , "BytesPerHour" .= _drrBytesPerHour
        ]

data ActionCode
    = ArchiveRetrieval   -- ^ ArchiveRetrieval
    | InventoryRetrieval -- ^ InventoryRetrieval
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ActionCode

instance FromText ActionCode where
    parser = takeLowerText >>= \case
        "archiveretrieval"   -> pure ArchiveRetrieval
        "inventoryretrieval" -> pure InventoryRetrieval
        e                    -> fail $
            "Failure parsing ActionCode from " ++ show e

instance ToText ActionCode where
    toText = \case
        ArchiveRetrieval   -> "ArchiveRetrieval"
        InventoryRetrieval -> "InventoryRetrieval"

instance ToByteString ActionCode
instance ToHeader     ActionCode
instance ToQuery      ActionCode

instance FromJSON ActionCode where
    parseJSON = parseJSONText "ActionCode"

instance ToJSON ActionCode where
    toJSON = toJSONText

data VaultNotificationConfig = VaultNotificationConfig
    { _vncEvents   :: List "Events" Text
    , _vncSNSTopic :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'VaultNotificationConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vncEvents' @::@ ['Text']
--
-- * 'vncSNSTopic' @::@ 'Maybe' 'Text'
--
vaultNotificationConfig :: VaultNotificationConfig
vaultNotificationConfig = VaultNotificationConfig
    { _vncSNSTopic = Nothing
    , _vncEvents   = mempty
    }

-- | A list of one or more events for which Amazon Glacier will send a
-- notification to the specified Amazon SNS topic.
vncEvents :: Lens' VaultNotificationConfig [Text]
vncEvents = lens _vncEvents (\s a -> s { _vncEvents = a }) . _List

-- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon Resource
-- Name (ARN).
vncSNSTopic :: Lens' VaultNotificationConfig (Maybe Text)
vncSNSTopic = lens _vncSNSTopic (\s a -> s { _vncSNSTopic = a })

instance FromJSON VaultNotificationConfig where
    parseJSON = withObject "VaultNotificationConfig" $ \o -> VaultNotificationConfig
        <$> o .:? "Events" .!= mempty
        <*> o .:? "SNSTopic"

instance ToJSON VaultNotificationConfig where
    toJSON VaultNotificationConfig{..} = object
        [ "SNSTopic" .= _vncSNSTopic
        , "Events"   .= _vncEvents
        ]

data InventoryRetrievalJobInput = InventoryRetrievalJobInput
    { _irjiEndDate   :: Maybe Text
    , _irjiLimit     :: Maybe Text
    , _irjiMarker    :: Maybe Text
    , _irjiStartDate :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InventoryRetrievalJobInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'irjiEndDate' @::@ 'Maybe' 'Text'
--
-- * 'irjiLimit' @::@ 'Maybe' 'Text'
--
-- * 'irjiMarker' @::@ 'Maybe' 'Text'
--
-- * 'irjiStartDate' @::@ 'Maybe' 'Text'
--
inventoryRetrievalJobInput :: InventoryRetrievalJobInput
inventoryRetrievalJobInput = InventoryRetrievalJobInput
    { _irjiStartDate = Nothing
    , _irjiEndDate   = Nothing
    , _irjiLimit     = Nothing
    , _irjiMarker    = Nothing
    }

-- | The end of the date range in UTC for vault inventory retrieval that includes
-- archives created before this date. A string representation of ISO 8601 date
-- format, for example, 2013-03-20T17:03:43Z.
irjiEndDate :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiEndDate = lens _irjiEndDate (\s a -> s { _irjiEndDate = a })

-- | Specifies the maximum number of inventory items returned per vault inventory
-- retrieval request. Valid values are greater than or equal to 1.
irjiLimit :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiLimit = lens _irjiLimit (\s a -> s { _irjiLimit = a })

-- | An opaque string that represents where to continue pagination of the vault
-- inventory retrieval results. You use the marker in a new InitiateJob request
-- to obtain additional inventory items. If there are no more inventory items,
-- this value is 'null'.
irjiMarker :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiMarker = lens _irjiMarker (\s a -> s { _irjiMarker = a })

-- | The start of the date range in UTC for vault inventory retrieval that
-- includes archives created on or after this date. A string representation of
-- ISO 8601 date format, for example, 2013-03-20T17:03:43Z.
irjiStartDate :: Lens' InventoryRetrievalJobInput (Maybe Text)
irjiStartDate = lens _irjiStartDate (\s a -> s { _irjiStartDate = a })

instance FromJSON InventoryRetrievalJobInput where
    parseJSON = withObject "InventoryRetrievalJobInput" $ \o -> InventoryRetrievalJobInput
        <$> o .:? "EndDate"
        <*> o .:? "Limit"
        <*> o .:? "Marker"
        <*> o .:? "StartDate"

instance ToJSON InventoryRetrievalJobInput where
    toJSON InventoryRetrievalJobInput{..} = object
        [ "StartDate" .= _irjiStartDate
        , "EndDate"   .= _irjiEndDate
        , "Limit"     .= _irjiLimit
        , "Marker"    .= _irjiMarker
        ]

data PartListElement = PartListElement
    { _pleRangeInBytes   :: Maybe Text
    , _pleSHA256TreeHash :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PartListElement' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pleRangeInBytes' @::@ 'Maybe' 'Text'
--
-- * 'pleSHA256TreeHash' @::@ 'Maybe' 'Text'
--
partListElement :: PartListElement
partListElement = PartListElement
    { _pleRangeInBytes   = Nothing
    , _pleSHA256TreeHash = Nothing
    }

-- | The byte range of a part, inclusive of the upper value of the range.
pleRangeInBytes :: Lens' PartListElement (Maybe Text)
pleRangeInBytes = lens _pleRangeInBytes (\s a -> s { _pleRangeInBytes = a })

-- | The SHA256 tree hash value that Amazon Glacier calculated for the part. This
-- field is never 'null'.
pleSHA256TreeHash :: Lens' PartListElement (Maybe Text)
pleSHA256TreeHash =
    lens _pleSHA256TreeHash (\s a -> s { _pleSHA256TreeHash = a })

instance FromJSON PartListElement where
    parseJSON = withObject "PartListElement" $ \o -> PartListElement
        <$> o .:? "RangeInBytes"
        <*> o .:? "SHA256TreeHash"

instance ToJSON PartListElement where
    toJSON PartListElement{..} = object
        [ "RangeInBytes"   .= _pleRangeInBytes
        , "SHA256TreeHash" .= _pleSHA256TreeHash
        ]

newtype DataRetrievalPolicy = DataRetrievalPolicy
    { _drpRules :: List "Rules" DataRetrievalRule
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DataRetrievalPolicy where
    type Item DataRetrievalPolicy = DataRetrievalRule

    fromList = DataRetrievalPolicy . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _drpRules

-- | 'DataRetrievalPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drpRules' @::@ ['DataRetrievalRule']
--
dataRetrievalPolicy :: DataRetrievalPolicy
dataRetrievalPolicy = DataRetrievalPolicy
    { _drpRules = mempty
    }

-- | The policy rule. Although this is a list type, currently there must be only
-- one rule, which contains a Strategy field and optionally a BytesPerHour field.
drpRules :: Lens' DataRetrievalPolicy [DataRetrievalRule]
drpRules = lens _drpRules (\s a -> s { _drpRules = a }) . _List

instance FromJSON DataRetrievalPolicy where
    parseJSON = withObject "DataRetrievalPolicy" $ \o -> DataRetrievalPolicy
        <$> o .:? "Rules" .!= mempty

instance ToJSON DataRetrievalPolicy where
    toJSON DataRetrievalPolicy{..} = object
        [ "Rules" .= _drpRules
        ]

data GlacierJobDescription = GlacierJobDescription
    { _gjdAction                       :: Maybe ActionCode
    , _gjdArchiveId                    :: Maybe Text
    , _gjdArchiveSHA256TreeHash        :: Maybe Text
    , _gjdArchiveSizeInBytes           :: Maybe Integer
    , _gjdCompleted                    :: Maybe Bool
    , _gjdCompletionDate               :: Maybe Text
    , _gjdCreationDate                 :: Maybe Text
    , _gjdInventoryRetrievalParameters :: Maybe InventoryRetrievalJobDescription
    , _gjdInventorySizeInBytes         :: Maybe Integer
    , _gjdJobDescription               :: Maybe Text
    , _gjdJobId                        :: Maybe Text
    , _gjdRetrievalByteRange           :: Maybe Text
    , _gjdSHA256TreeHash               :: Maybe Text
    , _gjdSNSTopic                     :: Maybe Text
    , _gjdStatusCode                   :: Maybe StatusCode
    , _gjdStatusMessage                :: Maybe Text
    , _gjdVaultARN                     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GlacierJobDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gjdAction' @::@ 'Maybe' 'ActionCode'
--
-- * 'gjdArchiveId' @::@ 'Maybe' 'Text'
--
-- * 'gjdArchiveSHA256TreeHash' @::@ 'Maybe' 'Text'
--
-- * 'gjdArchiveSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gjdCompleted' @::@ 'Maybe' 'Bool'
--
-- * 'gjdCompletionDate' @::@ 'Maybe' 'Text'
--
-- * 'gjdCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'gjdInventoryRetrievalParameters' @::@ 'Maybe' 'InventoryRetrievalJobDescription'
--
-- * 'gjdInventorySizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gjdJobDescription' @::@ 'Maybe' 'Text'
--
-- * 'gjdJobId' @::@ 'Maybe' 'Text'
--
-- * 'gjdRetrievalByteRange' @::@ 'Maybe' 'Text'
--
-- * 'gjdSHA256TreeHash' @::@ 'Maybe' 'Text'
--
-- * 'gjdSNSTopic' @::@ 'Maybe' 'Text'
--
-- * 'gjdStatusCode' @::@ 'Maybe' 'StatusCode'
--
-- * 'gjdStatusMessage' @::@ 'Maybe' 'Text'
--
-- * 'gjdVaultARN' @::@ 'Maybe' 'Text'
--
glacierJobDescription :: GlacierJobDescription
glacierJobDescription = GlacierJobDescription
    { _gjdJobId                        = Nothing
    , _gjdJobDescription               = Nothing
    , _gjdAction                       = Nothing
    , _gjdArchiveId                    = Nothing
    , _gjdVaultARN                     = Nothing
    , _gjdCreationDate                 = Nothing
    , _gjdCompleted                    = Nothing
    , _gjdStatusCode                   = Nothing
    , _gjdStatusMessage                = Nothing
    , _gjdArchiveSizeInBytes           = Nothing
    , _gjdInventorySizeInBytes         = Nothing
    , _gjdSNSTopic                     = Nothing
    , _gjdCompletionDate               = Nothing
    , _gjdSHA256TreeHash               = Nothing
    , _gjdArchiveSHA256TreeHash        = Nothing
    , _gjdRetrievalByteRange           = Nothing
    , _gjdInventoryRetrievalParameters = Nothing
    }

-- | The job type. It is either ArchiveRetrieval or InventoryRetrieval.
gjdAction :: Lens' GlacierJobDescription (Maybe ActionCode)
gjdAction = lens _gjdAction (\s a -> s { _gjdAction = a })

-- | For an ArchiveRetrieval job, this is the archive ID requested for download.
-- Otherwise, this field is null.
gjdArchiveId :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveId = lens _gjdArchiveId (\s a -> s { _gjdArchiveId = a })

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval jobs, this field is null.
gjdArchiveSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdArchiveSHA256TreeHash =
    lens _gjdArchiveSHA256TreeHash
        (\s a -> s { _gjdArchiveSHA256TreeHash = a })

-- | For an ArchiveRetrieval job, this is the size in bytes of the archive being
-- requested for download. For the InventoryRetrieval job, the value is null.
gjdArchiveSizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdArchiveSizeInBytes =
    lens _gjdArchiveSizeInBytes (\s a -> s { _gjdArchiveSizeInBytes = a })

-- | The job status. When a job is completed, you get the job's output.
gjdCompleted :: Lens' GlacierJobDescription (Maybe Bool)
gjdCompleted = lens _gjdCompleted (\s a -> s { _gjdCompleted = a })

-- | The UTC time that the archive retrieval request completed. While the job is
-- in progress, the value will be null.
gjdCompletionDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCompletionDate =
    lens _gjdCompletionDate (\s a -> s { _gjdCompletionDate = a })

-- | The UTC date when the job was created. A string representation of ISO 8601
-- date format, for example, "2012-03-20T17:03:43.221Z".
gjdCreationDate :: Lens' GlacierJobDescription (Maybe Text)
gjdCreationDate = lens _gjdCreationDate (\s a -> s { _gjdCreationDate = a })

-- | Parameters used for range inventory retrieval.
gjdInventoryRetrievalParameters :: Lens' GlacierJobDescription (Maybe InventoryRetrievalJobDescription)
gjdInventoryRetrievalParameters =
    lens _gjdInventoryRetrievalParameters
        (\s a -> s { _gjdInventoryRetrievalParameters = a })

-- | For an InventoryRetrieval job, this is the size in bytes of the inventory
-- requested for download. For the ArchiveRetrieval job, the value is null.
gjdInventorySizeInBytes :: Lens' GlacierJobDescription (Maybe Integer)
gjdInventorySizeInBytes =
    lens _gjdInventorySizeInBytes (\s a -> s { _gjdInventorySizeInBytes = a })

-- | The job description you provided when you initiated the job.
gjdJobDescription :: Lens' GlacierJobDescription (Maybe Text)
gjdJobDescription =
    lens _gjdJobDescription (\s a -> s { _gjdJobDescription = a })

-- | An opaque string that identifies an Amazon Glacier job.
gjdJobId :: Lens' GlacierJobDescription (Maybe Text)
gjdJobId = lens _gjdJobId (\s a -> s { _gjdJobId = a })

-- | The retrieved byte range for archive retrieval jobs in the form "/StartByteValue/-/EndByteValue/" If no range was specified in the archive retrieval, then the
-- whole archive is retrieved and /StartByteValue/ equals 0 and /EndByteValue/
-- equals the size of the archive minus 1. For inventory retrieval jobs this
-- field is null.
gjdRetrievalByteRange :: Lens' GlacierJobDescription (Maybe Text)
gjdRetrievalByteRange =
    lens _gjdRetrievalByteRange (\s a -> s { _gjdRetrievalByteRange = a })

-- | For an ArchiveRetrieval job, it is the checksum of the archive. Otherwise,
-- the value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the
-- Initiate a Job request for an archive specified a tree-hash aligned range,
-- then this field returns a value.
--
-- For the specific case when the whole archive is retrieved, this value is
-- the same as the ArchiveSHA256TreeHash value.
--
-- This field is null in the following situations:  Archive retrieval jobs
-- that specify a range that is not tree-hash aligned.
--
-- Archival jobs that specify a range that is equal to the whole archive and
-- the job status is InProgress.
--
-- Inventory jobs.
--
--
gjdSHA256TreeHash :: Lens' GlacierJobDescription (Maybe Text)
gjdSHA256TreeHash =
    lens _gjdSHA256TreeHash (\s a -> s { _gjdSHA256TreeHash = a })

-- | An Amazon Simple Notification Service (Amazon SNS) topic that receives
-- notification.
gjdSNSTopic :: Lens' GlacierJobDescription (Maybe Text)
gjdSNSTopic = lens _gjdSNSTopic (\s a -> s { _gjdSNSTopic = a })

-- | The status code can be InProgress, Succeeded, or Failed, and indicates the
-- status of the job.
gjdStatusCode :: Lens' GlacierJobDescription (Maybe StatusCode)
gjdStatusCode = lens _gjdStatusCode (\s a -> s { _gjdStatusCode = a })

-- | A friendly message that describes the job status.
gjdStatusMessage :: Lens' GlacierJobDescription (Maybe Text)
gjdStatusMessage = lens _gjdStatusMessage (\s a -> s { _gjdStatusMessage = a })

-- | The Amazon Resource Name (ARN) of the vault from which the archive retrieval
-- was requested.
gjdVaultARN :: Lens' GlacierJobDescription (Maybe Text)
gjdVaultARN = lens _gjdVaultARN (\s a -> s { _gjdVaultARN = a })

instance FromJSON GlacierJobDescription where
    parseJSON = withObject "GlacierJobDescription" $ \o -> GlacierJobDescription
        <$> o .:? "Action"
        <*> o .:? "ArchiveId"
        <*> o .:? "ArchiveSHA256TreeHash"
        <*> o .:? "ArchiveSizeInBytes"
        <*> o .:? "Completed"
        <*> o .:? "CompletionDate"
        <*> o .:? "CreationDate"
        <*> o .:? "InventoryRetrievalParameters"
        <*> o .:? "InventorySizeInBytes"
        <*> o .:? "JobDescription"
        <*> o .:? "JobId"
        <*> o .:? "RetrievalByteRange"
        <*> o .:? "SHA256TreeHash"
        <*> o .:? "SNSTopic"
        <*> o .:? "StatusCode"
        <*> o .:? "StatusMessage"
        <*> o .:? "VaultARN"

instance ToJSON GlacierJobDescription where
    toJSON GlacierJobDescription{..} = object
        [ "JobId"                        .= _gjdJobId
        , "JobDescription"               .= _gjdJobDescription
        , "Action"                       .= _gjdAction
        , "ArchiveId"                    .= _gjdArchiveId
        , "VaultARN"                     .= _gjdVaultARN
        , "CreationDate"                 .= _gjdCreationDate
        , "Completed"                    .= _gjdCompleted
        , "StatusCode"                   .= _gjdStatusCode
        , "StatusMessage"                .= _gjdStatusMessage
        , "ArchiveSizeInBytes"           .= _gjdArchiveSizeInBytes
        , "InventorySizeInBytes"         .= _gjdInventorySizeInBytes
        , "SNSTopic"                     .= _gjdSNSTopic
        , "CompletionDate"               .= _gjdCompletionDate
        , "SHA256TreeHash"               .= _gjdSHA256TreeHash
        , "ArchiveSHA256TreeHash"        .= _gjdArchiveSHA256TreeHash
        , "RetrievalByteRange"           .= _gjdRetrievalByteRange
        , "InventoryRetrievalParameters" .= _gjdInventoryRetrievalParameters
        ]

newtype VaultAccessPolicy = VaultAccessPolicy
    { _vapPolicy :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'VaultAccessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vapPolicy' @::@ 'Maybe' 'Text'
--
vaultAccessPolicy :: VaultAccessPolicy
vaultAccessPolicy = VaultAccessPolicy
    { _vapPolicy = Nothing
    }

-- | The vault access policy.
vapPolicy :: Lens' VaultAccessPolicy (Maybe Text)
vapPolicy = lens _vapPolicy (\s a -> s { _vapPolicy = a })

instance FromJSON VaultAccessPolicy where
    parseJSON = withObject "VaultAccessPolicy" $ \o -> VaultAccessPolicy
        <$> o .:? "Policy"

instance ToJSON VaultAccessPolicy where
    toJSON VaultAccessPolicy{..} = object
        [ "Policy" .= _vapPolicy
        ]

data StatusCode
    = Failed     -- ^ Failed
    | InProgress -- ^ InProgress
    | Succeeded  -- ^ Succeeded
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StatusCode

instance FromText StatusCode where
    parser = takeLowerText >>= \case
        "failed"     -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded"  -> pure Succeeded
        e            -> fail $
            "Failure parsing StatusCode from " ++ show e

instance ToText StatusCode where
    toText = \case
        Failed     -> "Failed"
        InProgress -> "InProgress"
        Succeeded  -> "Succeeded"

instance ToByteString StatusCode
instance ToHeader     StatusCode
instance ToQuery      StatusCode

instance FromJSON StatusCode where
    parseJSON = parseJSONText "StatusCode"

instance ToJSON StatusCode where
    toJSON = toJSONText

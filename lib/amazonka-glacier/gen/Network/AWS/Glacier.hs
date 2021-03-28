{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon S3 Glacier (Glacier) is a storage solution for "cold data."
--
-- Glacier is an extremely low-cost storage service that provides secure, durable, and easy-to-use storage for data backup and archival. With Glacier, customers can store their data cost effectively for months, years, or decades. Glacier also enables customers to offload the administrative burdens of operating and scaling storage to AWS, so they don't have to worry about capacity planning, hardware provisioning, data replication, hardware failure and recovery, or time-consuming hardware migrations.
-- Glacier is a great storage choice when low storage cost is paramount and your data is rarely retrieved. If your application requires fast or frequent access to your data, consider using Amazon S3. For more information, see <http://aws.amazon.com/s3/ Amazon Simple Storage Service (Amazon S3)> .
-- You can store any kind of data in any format. There is no maximum limit on the total amount of data you can store in Glacier.
-- If you are a first-time user of Glacier, we recommend that you begin by reading the following sections in the /Amazon S3 Glacier Developer Guide/ :
--
--     * <https://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html What is Amazon S3 Glacier> - This section of the Developer Guide describes the underlying data model, the operations it supports, and the AWS SDKs that you can use to interact with the service.
--
--
--     * <https://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html Getting Started with Amazon S3 Glacier> - The Getting Started section walks you through the process of creating a vault, uploading archives, creating jobs to download archives, retrieving the job output, and deleting archives.
--
--
module Network.AWS.Glacier
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** PolicyEnforcedException
    , _PolicyEnforcedException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** RequestTimeoutException
    , _RequestTimeoutException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InsufficientCapacityException
    , _InsufficientCapacityException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** MissingParameterValueException
    , _MissingParameterValueException

    -- * Waiters
    -- $waiters

    -- ** VaultNotExists
    , mkVaultNotExists

    -- ** VaultExists
    , mkVaultExists

    -- * Operations
    -- $operations

    -- ** SetVaultAccessPolicy 
    , module Network.AWS.Glacier.SetVaultAccessPolicy

    -- ** InitiateJob 
    , module Network.AWS.Glacier.InitiateJob

    -- ** SetDataRetrievalPolicy 
    , module Network.AWS.Glacier.SetDataRetrievalPolicy

    -- ** DeleteVaultAccessPolicy 
    , module Network.AWS.Glacier.DeleteVaultAccessPolicy

    -- ** GetVaultNotifications 
    , module Network.AWS.Glacier.GetVaultNotifications

    -- ** ListTagsForVault 
    , module Network.AWS.Glacier.ListTagsForVault

    -- ** UploadMultipartPart 
    , module Network.AWS.Glacier.UploadMultipartPart

    -- ** DeleteVaultNotifications 
    , module Network.AWS.Glacier.DeleteVaultNotifications

    -- ** CompleteVaultLock 
    , module Network.AWS.Glacier.CompleteVaultLock

    -- ** AbortVaultLock 
    , module Network.AWS.Glacier.AbortVaultLock

    -- ** ListVaults (Paginated)
    , module Network.AWS.Glacier.ListVaults

    -- ** ListProvisionedCapacity 
    , module Network.AWS.Glacier.ListProvisionedCapacity

    -- ** ListJobs (Paginated)
    , module Network.AWS.Glacier.ListJobs

    -- ** SetVaultNotifications 
    , module Network.AWS.Glacier.SetVaultNotifications

    -- ** GetJobOutput 
    , module Network.AWS.Glacier.GetJobOutput

    -- ** CompleteMultipartUpload 
    , module Network.AWS.Glacier.CompleteMultipartUpload

    -- ** ListMultipartUploads (Paginated)
    , module Network.AWS.Glacier.ListMultipartUploads

    -- ** AbortMultipartUpload 
    , module Network.AWS.Glacier.AbortMultipartUpload

    -- ** PurchaseProvisionedCapacity 
    , module Network.AWS.Glacier.PurchaseProvisionedCapacity

    -- ** DescribeVault 
    , module Network.AWS.Glacier.DescribeVault

    -- ** GetVaultLock 
    , module Network.AWS.Glacier.GetVaultLock

    -- ** DescribeJob 
    , module Network.AWS.Glacier.DescribeJob

    -- ** InitiateVaultLock 
    , module Network.AWS.Glacier.InitiateVaultLock

    -- ** GetVaultAccessPolicy 
    , module Network.AWS.Glacier.GetVaultAccessPolicy

    -- ** GetDataRetrievalPolicy 
    , module Network.AWS.Glacier.GetDataRetrievalPolicy

    -- ** RemoveTagsFromVault 
    , module Network.AWS.Glacier.RemoveTagsFromVault

    -- ** DeleteVault 
    , module Network.AWS.Glacier.DeleteVault

    -- ** DeleteArchive 
    , module Network.AWS.Glacier.DeleteArchive

    -- ** CreateVault 
    , module Network.AWS.Glacier.CreateVault

    -- ** InitiateMultipartUpload 
    , module Network.AWS.Glacier.InitiateMultipartUpload

    -- ** ListParts (Paginated)
    , module Network.AWS.Glacier.ListParts

    -- ** AddTagsToVault 
    , module Network.AWS.Glacier.AddTagsToVault

    -- ** UploadArchive 
    , module Network.AWS.Glacier.UploadArchive

    -- * Types

    -- ** ArchiveCreationOutput
    , ArchiveCreationOutput (..)
    , mkArchiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

    -- ** EncryptionType
    , EncryptionType (..)

    -- ** UploadListElement
    , UploadListElement (..)
    , mkUploadListElement
    , uleArchiveDescription
    , uleCreationDate
    , uleMultipartUploadId
    , ulePartSizeInBytes
    , uleVaultARN

    -- ** ExpressionType
    , ExpressionType (..)

    -- ** SelectParameters
    , SelectParameters (..)
    , mkSelectParameters
    , spExpression
    , spExpressionType
    , spInputSerialization
    , spOutputSerialization

    -- ** InventoryRetrievalJobDescription
    , InventoryRetrievalJobDescription (..)
    , mkInventoryRetrievalJobDescription
    , irjdEndDate
    , irjdFormat
    , irjdLimit
    , irjdMarker
    , irjdStartDate

    -- ** CannedACL
    , CannedACL (..)

    -- ** JobParameters
    , JobParameters (..)
    , mkJobParameters
    , jpArchiveId
    , jpDescription
    , jpFormat
    , jpInventoryRetrievalParameters
    , jpOutputLocation
    , jpRetrievalByteRange
    , jpSNSTopic
    , jpSelectParameters
    , jpTier
    , jpType

    -- ** DescribeVaultOutput
    , DescribeVaultOutput (..)
    , mkDescribeVaultOutput
    , dvoCreationDate
    , dvoLastInventoryDate
    , dvoNumberOfArchives
    , dvoSizeInBytes
    , dvoVaultARN
    , dvoVaultName

    -- ** CSVInput
    , CSVInput (..)
    , mkCSVInput
    , csviComments
    , csviFieldDelimiter
    , csviFileHeaderInfo
    , csviQuoteCharacter
    , csviQuoteEscapeCharacter
    , csviRecordDelimiter

    -- ** DataRetrievalRule
    , DataRetrievalRule (..)
    , mkDataRetrievalRule
    , drrBytesPerHour
    , drrStrategy

    -- ** QuoteFields
    , QuoteFields (..)

    -- ** ActionCode
    , ActionCode (..)

    -- ** Grant
    , Grant (..)
    , mkGrant
    , gGrantee
    , gPermission

    -- ** TagValue
    , TagValue (..)

    -- ** OutputSerialization
    , OutputSerialization (..)
    , mkOutputSerialization
    , osCsv

    -- ** CSVOutput
    , CSVOutput (..)
    , mkCSVOutput
    , csvoFieldDelimiter
    , csvoQuoteCharacter
    , csvoQuoteEscapeCharacter
    , csvoQuoteFields
    , csvoRecordDelimiter

    -- ** Encryption
    , Encryption (..)
    , mkEncryption
    , eEncryptionType
    , eKMSContext
    , eKMSKeyId

    -- ** FileHeaderInfo
    , FileHeaderInfo (..)

    -- ** StorageClass
    , StorageClass (..)

    -- ** OutputLocation
    , OutputLocation (..)
    , mkOutputLocation
    , olS3

    -- ** ProvisionedCapacityDescription
    , ProvisionedCapacityDescription (..)
    , mkProvisionedCapacityDescription
    , pcdCapacityId
    , pcdExpirationDate
    , pcdStartDate

    -- ** VaultNotificationConfig
    , VaultNotificationConfig (..)
    , mkVaultNotificationConfig
    , vncEvents
    , vncSNSTopic

    -- ** InventoryRetrievalJobInput
    , InventoryRetrievalJobInput (..)
    , mkInventoryRetrievalJobInput
    , irjiEndDate
    , irjiLimit
    , irjiMarker
    , irjiStartDate

    -- ** TagKey
    , TagKey (..)

    -- ** S3Location
    , S3Location (..)
    , mkS3Location
    , slAccessControlList
    , slBucketName
    , slCannedACL
    , slEncryption
    , slPrefix
    , slStorageClass
    , slTagging
    , slUserMetadata

    -- ** Type
    , Type (..)

    -- ** InputSerialization
    , InputSerialization (..)
    , mkInputSerialization
    , isCsv

    -- ** PartListElement
    , PartListElement (..)
    , mkPartListElement
    , pleRangeInBytes
    , pleSHA256TreeHash

    -- ** Permission
    , Permission (..)

    -- ** Grantee
    , Grantee (..)
    , mkGrantee
    , gType
    , gDisplayName
    , gEmailAddress
    , gID
    , gURI

    -- ** DataRetrievalPolicy
    , DataRetrievalPolicy (..)
    , mkDataRetrievalPolicy
    , drpRules

    -- ** GlacierJobDescription
    , GlacierJobDescription (..)
    , mkGlacierJobDescription
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
    , gjdJobOutputPath
    , gjdOutputLocation
    , gjdRetrievalByteRange
    , gjdSHA256TreeHash
    , gjdSNSTopic
    , gjdSelectParameters
    , gjdStatusCode
    , gjdStatusMessage
    , gjdTier
    , gjdVaultARN

    -- ** VaultAccessPolicy
    , VaultAccessPolicy (..)
    , mkVaultAccessPolicy
    , vapPolicy

    -- ** VaultLockPolicy
    , VaultLockPolicy (..)
    , mkVaultLockPolicy
    , vlpPolicy

    -- ** StatusCode
    , StatusCode (..)

    -- ** EndDate
    , EndDate (..)

    -- ** StartDate
    , StartDate (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Waiters
import Network.AWS.Glacier.SetVaultAccessPolicy
import Network.AWS.Glacier.InitiateJob
import Network.AWS.Glacier.SetDataRetrievalPolicy
import Network.AWS.Glacier.DeleteVaultAccessPolicy
import Network.AWS.Glacier.GetVaultNotifications
import Network.AWS.Glacier.ListTagsForVault
import Network.AWS.Glacier.UploadMultipartPart
import Network.AWS.Glacier.DeleteVaultNotifications
import Network.AWS.Glacier.CompleteVaultLock
import Network.AWS.Glacier.AbortVaultLock
import Network.AWS.Glacier.ListVaults
import Network.AWS.Glacier.ListProvisionedCapacity
import Network.AWS.Glacier.ListJobs
import Network.AWS.Glacier.SetVaultNotifications
import Network.AWS.Glacier.GetJobOutput
import Network.AWS.Glacier.CompleteMultipartUpload
import Network.AWS.Glacier.ListMultipartUploads
import Network.AWS.Glacier.AbortMultipartUpload
import Network.AWS.Glacier.PurchaseProvisionedCapacity
import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.GetVaultLock
import Network.AWS.Glacier.DescribeJob
import Network.AWS.Glacier.InitiateVaultLock
import Network.AWS.Glacier.GetVaultAccessPolicy
import Network.AWS.Glacier.GetDataRetrievalPolicy
import Network.AWS.Glacier.RemoveTagsFromVault
import Network.AWS.Glacier.DeleteVault
import Network.AWS.Glacier.DeleteArchive
import Network.AWS.Glacier.CreateVault
import Network.AWS.Glacier.InitiateMultipartUpload
import Network.AWS.Glacier.ListParts
import Network.AWS.Glacier.AddTagsToVault
import Network.AWS.Glacier.UploadArchive
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Glacier'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

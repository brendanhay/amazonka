{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Glacier is a storage solution for \"cold data.\"
--
-- Amazon Glacier is an extremely low-cost storage service that provides
-- secure, durable, and easy-to-use storage for data backup and archival.
-- With Amazon Glacier, customers can store their data cost effectively for
-- months, years, or decades. Amazon Glacier also enables customers to
-- offload the administrative burdens of operating and scaling storage to
-- AWS, so they don\'t have to worry about capacity planning, hardware
-- provisioning, data replication, hardware failure and recovery, or
-- time-consuming hardware migrations.
--
-- Amazon Glacier is a great storage choice when low storage cost is
-- paramount, your data is rarely retrieved, and retrieval latency of
-- several hours is acceptable. If your application requires fast or
-- frequent access to your data, consider using Amazon S3. For more
-- information, go to
-- <http://aws.amazon.com/s3/ Amazon Simple Storage Service (Amazon S3)>.
--
-- You can store any kind of data in any format. There is no maximum limit
-- on the total amount of data you can store in Amazon Glacier.
--
-- If you are a first-time user of Amazon Glacier, we recommend that you
-- begin by reading the following sections in the /Amazon Glacier Developer
-- Guide/:
--
-- -   <http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html What is Amazon Glacier>
--     - This section of the Developer Guide describes the underlying data
--     model, the operations it supports, and the AWS SDKs that you can use
--     to interact with the service.
--
-- -   <http://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html Getting Started with Amazon Glacier>
--     - The Getting Started section walks you through the process of
--     creating a vault, uploading archives, creating jobs to download
--     archives, retrieving the job output, and deleting archives.
--
--
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html AWS API Reference>
module Network.AWS.Glacier
    (
    -- * Service Configuration
      glacier

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

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** MissingParameterValueException
    , _MissingParameterValueException

    -- * Waiters
    -- $waiters

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

    -- ** ListTagsForVault
    , module Network.AWS.Glacier.ListTagsForVault

    -- ** GetVaultNotifications
    , module Network.AWS.Glacier.GetVaultNotifications

    -- ** UploadMultipartPart
    , module Network.AWS.Glacier.UploadMultipartPart

    -- ** DeleteVaultNotifications
    , module Network.AWS.Glacier.DeleteVaultNotifications

    -- ** CompleteVaultLock
    , module Network.AWS.Glacier.CompleteVaultLock

    -- ** AbortVaultLock
    , module Network.AWS.Glacier.AbortVaultLock

    -- ** ListVaults
    , module Network.AWS.Glacier.ListVaults

    -- ** GetJobOutput
    , module Network.AWS.Glacier.GetJobOutput

    -- ** ListJobs
    , module Network.AWS.Glacier.ListJobs

    -- ** SetVaultNotifications
    , module Network.AWS.Glacier.SetVaultNotifications

    -- ** CompleteMultipartUpload
    , module Network.AWS.Glacier.CompleteMultipartUpload

    -- ** ListMultipartUploads
    , module Network.AWS.Glacier.ListMultipartUploads

    -- ** DescribeVault
    , module Network.AWS.Glacier.DescribeVault

    -- ** AbortMultipartUpload
    , module Network.AWS.Glacier.AbortMultipartUpload

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

    -- ** DeleteArchive
    , module Network.AWS.Glacier.DeleteArchive

    -- ** DeleteVault
    , module Network.AWS.Glacier.DeleteVault

    -- ** InitiateMultipartUpload
    , module Network.AWS.Glacier.InitiateMultipartUpload

    -- ** ListParts
    , module Network.AWS.Glacier.ListParts

    -- ** CreateVault
    , module Network.AWS.Glacier.CreateVault

    -- ** AddTagsToVault
    , module Network.AWS.Glacier.AddTagsToVault

    -- ** UploadArchive
    , module Network.AWS.Glacier.UploadArchive

    -- * Types

    -- ** ActionCode
    , ActionCode (..)

    -- ** StatusCode
    , StatusCode (..)

    -- ** ArchiveCreationOutput
    , ArchiveCreationOutput
    , archiveCreationOutput
    , acoArchiveId
    , acoChecksum
    , acoLocation

    -- ** DataRetrievalPolicy
    , DataRetrievalPolicy
    , dataRetrievalPolicy
    , drpRules

    -- ** DataRetrievalRule
    , DataRetrievalRule
    , dataRetrievalRule
    , drrStrategy
    , drrBytesPerHour

    -- ** DescribeVaultOutput
    , DescribeVaultOutput
    , describeVaultOutput
    , dvoVaultName
    , dvoSizeInBytes
    , dvoLastInventoryDate
    , dvoVaultARN
    , dvoCreationDate
    , dvoNumberOfArchives

    -- ** GlacierJobDescription
    , GlacierJobDescription
    , glacierJobDescription
    , gjdArchiveId
    , gjdSHA256TreeHash
    , gjdJobId
    , gjdRetrievalByteRange
    , gjdInventoryRetrievalParameters
    , gjdAction
    , gjdJobDescription
    , gjdSNSTopic
    , gjdVaultARN
    , gjdStatusMessage
    , gjdArchiveSHA256TreeHash
    , gjdCreationDate
    , gjdCompleted
    , gjdCompletionDate
    , gjdArchiveSizeInBytes
    , gjdStatusCode
    , gjdInventorySizeInBytes

    -- ** InventoryRetrievalJobDescription
    , InventoryRetrievalJobDescription
    , inventoryRetrievalJobDescription
    , irjdFormat
    , irjdEndDate
    , irjdStartDate
    , irjdMarker
    , irjdLimit

    -- ** InventoryRetrievalJobInput
    , InventoryRetrievalJobInput
    , inventoryRetrievalJobInput
    , irjiEndDate
    , irjiStartDate
    , irjiMarker
    , irjiLimit

    -- ** JobParameters
    , JobParameters
    , jobParameters
    , jpArchiveId
    , jpRetrievalByteRange
    , jpFormat
    , jpInventoryRetrievalParameters
    , jpSNSTopic
    , jpType
    , jpDescription

    -- ** PartListElement
    , PartListElement
    , partListElement
    , pleSHA256TreeHash
    , pleRangeInBytes

    -- ** UploadListElement
    , UploadListElement
    , uploadListElement
    , uleMultipartUploadId
    , uleArchiveDescription
    , ulePartSizeInBytes
    , uleVaultARN
    , uleCreationDate

    -- ** VaultAccessPolicy
    , VaultAccessPolicy
    , vaultAccessPolicy
    , vapPolicy

    -- ** VaultLockPolicy
    , VaultLockPolicy
    , vaultLockPolicy
    , vlpPolicy

    -- ** VaultNotificationConfig
    , VaultNotificationConfig
    , vaultNotificationConfig
    , vncSNSTopic
    , vncEvents
    ) where

import           Network.AWS.Glacier.AbortMultipartUpload
import           Network.AWS.Glacier.AbortVaultLock
import           Network.AWS.Glacier.AddTagsToVault
import           Network.AWS.Glacier.CompleteMultipartUpload
import           Network.AWS.Glacier.CompleteVaultLock
import           Network.AWS.Glacier.CreateVault
import           Network.AWS.Glacier.DeleteArchive
import           Network.AWS.Glacier.DeleteVault
import           Network.AWS.Glacier.DeleteVaultAccessPolicy
import           Network.AWS.Glacier.DeleteVaultNotifications
import           Network.AWS.Glacier.DescribeJob
import           Network.AWS.Glacier.DescribeVault
import           Network.AWS.Glacier.GetDataRetrievalPolicy
import           Network.AWS.Glacier.GetJobOutput
import           Network.AWS.Glacier.GetVaultAccessPolicy
import           Network.AWS.Glacier.GetVaultLock
import           Network.AWS.Glacier.GetVaultNotifications
import           Network.AWS.Glacier.InitiateJob
import           Network.AWS.Glacier.InitiateMultipartUpload
import           Network.AWS.Glacier.InitiateVaultLock
import           Network.AWS.Glacier.ListJobs
import           Network.AWS.Glacier.ListMultipartUploads
import           Network.AWS.Glacier.ListParts
import           Network.AWS.Glacier.ListTagsForVault
import           Network.AWS.Glacier.ListVaults
import           Network.AWS.Glacier.RemoveTagsFromVault
import           Network.AWS.Glacier.SetDataRetrievalPolicy
import           Network.AWS.Glacier.SetVaultAccessPolicy
import           Network.AWS.Glacier.SetVaultNotifications
import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.UploadArchive
import           Network.AWS.Glacier.UploadMultipartPart
import           Network.AWS.Glacier.Waiters

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

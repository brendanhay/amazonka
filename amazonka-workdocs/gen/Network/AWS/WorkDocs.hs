{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The WorkDocs API is designed for the following use cases:
--
--
--     * File Migration: File migration applications are supported for users who want to migrate their files from an on-premise or off-premise file system or service. Users can insert files into a user directory structure, as well as allow for basic metadata changes, such as modifications to the permissions of files.
--
--     * Security: Support security applications are supported for users who have additional security needs, such as anti-virus or data loss prevention. The APIs, in conjunction with Amazon CloudTrail, allow these applications to detect when changes occur in Amazon WorkDocs, so the application can take the necessary actions and replace the target file. The application can also choose to email the user if the target file violates the policy.
--
--     * eDiscovery/Analytics: General administrative applications are supported, such as eDiscovery and analytics. These applications can choose to mimic and/or record the actions in an Amazon WorkDocs site, in conjunction with Amazon CloudTrails, to replicate data for eDiscovery, backup, or analytical applications.
--
--
--
-- All Amazon WorkDocs APIs are Amazon authenticated, certificate-signed APIs. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkDocs site, the IAM user gains full administrative visibility into the entire Amazon WorkDocs site (or as set in the IAM policy). This includes, but is not limited to, the ability to modify file permissions and upload any file to any user. This allows developers to perform the three use cases above, as well as give users the ability to grant access on a selective basis using the IAM model.
--
module Network.AWS.WorkDocs
    (
    -- * Service Configuration
      workDocs

    -- * Errors
    -- $errors

    -- ** EntityAlreadyExistsException
    , _EntityAlreadyExistsException

    -- ** ProhibitedStateException
    , _ProhibitedStateException

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** UnauthorizedResourceAccessException
    , _UnauthorizedResourceAccessException

    -- ** TooManySubscriptionsException
    , _TooManySubscriptionsException

    -- ** FailedDependencyException
    , _FailedDependencyException

    -- ** EntityNotExistsException
    , _EntityNotExistsException

    -- ** DeactivatingLastSystemUserException
    , _DeactivatingLastSystemUserException

    -- ** IllegalUserStateException
    , _IllegalUserStateException

    -- ** StorageLimitWillExceedException
    , _StorageLimitWillExceedException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** StorageLimitExceededException
    , _StorageLimitExceededException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InvalidOperationException
    , _InvalidOperationException

    -- ** UnauthorizedOperationException
    , _UnauthorizedOperationException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AbortDocumentVersionUpload
    , module Network.AWS.WorkDocs.AbortDocumentVersionUpload

    -- ** GetDocumentPath
    , module Network.AWS.WorkDocs.GetDocumentPath

    -- ** DescribeUsers (Paginated)
    , module Network.AWS.WorkDocs.DescribeUsers

    -- ** DeleteFolder
    , module Network.AWS.WorkDocs.DeleteFolder

    -- ** UpdateFolder
    , module Network.AWS.WorkDocs.UpdateFolder

    -- ** DescribeResourcePermissions
    , module Network.AWS.WorkDocs.DescribeResourcePermissions

    -- ** DeleteNotificationSubscription
    , module Network.AWS.WorkDocs.DeleteNotificationSubscription

    -- ** CreateFolder
    , module Network.AWS.WorkDocs.CreateFolder

    -- ** CreateNotificationSubscription
    , module Network.AWS.WorkDocs.CreateNotificationSubscription

    -- ** GetFolderPath
    , module Network.AWS.WorkDocs.GetFolderPath

    -- ** DeleteFolderContents
    , module Network.AWS.WorkDocs.DeleteFolderContents

    -- ** RemoveAllResourcePermissions
    , module Network.AWS.WorkDocs.RemoveAllResourcePermissions

    -- ** GetFolder
    , module Network.AWS.WorkDocs.GetFolder

    -- ** DescribeNotificationSubscriptions
    , module Network.AWS.WorkDocs.DescribeNotificationSubscriptions

    -- ** ActivateUser
    , module Network.AWS.WorkDocs.ActivateUser

    -- ** DescribeDocumentVersions (Paginated)
    , module Network.AWS.WorkDocs.DescribeDocumentVersions

    -- ** GetDocumentVersion
    , module Network.AWS.WorkDocs.GetDocumentVersion

    -- ** DeactivateUser
    , module Network.AWS.WorkDocs.DeactivateUser

    -- ** GetDocument
    , module Network.AWS.WorkDocs.GetDocument

    -- ** DescribeFolderContents (Paginated)
    , module Network.AWS.WorkDocs.DescribeFolderContents

    -- ** UpdateDocumentVersion
    , module Network.AWS.WorkDocs.UpdateDocumentVersion

    -- ** RemoveResourcePermission
    , module Network.AWS.WorkDocs.RemoveResourcePermission

    -- ** InitiateDocumentVersionUpload
    , module Network.AWS.WorkDocs.InitiateDocumentVersionUpload

    -- ** CreateUser
    , module Network.AWS.WorkDocs.CreateUser

    -- ** UpdateUser
    , module Network.AWS.WorkDocs.UpdateUser

    -- ** DeleteUser
    , module Network.AWS.WorkDocs.DeleteUser

    -- ** AddResourcePermissions
    , module Network.AWS.WorkDocs.AddResourcePermissions

    -- ** UpdateDocument
    , module Network.AWS.WorkDocs.UpdateDocument

    -- ** DeleteDocument
    , module Network.AWS.WorkDocs.DeleteDocument

    -- * Types

    -- ** DocumentSourceType
    , DocumentSourceType (..)

    -- ** DocumentStatusType
    , DocumentStatusType (..)

    -- ** DocumentThumbnailType
    , DocumentThumbnailType (..)

    -- ** DocumentVersionStatus
    , DocumentVersionStatus (..)

    -- ** FolderContentType
    , FolderContentType (..)

    -- ** LocaleType
    , LocaleType (..)

    -- ** OrderType
    , OrderType (..)

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** ResourceSortType
    , ResourceSortType (..)

    -- ** ResourceStateType
    , ResourceStateType (..)

    -- ** RolePermissionType
    , RolePermissionType (..)

    -- ** RoleType
    , RoleType (..)

    -- ** ShareStatusType
    , ShareStatusType (..)

    -- ** StorageType
    , StorageType (..)

    -- ** SubscriptionProtocolType
    , SubscriptionProtocolType (..)

    -- ** SubscriptionType
    , SubscriptionType (..)

    -- ** UserFilterType
    , UserFilterType (..)

    -- ** UserSortType
    , UserSortType (..)

    -- ** UserStatusType
    , UserStatusType (..)

    -- ** UserType
    , UserType (..)

    -- ** DocumentMetadata
    , DocumentMetadata
    , documentMetadata
    , dmLatestVersionMetadata
    , dmParentFolderId
    , dmModifiedTimestamp
    , dmId
    , dmResourceState
    , dmCreatedTimestamp
    , dmCreatorId

    -- ** DocumentVersionMetadata
    , DocumentVersionMetadata
    , documentVersionMetadata
    , dvmThumbnail
    , dvmStatus
    , dvmSignature
    , dvmContentCreatedTimestamp
    , dvmSize
    , dvmName
    , dvmModifiedTimestamp
    , dvmSource
    , dvmId
    , dvmCreatedTimestamp
    , dvmContentModifiedTimestamp
    , dvmCreatorId
    , dvmContentType

    -- ** FolderMetadata
    , FolderMetadata
    , folderMetadata
    , fmSignature
    , fmParentFolderId
    , fmName
    , fmModifiedTimestamp
    , fmId
    , fmResourceState
    , fmCreatedTimestamp
    , fmCreatorId

    -- ** PermissionInfo
    , PermissionInfo
    , permissionInfo
    , piRole
    , piType

    -- ** Principal
    , Principal
    , principal
    , pRoles
    , pId
    , pType

    -- ** ResourcePath
    , ResourcePath
    , resourcePath
    , rpComponents

    -- ** ResourcePathComponent
    , ResourcePathComponent
    , resourcePathComponent
    , rpcName
    , rpcId

    -- ** SharePrincipal
    , SharePrincipal
    , sharePrincipal
    , spId
    , spType
    , spRole

    -- ** ShareResult
    , ShareResult
    , shareResult
    , srStatus
    , srPrincipalId
    , srRole
    , srStatusMessage
    , srShareId

    -- ** StorageRuleType
    , StorageRuleType
    , storageRuleType
    , srtStorageAllocatedInBytes
    , srtStorageType

    -- ** Subscription
    , Subscription
    , subscription
    , sProtocol
    , sEndPoint
    , sSubscriptionId

    -- ** UploadMetadata
    , UploadMetadata
    , uploadMetadata
    , umUploadURL
    , umSignedHeaders

    -- ** User
    , User
    , user
    , uGivenName
    , uStatus
    , uLocale
    , uUsername
    , uStorage
    , uModifiedTimestamp
    , uEmailAddress
    , uId
    , uRootFolderId
    , uType
    , uSurname
    , uTimeZoneId
    , uCreatedTimestamp
    , uOrganizationId
    , uRecycleBinFolderId

    -- ** UserStorageMetadata
    , UserStorageMetadata
    , userStorageMetadata
    , usmStorageUtilizedInBytes
    , usmStorageRule
    ) where

import           Network.AWS.WorkDocs.AbortDocumentVersionUpload
import           Network.AWS.WorkDocs.ActivateUser
import           Network.AWS.WorkDocs.AddResourcePermissions
import           Network.AWS.WorkDocs.CreateFolder
import           Network.AWS.WorkDocs.CreateNotificationSubscription
import           Network.AWS.WorkDocs.CreateUser
import           Network.AWS.WorkDocs.DeactivateUser
import           Network.AWS.WorkDocs.DeleteDocument
import           Network.AWS.WorkDocs.DeleteFolder
import           Network.AWS.WorkDocs.DeleteFolderContents
import           Network.AWS.WorkDocs.DeleteNotificationSubscription
import           Network.AWS.WorkDocs.DeleteUser
import           Network.AWS.WorkDocs.DescribeDocumentVersions
import           Network.AWS.WorkDocs.DescribeFolderContents
import           Network.AWS.WorkDocs.DescribeNotificationSubscriptions
import           Network.AWS.WorkDocs.DescribeResourcePermissions
import           Network.AWS.WorkDocs.DescribeUsers
import           Network.AWS.WorkDocs.GetDocument
import           Network.AWS.WorkDocs.GetDocumentPath
import           Network.AWS.WorkDocs.GetDocumentVersion
import           Network.AWS.WorkDocs.GetFolder
import           Network.AWS.WorkDocs.GetFolderPath
import           Network.AWS.WorkDocs.InitiateDocumentVersionUpload
import           Network.AWS.WorkDocs.RemoveAllResourcePermissions
import           Network.AWS.WorkDocs.RemoveResourcePermission
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.UpdateDocument
import           Network.AWS.WorkDocs.UpdateDocumentVersion
import           Network.AWS.WorkDocs.UpdateFolder
import           Network.AWS.WorkDocs.UpdateUser
import           Network.AWS.WorkDocs.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WorkDocs'.
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

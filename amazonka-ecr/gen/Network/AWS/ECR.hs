{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic Container Registry (Amazon ECR) is a managed Docker registry service. Customers can use the familiar Docker CLI to push, pull, and manage images. Amazon ECR provides a secure, scalable, and reliable registry. Amazon ECR supports private Docker repositories with resource-based permissions using IAM so that specific users or Amazon EC2 instances can access repositories and images. Developers can use the Docker CLI to author and manage images.
--
--
module Network.AWS.ECR
    (
    -- * Service Configuration
      ecr

    -- * Errors
    -- $errors

    -- ** LayersNotFoundException
    , _LayersNotFoundException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** LayerAlreadyExistsException
    , _LayerAlreadyExistsException

    -- ** ServerException
    , _ServerException

    -- ** LayerInaccessibleException
    , _LayerInaccessibleException

    -- ** InvalidLayerException
    , _InvalidLayerException

    -- ** LayerPartTooSmallException
    , _LayerPartTooSmallException

    -- ** LifecyclePolicyPreviewNotFoundException
    , _LifecyclePolicyPreviewNotFoundException

    -- ** ImageNotFoundException
    , _ImageNotFoundException

    -- ** ImageAlreadyExistsException
    , _ImageAlreadyExistsException

    -- ** RepositoryNotFoundException
    , _RepositoryNotFoundException

    -- ** LifecyclePolicyPreviewInProgressException
    , _LifecyclePolicyPreviewInProgressException

    -- ** UploadNotFoundException
    , _UploadNotFoundException

    -- ** LifecyclePolicyNotFoundException
    , _LifecyclePolicyNotFoundException

    -- ** InvalidLayerPartException
    , _InvalidLayerPartException

    -- ** RepositoryNotEmptyException
    , _RepositoryNotEmptyException

    -- ** RepositoryAlreadyExistsException
    , _RepositoryAlreadyExistsException

    -- ** RepositoryPolicyNotFoundException
    , _RepositoryPolicyNotFoundException

    -- ** EmptyUploadException
    , _EmptyUploadException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetRepositoryPolicy
    , module Network.AWS.ECR.GetRepositoryPolicy

    -- ** PutLifecyclePolicy
    , module Network.AWS.ECR.PutLifecyclePolicy

    -- ** DeleteLifecyclePolicy
    , module Network.AWS.ECR.DeleteLifecyclePolicy

    -- ** BatchDeleteImage
    , module Network.AWS.ECR.BatchDeleteImage

    -- ** GetLifecyclePolicyPreview
    , module Network.AWS.ECR.GetLifecyclePolicyPreview

    -- ** BatchCheckLayerAvailability
    , module Network.AWS.ECR.BatchCheckLayerAvailability

    -- ** DeleteRepositoryPolicy
    , module Network.AWS.ECR.DeleteRepositoryPolicy

    -- ** CreateRepository
    , module Network.AWS.ECR.CreateRepository

    -- ** CompleteLayerUpload
    , module Network.AWS.ECR.CompleteLayerUpload

    -- ** DescribeRepositories (Paginated)
    , module Network.AWS.ECR.DescribeRepositories

    -- ** StartLifecyclePolicyPreview
    , module Network.AWS.ECR.StartLifecyclePolicyPreview

    -- ** UploadLayerPart
    , module Network.AWS.ECR.UploadLayerPart

    -- ** BatchGetImage
    , module Network.AWS.ECR.BatchGetImage

    -- ** GetLifecyclePolicy
    , module Network.AWS.ECR.GetLifecyclePolicy

    -- ** SetRepositoryPolicy
    , module Network.AWS.ECR.SetRepositoryPolicy

    -- ** InitiateLayerUpload
    , module Network.AWS.ECR.InitiateLayerUpload

    -- ** DeleteRepository
    , module Network.AWS.ECR.DeleteRepository

    -- ** PutImage
    , module Network.AWS.ECR.PutImage

    -- ** ListImages (Paginated)
    , module Network.AWS.ECR.ListImages

    -- ** GetAuthorizationToken
    , module Network.AWS.ECR.GetAuthorizationToken

    -- ** GetDownloadURLForLayer
    , module Network.AWS.ECR.GetDownloadURLForLayer

    -- ** DescribeImages (Paginated)
    , module Network.AWS.ECR.DescribeImages

    -- * Types

    -- ** ImageActionType
    , ImageActionType (..)

    -- ** ImageFailureCode
    , ImageFailureCode (..)

    -- ** LayerAvailability
    , LayerAvailability (..)

    -- ** LayerFailureCode
    , LayerFailureCode (..)

    -- ** LifecyclePolicyPreviewStatus
    , LifecyclePolicyPreviewStatus (..)

    -- ** TagStatus
    , TagStatus (..)

    -- ** AuthorizationData
    , AuthorizationData
    , authorizationData
    , adExpiresAt
    , adProxyEndpoint
    , adAuthorizationToken

    -- ** DescribeImagesFilter
    , DescribeImagesFilter
    , describeImagesFilter
    , difTagStatus

    -- ** Image
    , Image
    , image
    , iRegistryId
    , iImageId
    , iRepositoryName
    , iImageManifest

    -- ** ImageDetail
    , ImageDetail
    , imageDetail
    , idRegistryId
    , idImageTags
    , idImageSizeInBytes
    , idImageDigest
    , idImagePushedAt
    , idRepositoryName

    -- ** ImageFailure
    , ImageFailure
    , imageFailure
    , ifFailureReason
    , ifFailureCode
    , ifImageId

    -- ** ImageIdentifier
    , ImageIdentifier
    , imageIdentifier
    , iiImageDigest
    , iiImageTag

    -- ** Layer
    , Layer
    , layer
    , lMediaType
    , lLayerDigest
    , lLayerSize
    , lLayerAvailability

    -- ** LayerFailure
    , LayerFailure
    , layerFailure
    , lfFailureReason
    , lfFailureCode
    , lfLayerDigest

    -- ** LifecyclePolicyPreviewFilter
    , LifecyclePolicyPreviewFilter
    , lifecyclePolicyPreviewFilter
    , lppfTagStatus

    -- ** LifecyclePolicyPreviewResult
    , LifecyclePolicyPreviewResult
    , lifecyclePolicyPreviewResult
    , lpprImageTags
    , lpprAction
    , lpprImageDigest
    , lpprImagePushedAt
    , lpprAppliedRulePriority

    -- ** LifecyclePolicyPreviewSummary
    , LifecyclePolicyPreviewSummary
    , lifecyclePolicyPreviewSummary
    , lppsExpiringImageTotalCount

    -- ** LifecyclePolicyRuleAction
    , LifecyclePolicyRuleAction
    , lifecyclePolicyRuleAction
    , lpraType

    -- ** ListImagesFilter
    , ListImagesFilter
    , listImagesFilter
    , lifTagStatus

    -- ** Repository
    , Repository
    , repository
    , rRepositoryARN
    , rCreatedAt
    , rRegistryId
    , rRepositoryURI
    , rRepositoryName
    ) where

import Network.AWS.ECR.BatchCheckLayerAvailability
import Network.AWS.ECR.BatchDeleteImage
import Network.AWS.ECR.BatchGetImage
import Network.AWS.ECR.CompleteLayerUpload
import Network.AWS.ECR.CreateRepository
import Network.AWS.ECR.DeleteLifecyclePolicy
import Network.AWS.ECR.DeleteRepository
import Network.AWS.ECR.DeleteRepositoryPolicy
import Network.AWS.ECR.DescribeImages
import Network.AWS.ECR.DescribeRepositories
import Network.AWS.ECR.GetAuthorizationToken
import Network.AWS.ECR.GetDownloadURLForLayer
import Network.AWS.ECR.GetLifecyclePolicy
import Network.AWS.ECR.GetLifecyclePolicyPreview
import Network.AWS.ECR.GetRepositoryPolicy
import Network.AWS.ECR.InitiateLayerUpload
import Network.AWS.ECR.ListImages
import Network.AWS.ECR.PutImage
import Network.AWS.ECR.PutLifecyclePolicy
import Network.AWS.ECR.SetRepositoryPolicy
import Network.AWS.ECR.StartLifecyclePolicyPreview
import Network.AWS.ECR.Types
import Network.AWS.ECR.UploadLayerPart
import Network.AWS.ECR.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ECR'.
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

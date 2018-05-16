{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Resource Groups Tagging API__
--
-- This guide describes the API operations for the resource groups tagging.
--
-- A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.
--
-- Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. For more information about tagging, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/tag-editor.html Working with Tag Editor> and <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/resource-groups.html Working with Resource Groups> . For more information about permissions you need to use the resource groups tagging APIs, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-resource-groups.html Obtaining Permissions for Resource Groups > and <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html Obtaining Permissions for Tagging > .
--
-- You can use the resource groups tagging APIs to complete the following tasks:
--
--     * Tag and untag supported resources located in the specified region for the AWS account
--
--     * Use tag-based filters to search for resources located in the specified region for the AWS account
--
--     * List all existing tag keys in the specified region for the AWS account
--
--     * List all existing values for the specified key in the specified region for the AWS account
--
--
--
-- Not all resources can have tags. For a lists of resources that you can tag, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/supported-resources.html Supported Resources> in the /AWS Resource Groups and Tag Editor User Guide/ .
--
-- To make full use of the resource groups tagging APIs, you might need additional IAM permissions, including permission to access the resources of individual services as well as permission to view and apply tags to those resources. For more information, see <http://docs.aws.amazon.com/awsconsolehelpdocs/latest/gsg/obtaining-permissions-for-tagging.html Obtaining Permissions for Tagging> in the /AWS Resource Groups and Tag Editor User Guide/ .
--
module Network.AWS.ResourceGroupsTagging
    (
    -- * Service Configuration
      resourceGroupsTagging

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ThrottledException
    , _ThrottledException

    -- ** PaginationTokenExpiredException
    , _PaginationTokenExpiredException

    -- ** InternalServiceException
    , _InternalServiceException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetTagKeys (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagKeys

    -- ** TagResources
    , module Network.AWS.ResourceGroupsTagging.TagResources

    -- ** GetTagValues (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagValues

    -- ** GetResources (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetResources

    -- ** UntagResources
    , module Network.AWS.ResourceGroupsTagging.UntagResources

    -- * Types

    -- ** ResourceErrorCode
    , ResourceErrorCode (..)

    -- ** FailureInfo
    , FailureInfo
    , failureInfo
    , fiErrorCode
    , fiErrorMessage
    , fiStatusCode

    -- ** ResourceTagMapping
    , ResourceTagMapping
    , resourceTagMapping
    , rtmResourceARN
    , rtmTags

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfValues
    , tfKey
    ) where

import Network.AWS.ResourceGroupsTagging.GetResources
import Network.AWS.ResourceGroupsTagging.GetTagKeys
import Network.AWS.ResourceGroupsTagging.GetTagValues
import Network.AWS.ResourceGroupsTagging.TagResources
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.UntagResources
import Network.AWS.ResourceGroupsTagging.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ResourceGroupsTagging'.
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

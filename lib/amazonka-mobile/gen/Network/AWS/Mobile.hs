{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Mobile Service provides mobile app and website developers with capabilities required to configure AWS resources and bootstrap their developer desktop projects with the necessary SDKs, constants, tools and samples to make use of those resources.
--
--
module Network.AWS.Mobile
    (
    -- * Service Configuration
      mobile

    -- * Errors
    -- $errors

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** AccountActionRequiredException
    , _AccountActionRequiredException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    , module Network.AWS.Mobile.ListProjects

    -- ** DeleteProject
    , module Network.AWS.Mobile.DeleteProject

    -- ** UpdateProject
    , module Network.AWS.Mobile.UpdateProject

    -- ** ListBundles (Paginated)
    , module Network.AWS.Mobile.ListBundles

    -- ** DescribeProject
    , module Network.AWS.Mobile.DescribeProject

    -- ** ExportProject
    , module Network.AWS.Mobile.ExportProject

    -- ** DescribeBundle
    , module Network.AWS.Mobile.DescribeBundle

    -- ** ExportBundle
    , module Network.AWS.Mobile.ExportBundle

    -- ** CreateProject
    , module Network.AWS.Mobile.CreateProject

    -- * Types

    -- ** Platform
    , Platform (..)

    -- ** ProjectState
    , ProjectState (..)

    -- ** BundleDetails
    , BundleDetails
    , bundleDetails
    , bdAvailablePlatforms
    , bdBundleId
    , bdVersion
    , bdIconURL
    , bdTitle
    , bdDescription

    -- ** ProjectDetails
    , ProjectDetails
    , projectDetails
    , pdState
    , pdResources
    , pdCreatedDate
    , pdConsoleURL
    , pdName
    , pdRegion
    , pdProjectId
    , pdLastUpdatedDate

    -- ** ProjectSummary
    , ProjectSummary
    , projectSummary
    , psName
    , psProjectId

    -- ** Resource
    , Resource
    , resource
    , rFeature
    , rArn
    , rName
    , rAttributes
    , rType
    ) where

import Network.AWS.Mobile.CreateProject
import Network.AWS.Mobile.DeleteProject
import Network.AWS.Mobile.DescribeBundle
import Network.AWS.Mobile.DescribeProject
import Network.AWS.Mobile.ExportBundle
import Network.AWS.Mobile.ExportProject
import Network.AWS.Mobile.ListBundles
import Network.AWS.Mobile.ListProjects
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.UpdateProject
import Network.AWS.Mobile.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Mobile'.
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

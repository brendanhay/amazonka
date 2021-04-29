{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Mobile Service provides mobile app and website developers with
-- capabilities required to configure AWS resources and bootstrap their
-- developer desktop projects with the necessary SDKs, constants, tools and
-- samples to make use of those resources.
module Network.AWS.Mobile
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** AccountActionRequiredException
    _AccountActionRequiredException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** DescribeBundle
    DescribeBundle (DescribeBundle'),
    newDescribeBundle,
    DescribeBundleResponse (DescribeBundleResponse'),
    newDescribeBundleResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** ListBundles (Paginated)
    ListBundles (ListBundles'),
    newListBundles,
    ListBundlesResponse (ListBundlesResponse'),
    newListBundlesResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** ExportProject
    ExportProject (ExportProject'),
    newExportProject,
    ExportProjectResponse (ExportProjectResponse'),
    newExportProjectResponse,

    -- ** ExportBundle
    ExportBundle (ExportBundle'),
    newExportBundle,
    ExportBundleResponse (ExportBundleResponse'),
    newExportBundleResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- * Types

    -- ** Platform
    Platform (..),

    -- ** ProjectState
    ProjectState (..),

    -- ** BundleDetails
    BundleDetails (BundleDetails'),
    newBundleDetails,

    -- ** ProjectDetails
    ProjectDetails (ProjectDetails'),
    newProjectDetails,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,

    -- ** Resource
    Resource (Resource'),
    newResource,
  )
where

import Network.AWS.Mobile.CreateProject
import Network.AWS.Mobile.DeleteProject
import Network.AWS.Mobile.DescribeBundle
import Network.AWS.Mobile.DescribeProject
import Network.AWS.Mobile.ExportBundle
import Network.AWS.Mobile.ExportProject
import Network.AWS.Mobile.Lens
import Network.AWS.Mobile.ListBundles
import Network.AWS.Mobile.ListProjects
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.UpdateProject
import Network.AWS.Mobile.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Mobile'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

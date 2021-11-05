{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoT1ClickProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The AWS IoT 1-Click Projects API Reference
module Amazonka.IoT1ClickProjects
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DisassociateDeviceFromPlacement
    DisassociateDeviceFromPlacement (DisassociateDeviceFromPlacement'),
    newDisassociateDeviceFromPlacement,
    DisassociateDeviceFromPlacementResponse (DisassociateDeviceFromPlacementResponse'),
    newDisassociateDeviceFromPlacementResponse,

    -- ** CreatePlacement
    CreatePlacement (CreatePlacement'),
    newCreatePlacement,
    CreatePlacementResponse (CreatePlacementResponse'),
    newCreatePlacementResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** GetDevicesInPlacement
    GetDevicesInPlacement (GetDevicesInPlacement'),
    newGetDevicesInPlacement,
    GetDevicesInPlacementResponse (GetDevicesInPlacementResponse'),
    newGetDevicesInPlacementResponse,

    -- ** DescribePlacement
    DescribePlacement (DescribePlacement'),
    newDescribePlacement,
    DescribePlacementResponse (DescribePlacementResponse'),
    newDescribePlacementResponse,

    -- ** AssociateDeviceWithPlacement
    AssociateDeviceWithPlacement (AssociateDeviceWithPlacement'),
    newAssociateDeviceWithPlacement,
    AssociateDeviceWithPlacementResponse (AssociateDeviceWithPlacementResponse'),
    newAssociateDeviceWithPlacementResponse,

    -- ** DeletePlacement
    DeletePlacement (DeletePlacement'),
    newDeletePlacement,
    DeletePlacementResponse (DeletePlacementResponse'),
    newDeletePlacementResponse,

    -- ** UpdatePlacement
    UpdatePlacement (UpdatePlacement'),
    newUpdatePlacement,
    UpdatePlacementResponse (UpdatePlacementResponse'),
    newUpdatePlacementResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListPlacements (Paginated)
    ListPlacements (ListPlacements'),
    newListPlacements,
    ListPlacementsResponse (ListPlacementsResponse'),
    newListPlacementsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- * Types

    -- ** DeviceTemplate
    DeviceTemplate (DeviceTemplate'),
    newDeviceTemplate,

    -- ** PlacementDescription
    PlacementDescription (PlacementDescription'),
    newPlacementDescription,

    -- ** PlacementSummary
    PlacementSummary (PlacementSummary'),
    newPlacementSummary,

    -- ** PlacementTemplate
    PlacementTemplate (PlacementTemplate'),
    newPlacementTemplate,

    -- ** ProjectDescription
    ProjectDescription (ProjectDescription'),
    newProjectDescription,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,
  )
where

import Amazonka.IoT1ClickProjects.AssociateDeviceWithPlacement
import Amazonka.IoT1ClickProjects.CreatePlacement
import Amazonka.IoT1ClickProjects.CreateProject
import Amazonka.IoT1ClickProjects.DeletePlacement
import Amazonka.IoT1ClickProjects.DeleteProject
import Amazonka.IoT1ClickProjects.DescribePlacement
import Amazonka.IoT1ClickProjects.DescribeProject
import Amazonka.IoT1ClickProjects.DisassociateDeviceFromPlacement
import Amazonka.IoT1ClickProjects.GetDevicesInPlacement
import Amazonka.IoT1ClickProjects.Lens
import Amazonka.IoT1ClickProjects.ListPlacements
import Amazonka.IoT1ClickProjects.ListProjects
import Amazonka.IoT1ClickProjects.ListTagsForResource
import Amazonka.IoT1ClickProjects.TagResource
import Amazonka.IoT1ClickProjects.Types
import Amazonka.IoT1ClickProjects.UntagResource
import Amazonka.IoT1ClickProjects.UpdatePlacement
import Amazonka.IoT1ClickProjects.UpdateProject
import Amazonka.IoT1ClickProjects.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoT1ClickProjects'.

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

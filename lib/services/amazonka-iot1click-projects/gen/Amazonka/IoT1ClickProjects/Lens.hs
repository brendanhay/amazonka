{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT1ClickProjects.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Lens
  ( -- * Operations

    -- ** AssociateDeviceWithPlacement
    associateDeviceWithPlacement_projectName,
    associateDeviceWithPlacement_placementName,
    associateDeviceWithPlacement_deviceId,
    associateDeviceWithPlacement_deviceTemplateName,
    associateDeviceWithPlacementResponse_httpStatus,

    -- ** CreatePlacement
    createPlacement_attributes,
    createPlacement_placementName,
    createPlacement_projectName,
    createPlacementResponse_httpStatus,

    -- ** CreateProject
    createProject_description,
    createProject_placementTemplate,
    createProject_tags,
    createProject_projectName,
    createProjectResponse_httpStatus,

    -- ** DeletePlacement
    deletePlacement_placementName,
    deletePlacement_projectName,
    deletePlacementResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectName,
    deleteProjectResponse_httpStatus,

    -- ** DescribePlacement
    describePlacement_placementName,
    describePlacement_projectName,
    describePlacementResponse_httpStatus,
    describePlacementResponse_placement,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_httpStatus,
    describeProjectResponse_project,

    -- ** DisassociateDeviceFromPlacement
    disassociateDeviceFromPlacement_projectName,
    disassociateDeviceFromPlacement_placementName,
    disassociateDeviceFromPlacement_deviceTemplateName,
    disassociateDeviceFromPlacementResponse_httpStatus,

    -- ** GetDevicesInPlacement
    getDevicesInPlacement_projectName,
    getDevicesInPlacement_placementName,
    getDevicesInPlacementResponse_httpStatus,
    getDevicesInPlacementResponse_devices,

    -- ** ListPlacements
    listPlacements_maxResults,
    listPlacements_nextToken,
    listPlacements_projectName,
    listPlacementsResponse_nextToken,
    listPlacementsResponse_httpStatus,
    listPlacementsResponse_placements,

    -- ** ListProjects
    listProjects_maxResults,
    listProjects_nextToken,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePlacement
    updatePlacement_attributes,
    updatePlacement_placementName,
    updatePlacement_projectName,
    updatePlacementResponse_httpStatus,

    -- ** UpdateProject
    updateProject_description,
    updateProject_placementTemplate,
    updateProject_projectName,
    updateProjectResponse_httpStatus,

    -- * Types

    -- ** DeviceTemplate
    deviceTemplate_callbackOverrides,
    deviceTemplate_deviceType,

    -- ** PlacementDescription
    placementDescription_projectName,
    placementDescription_placementName,
    placementDescription_attributes,
    placementDescription_createdDate,
    placementDescription_updatedDate,

    -- ** PlacementSummary
    placementSummary_projectName,
    placementSummary_placementName,
    placementSummary_createdDate,
    placementSummary_updatedDate,

    -- ** PlacementTemplate
    placementTemplate_defaultAttributes,
    placementTemplate_deviceTemplates,

    -- ** ProjectDescription
    projectDescription_arn,
    projectDescription_description,
    projectDescription_placementTemplate,
    projectDescription_tags,
    projectDescription_projectName,
    projectDescription_createdDate,
    projectDescription_updatedDate,

    -- ** ProjectSummary
    projectSummary_arn,
    projectSummary_tags,
    projectSummary_projectName,
    projectSummary_createdDate,
    projectSummary_updatedDate,
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
import Amazonka.IoT1ClickProjects.ListPlacements
import Amazonka.IoT1ClickProjects.ListProjects
import Amazonka.IoT1ClickProjects.ListTagsForResource
import Amazonka.IoT1ClickProjects.TagResource
import Amazonka.IoT1ClickProjects.Types.DeviceTemplate
import Amazonka.IoT1ClickProjects.Types.PlacementDescription
import Amazonka.IoT1ClickProjects.Types.PlacementSummary
import Amazonka.IoT1ClickProjects.Types.PlacementTemplate
import Amazonka.IoT1ClickProjects.Types.ProjectDescription
import Amazonka.IoT1ClickProjects.Types.ProjectSummary
import Amazonka.IoT1ClickProjects.UntagResource
import Amazonka.IoT1ClickProjects.UpdatePlacement
import Amazonka.IoT1ClickProjects.UpdateProject

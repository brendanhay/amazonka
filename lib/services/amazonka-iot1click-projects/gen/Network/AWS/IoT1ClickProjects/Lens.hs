{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT1ClickProjects.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT1ClickProjects.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,

    -- ** DeleteProject
    deleteProject_projectName,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_placementTemplate,
    updateProject_description,
    updateProject_projectName,
    updateProjectResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DisassociateDeviceFromPlacement
    disassociateDeviceFromPlacement_projectName,
    disassociateDeviceFromPlacement_placementName,
    disassociateDeviceFromPlacement_deviceTemplateName,
    disassociateDeviceFromPlacementResponse_httpStatus,

    -- ** CreatePlacement
    createPlacement_attributes,
    createPlacement_placementName,
    createPlacement_projectName,
    createPlacementResponse_httpStatus,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_httpStatus,
    describeProjectResponse_project,

    -- ** GetDevicesInPlacement
    getDevicesInPlacement_projectName,
    getDevicesInPlacement_placementName,
    getDevicesInPlacementResponse_httpStatus,
    getDevicesInPlacementResponse_devices,

    -- ** DescribePlacement
    describePlacement_placementName,
    describePlacement_projectName,
    describePlacementResponse_httpStatus,
    describePlacementResponse_placement,

    -- ** AssociateDeviceWithPlacement
    associateDeviceWithPlacement_projectName,
    associateDeviceWithPlacement_placementName,
    associateDeviceWithPlacement_deviceId,
    associateDeviceWithPlacement_deviceTemplateName,
    associateDeviceWithPlacementResponse_httpStatus,

    -- ** DeletePlacement
    deletePlacement_placementName,
    deletePlacement_projectName,
    deletePlacementResponse_httpStatus,

    -- ** UpdatePlacement
    updatePlacement_attributes,
    updatePlacement_placementName,
    updatePlacement_projectName,
    updatePlacementResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListPlacements
    listPlacements_nextToken,
    listPlacements_maxResults,
    listPlacements_projectName,
    listPlacementsResponse_nextToken,
    listPlacementsResponse_httpStatus,
    listPlacementsResponse_placements,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateProject
    createProject_placementTemplate,
    createProject_description,
    createProject_tags,
    createProject_projectName,
    createProjectResponse_httpStatus,

    -- * Types

    -- ** DeviceTemplate
    deviceTemplate_deviceType,
    deviceTemplate_callbackOverrides,

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
    placementTemplate_deviceTemplates,
    placementTemplate_defaultAttributes,

    -- ** ProjectDescription
    projectDescription_arn,
    projectDescription_placementTemplate,
    projectDescription_description,
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

import Network.AWS.IoT1ClickProjects.AssociateDeviceWithPlacement
import Network.AWS.IoT1ClickProjects.CreatePlacement
import Network.AWS.IoT1ClickProjects.CreateProject
import Network.AWS.IoT1ClickProjects.DeletePlacement
import Network.AWS.IoT1ClickProjects.DeleteProject
import Network.AWS.IoT1ClickProjects.DescribePlacement
import Network.AWS.IoT1ClickProjects.DescribeProject
import Network.AWS.IoT1ClickProjects.DisassociateDeviceFromPlacement
import Network.AWS.IoT1ClickProjects.GetDevicesInPlacement
import Network.AWS.IoT1ClickProjects.ListPlacements
import Network.AWS.IoT1ClickProjects.ListProjects
import Network.AWS.IoT1ClickProjects.ListTagsForResource
import Network.AWS.IoT1ClickProjects.TagResource
import Network.AWS.IoT1ClickProjects.Types.DeviceTemplate
import Network.AWS.IoT1ClickProjects.Types.PlacementDescription
import Network.AWS.IoT1ClickProjects.Types.PlacementSummary
import Network.AWS.IoT1ClickProjects.Types.PlacementTemplate
import Network.AWS.IoT1ClickProjects.Types.ProjectDescription
import Network.AWS.IoT1ClickProjects.Types.ProjectSummary
import Network.AWS.IoT1ClickProjects.UntagResource
import Network.AWS.IoT1ClickProjects.UpdatePlacement
import Network.AWS.IoT1ClickProjects.UpdateProject

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** DescribeBundle
    describeBundle_bundleId,
    describeBundleResponse_details,
    describeBundleResponse_httpStatus,

    -- ** CreateProject
    createProject_contents,
    createProject_name,
    createProject_snapshotId,
    createProject_region,
    createProjectResponse_details,
    createProjectResponse_httpStatus,

    -- ** ListBundles
    listBundles_nextToken,
    listBundles_maxResults,
    listBundlesResponse_nextToken,
    listBundlesResponse_bundleList,
    listBundlesResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectId,
    deleteProjectResponse_deletedResources,
    deleteProjectResponse_orphanedResources,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_contents,
    updateProject_projectId,
    updateProjectResponse_details,
    updateProjectResponse_httpStatus,

    -- ** ExportProject
    exportProject_projectId,
    exportProjectResponse_downloadUrl,
    exportProjectResponse_snapshotId,
    exportProjectResponse_shareUrl,
    exportProjectResponse_httpStatus,

    -- ** ExportBundle
    exportBundle_platform,
    exportBundle_projectId,
    exportBundle_bundleId,
    exportBundleResponse_downloadUrl,
    exportBundleResponse_httpStatus,

    -- ** DescribeProject
    describeProject_syncFromResources,
    describeProject_projectId,
    describeProjectResponse_details,
    describeProjectResponse_httpStatus,

    -- * Types

    -- ** BundleDetails
    bundleDetails_bundleId,
    bundleDetails_title,
    bundleDetails_iconUrl,
    bundleDetails_version,
    bundleDetails_description,
    bundleDetails_availablePlatforms,

    -- ** ProjectDetails
    projectDetails_createdDate,
    projectDetails_lastUpdatedDate,
    projectDetails_projectId,
    projectDetails_state,
    projectDetails_name,
    projectDetails_consoleUrl,
    projectDetails_resources,
    projectDetails_region,

    -- ** ProjectSummary
    projectSummary_projectId,
    projectSummary_name,

    -- ** Resource
    resource_arn,
    resource_name,
    resource_attributes,
    resource_feature,
    resource_type,
  )
where

import Network.AWS.Mobile.CreateProject
import Network.AWS.Mobile.DeleteProject
import Network.AWS.Mobile.DescribeBundle
import Network.AWS.Mobile.DescribeProject
import Network.AWS.Mobile.ExportBundle
import Network.AWS.Mobile.ExportProject
import Network.AWS.Mobile.ListBundles
import Network.AWS.Mobile.ListProjects
import Network.AWS.Mobile.Types.BundleDetails
import Network.AWS.Mobile.Types.ProjectDetails
import Network.AWS.Mobile.Types.ProjectSummary
import Network.AWS.Mobile.Types.Resource
import Network.AWS.Mobile.UpdateProject

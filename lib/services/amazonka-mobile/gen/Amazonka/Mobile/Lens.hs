{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Mobile.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Lens
  ( -- * Operations

    -- ** CreateProject
    createProject_contents,
    createProject_name,
    createProject_region,
    createProject_snapshotId,
    createProjectResponse_details,
    createProjectResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectId,
    deleteProjectResponse_deletedResources,
    deleteProjectResponse_orphanedResources,
    deleteProjectResponse_httpStatus,

    -- ** DescribeBundle
    describeBundle_bundleId,
    describeBundleResponse_details,
    describeBundleResponse_httpStatus,

    -- ** DescribeProject
    describeProject_syncFromResources,
    describeProject_projectId,
    describeProjectResponse_details,
    describeProjectResponse_httpStatus,

    -- ** ExportBundle
    exportBundle_platform,
    exportBundle_projectId,
    exportBundle_bundleId,
    exportBundleResponse_downloadUrl,
    exportBundleResponse_httpStatus,

    -- ** ExportProject
    exportProject_projectId,
    exportProjectResponse_downloadUrl,
    exportProjectResponse_shareUrl,
    exportProjectResponse_snapshotId,
    exportProjectResponse_httpStatus,

    -- ** ListBundles
    listBundles_maxResults,
    listBundles_nextToken,
    listBundlesResponse_bundleList,
    listBundlesResponse_nextToken,
    listBundlesResponse_httpStatus,

    -- ** ListProjects
    listProjects_maxResults,
    listProjects_nextToken,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** UpdateProject
    updateProject_contents,
    updateProject_projectId,
    updateProjectResponse_details,
    updateProjectResponse_httpStatus,

    -- * Types

    -- ** BundleDetails
    bundleDetails_availablePlatforms,
    bundleDetails_bundleId,
    bundleDetails_description,
    bundleDetails_iconUrl,
    bundleDetails_title,
    bundleDetails_version,

    -- ** ProjectDetails
    projectDetails_consoleUrl,
    projectDetails_createdDate,
    projectDetails_lastUpdatedDate,
    projectDetails_name,
    projectDetails_projectId,
    projectDetails_region,
    projectDetails_resources,
    projectDetails_state,

    -- ** ProjectSummary
    projectSummary_name,
    projectSummary_projectId,

    -- ** Resource
    resource_arn,
    resource_attributes,
    resource_feature,
    resource_name,
    resource_type,
  )
where

import Amazonka.Mobile.CreateProject
import Amazonka.Mobile.DeleteProject
import Amazonka.Mobile.DescribeBundle
import Amazonka.Mobile.DescribeProject
import Amazonka.Mobile.ExportBundle
import Amazonka.Mobile.ExportProject
import Amazonka.Mobile.ListBundles
import Amazonka.Mobile.ListProjects
import Amazonka.Mobile.Types.BundleDetails
import Amazonka.Mobile.Types.ProjectDetails
import Amazonka.Mobile.Types.ProjectSummary
import Amazonka.Mobile.Types.Resource
import Amazonka.Mobile.UpdateProject

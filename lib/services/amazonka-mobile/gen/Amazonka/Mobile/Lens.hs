{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Mobile.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Lens
  ( -- * Operations

    -- ** CreateProject
    createProject_name,
    createProject_contents,
    createProject_snapshotId,
    createProject_region,
    createProjectResponse_details,
    createProjectResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectId,
    deleteProjectResponse_orphanedResources,
    deleteProjectResponse_deletedResources,
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
    exportBundle_projectId,
    exportBundle_platform,
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
    listBundles_nextToken,
    listBundles_maxResults,
    listBundlesResponse_nextToken,
    listBundlesResponse_bundleList,
    listBundlesResponse_httpStatus,

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjectsResponse_projects,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,

    -- ** UpdateProject
    updateProject_contents,
    updateProject_projectId,
    updateProjectResponse_details,
    updateProjectResponse_httpStatus,

    -- * Types

    -- ** BundleDetails
    bundleDetails_availablePlatforms,
    bundleDetails_description,
    bundleDetails_iconUrl,
    bundleDetails_title,
    bundleDetails_bundleId,
    bundleDetails_version,

    -- ** ProjectDetails
    projectDetails_name,
    projectDetails_consoleUrl,
    projectDetails_lastUpdatedDate,
    projectDetails_state,
    projectDetails_projectId,
    projectDetails_region,
    projectDetails_createdDate,
    projectDetails_resources,

    -- ** ProjectSummary
    projectSummary_name,
    projectSummary_projectId,

    -- ** Resource
    resource_name,
    resource_type,
    resource_arn,
    resource_feature,
    resource_attributes,
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

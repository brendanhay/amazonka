{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTFleetHub.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTFleetHub.Lens
  ( -- * Operations

    -- ** DescribeApplication
    describeApplication_applicationId,
    describeApplicationResponse_applicationDescription,
    describeApplicationResponse_ssoClientId,
    describeApplicationResponse_errorMessage,
    describeApplicationResponse_tags,
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationId,
    describeApplicationResponse_applicationArn,
    describeApplicationResponse_applicationName,
    describeApplicationResponse_applicationUrl,
    describeApplicationResponse_applicationState,
    describeApplicationResponse_applicationCreationDate,
    describeApplicationResponse_applicationLastUpdateDate,
    describeApplicationResponse_roleArn,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_clientToken,
    deleteApplication_applicationId,
    deleteApplicationResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_applicationDescription,
    updateApplication_clientToken,
    updateApplication_applicationName,
    updateApplication_applicationId,
    updateApplicationResponse_httpStatus,

    -- ** CreateApplication
    createApplication_applicationDescription,
    createApplication_clientToken,
    createApplication_tags,
    createApplication_applicationName,
    createApplication_roleArn,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationId,
    createApplicationResponse_applicationArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applicationSummaries,
    listApplicationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ApplicationSummary
    applicationSummary_applicationDescription,
    applicationSummary_applicationState,
    applicationSummary_applicationCreationDate,
    applicationSummary_applicationLastUpdateDate,
    applicationSummary_applicationId,
    applicationSummary_applicationName,
    applicationSummary_applicationUrl,
  )
where

import Network.AWS.IoTFleetHub.CreateApplication
import Network.AWS.IoTFleetHub.DeleteApplication
import Network.AWS.IoTFleetHub.DescribeApplication
import Network.AWS.IoTFleetHub.ListApplications
import Network.AWS.IoTFleetHub.ListTagsForResource
import Network.AWS.IoTFleetHub.TagResource
import Network.AWS.IoTFleetHub.Types.ApplicationSummary
import Network.AWS.IoTFleetHub.UntagResource
import Network.AWS.IoTFleetHub.UpdateApplication

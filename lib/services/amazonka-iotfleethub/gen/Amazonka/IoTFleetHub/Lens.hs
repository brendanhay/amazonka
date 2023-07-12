{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTFleetHub.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetHub.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_applicationDescription,
    createApplication_clientToken,
    createApplication_tags,
    createApplication_applicationName,
    createApplication_roleArn,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationId,
    createApplicationResponse_applicationArn,

    -- ** DeleteApplication
    deleteApplication_clientToken,
    deleteApplication_applicationId,
    deleteApplicationResponse_httpStatus,

    -- ** DescribeApplication
    describeApplication_applicationId,
    describeApplicationResponse_applicationDescription,
    describeApplicationResponse_errorMessage,
    describeApplicationResponse_ssoClientId,
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

    -- ** ListApplications
    listApplications_nextToken,
    listApplicationsResponse_applicationSummaries,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

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

    -- ** UpdateApplication
    updateApplication_applicationDescription,
    updateApplication_applicationName,
    updateApplication_clientToken,
    updateApplication_applicationId,
    updateApplicationResponse_httpStatus,

    -- * Types

    -- ** ApplicationSummary
    applicationSummary_applicationCreationDate,
    applicationSummary_applicationDescription,
    applicationSummary_applicationLastUpdateDate,
    applicationSummary_applicationState,
    applicationSummary_applicationId,
    applicationSummary_applicationName,
    applicationSummary_applicationUrl,
  )
where

import Amazonka.IoTFleetHub.CreateApplication
import Amazonka.IoTFleetHub.DeleteApplication
import Amazonka.IoTFleetHub.DescribeApplication
import Amazonka.IoTFleetHub.ListApplications
import Amazonka.IoTFleetHub.ListTagsForResource
import Amazonka.IoTFleetHub.TagResource
import Amazonka.IoTFleetHub.Types.ApplicationSummary
import Amazonka.IoTFleetHub.UntagResource
import Amazonka.IoTFleetHub.UpdateApplication

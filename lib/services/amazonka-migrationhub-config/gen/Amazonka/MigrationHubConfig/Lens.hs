{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubConfig.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubConfig.Lens
  ( -- * Operations

    -- ** CreateHomeRegionControl
    createHomeRegionControl_dryRun,
    createHomeRegionControl_homeRegion,
    createHomeRegionControl_target,
    createHomeRegionControlResponse_homeRegionControl,
    createHomeRegionControlResponse_httpStatus,

    -- ** DescribeHomeRegionControls
    describeHomeRegionControls_controlId,
    describeHomeRegionControls_homeRegion,
    describeHomeRegionControls_maxResults,
    describeHomeRegionControls_nextToken,
    describeHomeRegionControls_target,
    describeHomeRegionControlsResponse_homeRegionControls,
    describeHomeRegionControlsResponse_nextToken,
    describeHomeRegionControlsResponse_httpStatus,

    -- ** GetHomeRegion
    getHomeRegionResponse_homeRegion,
    getHomeRegionResponse_httpStatus,

    -- * Types

    -- ** HomeRegionControl
    homeRegionControl_controlId,
    homeRegionControl_homeRegion,
    homeRegionControl_requestedTime,
    homeRegionControl_target,

    -- ** Target
    target_id,
    target_type,
  )
where

import Amazonka.MigrationHubConfig.CreateHomeRegionControl
import Amazonka.MigrationHubConfig.DescribeHomeRegionControls
import Amazonka.MigrationHubConfig.GetHomeRegion
import Amazonka.MigrationHubConfig.Types.HomeRegionControl
import Amazonka.MigrationHubConfig.Types.Target

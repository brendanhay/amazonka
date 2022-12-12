{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MigrationHubConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-06-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The AWS Migration Hub home region APIs are available specifically for
-- working with your Migration Hub home region. You can use these APIs to
-- determine a home region, as well as to create and work with controls
-- that describe the home region.
--
-- -   You must make API calls for write actions (create, notify,
--     associate, disassociate, import, or put) while in your home region,
--     or a @HomeRegionNotSetException@ error is returned.
--
-- -   API calls for read actions (list, describe, stop, and delete) are
--     permitted outside of your home region.
--
-- -   If you call a write API outside the home region, an
--     @InvalidInputException@ is returned.
--
-- -   You can call @GetHomeRegion@ action to obtain the account\'s
--     Migration Hub home region.
--
-- For specific API usage, see the sections that follow in this AWS
-- Migration Hub Home Region API reference.
module Amazonka.MigrationHubConfig
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DryRunOperation
    _DryRunOperation,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateHomeRegionControl
    CreateHomeRegionControl (CreateHomeRegionControl'),
    newCreateHomeRegionControl,
    CreateHomeRegionControlResponse (CreateHomeRegionControlResponse'),
    newCreateHomeRegionControlResponse,

    -- ** DescribeHomeRegionControls
    DescribeHomeRegionControls (DescribeHomeRegionControls'),
    newDescribeHomeRegionControls,
    DescribeHomeRegionControlsResponse (DescribeHomeRegionControlsResponse'),
    newDescribeHomeRegionControlsResponse,

    -- ** GetHomeRegion
    GetHomeRegion (GetHomeRegion'),
    newGetHomeRegion,
    GetHomeRegionResponse (GetHomeRegionResponse'),
    newGetHomeRegionResponse,

    -- * Types

    -- ** TargetType
    TargetType (..),

    -- ** HomeRegionControl
    HomeRegionControl (HomeRegionControl'),
    newHomeRegionControl,

    -- ** Target
    Target (Target'),
    newTarget,
  )
where

import Amazonka.MigrationHubConfig.CreateHomeRegionControl
import Amazonka.MigrationHubConfig.DescribeHomeRegionControls
import Amazonka.MigrationHubConfig.GetHomeRegion
import Amazonka.MigrationHubConfig.Lens
import Amazonka.MigrationHubConfig.Types
import Amazonka.MigrationHubConfig.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MigrationHubConfig'.

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

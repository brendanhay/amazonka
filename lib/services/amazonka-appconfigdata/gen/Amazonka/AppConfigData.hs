{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppConfigData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-11-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AppConfig Data provides the data plane APIs your application uses to
-- retrieve configuration data. Here\'s how it works:
--
-- Your application retrieves configuration data by first establishing a
-- configuration session using the AppConfig Data StartConfigurationSession
-- API action. Your session\'s client then makes periodic calls to
-- GetLatestConfiguration to check for and retrieve the latest data
-- available.
--
-- When calling @StartConfigurationSession@, your code sends the following
-- information:
--
-- -   Identifiers (ID or name) of an AppConfig application, environment,
--     and configuration profile that the session tracks.
--
-- -   (Optional) The minimum amount of time the session\'s client must
--     wait between calls to @GetLatestConfiguration@.
--
-- In response, AppConfig provides an @InitialConfigurationToken@ to be
-- given to the session\'s client and used the first time it calls
-- @GetLatestConfiguration@ for that session.
--
-- When calling @GetLatestConfiguration@, your client code sends the most
-- recent @ConfigurationToken@ value it has and receives in response:
--
-- -   @NextPollConfigurationToken@: the @ConfigurationToken@ value to use
--     on the next call to @GetLatestConfiguration@.
--
-- -   @NextPollIntervalInSeconds@: the duration the client should wait
--     before making its next call to @GetLatestConfiguration@. This
--     duration may vary over the course of the session, so it should be
--     used instead of the value sent on the @StartConfigurationSession@
--     call.
--
-- -   The configuration: the latest data intended for the session. This
--     may be empty if the client already has the latest version of the
--     configuration.
--
-- For more information and to view example CLI commands that show how to
-- retrieve a configuration using the AppConfig Data
-- @StartConfigurationSession@ and @GetLatestConfiguration@ API actions,
-- see
-- <http://docs.aws.amazon.com/appconfig/latest/userguide/appconfig-retrieving-the-configuration Receiving the configuration>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfigData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetLatestConfiguration
    GetLatestConfiguration (GetLatestConfiguration'),
    newGetLatestConfiguration,
    GetLatestConfigurationResponse (GetLatestConfigurationResponse'),
    newGetLatestConfigurationResponse,

    -- ** StartConfigurationSession
    StartConfigurationSession (StartConfigurationSession'),
    newStartConfigurationSession,
    StartConfigurationSessionResponse (StartConfigurationSessionResponse'),
    newStartConfigurationSessionResponse,

    -- * Types
  )
where

import Amazonka.AppConfigData.GetLatestConfiguration
import Amazonka.AppConfigData.Lens
import Amazonka.AppConfigData.StartConfigurationSession
import Amazonka.AppConfigData.Types
import Amazonka.AppConfigData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppConfigData'.

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

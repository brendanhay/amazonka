{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ForecastQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-06-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides APIs for creating and managing Amazon Forecast resources.
module Amazonka.ForecastQuery
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** QueryForecast
    QueryForecast (QueryForecast'),
    newQueryForecast,
    QueryForecastResponse (QueryForecastResponse'),
    newQueryForecastResponse,

    -- ** QueryWhatIfForecast
    QueryWhatIfForecast (QueryWhatIfForecast'),
    newQueryWhatIfForecast,
    QueryWhatIfForecastResponse (QueryWhatIfForecastResponse'),
    newQueryWhatIfForecastResponse,

    -- * Types

    -- ** DataPoint
    DataPoint (DataPoint'),
    newDataPoint,

    -- ** Forecast
    Forecast (Forecast'),
    newForecast,
  )
where

import Amazonka.ForecastQuery.Lens
import Amazonka.ForecastQuery.QueryForecast
import Amazonka.ForecastQuery.QueryWhatIfForecast
import Amazonka.ForecastQuery.Types
import Amazonka.ForecastQuery.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ForecastQuery'.

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

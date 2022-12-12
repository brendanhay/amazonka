{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PersonalizeEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-03-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Personalize can consume real-time user event data, such as
-- /stream/ or /click/ data, and use it for model training either alone or
-- combined with historical data. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/recording-events.html Recording Events>.
module Amazonka.PersonalizeEvents
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutEvents
    PutEvents (PutEvents'),
    newPutEvents,
    PutEventsResponse (PutEventsResponse'),
    newPutEventsResponse,

    -- ** PutItems
    PutItems (PutItems'),
    newPutItems,
    PutItemsResponse (PutItemsResponse'),
    newPutItemsResponse,

    -- ** PutUsers
    PutUsers (PutUsers'),
    newPutUsers,
    PutUsersResponse (PutUsersResponse'),
    newPutUsersResponse,

    -- * Types

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Item
    Item (Item'),
    newItem,

    -- ** MetricAttribution
    MetricAttribution (MetricAttribution'),
    newMetricAttribution,

    -- ** User
    User (User'),
    newUser,
  )
where

import Amazonka.PersonalizeEvents.Lens
import Amazonka.PersonalizeEvents.PutEvents
import Amazonka.PersonalizeEvents.PutItems
import Amazonka.PersonalizeEvents.PutUsers
import Amazonka.PersonalizeEvents.Types
import Amazonka.PersonalizeEvents.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PersonalizeEvents'.

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

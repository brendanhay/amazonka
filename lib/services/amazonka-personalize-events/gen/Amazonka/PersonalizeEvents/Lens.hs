{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PersonalizeEvents.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeEvents.Lens
  ( -- * Operations

    -- ** PutEvents
    putEvents_userId,
    putEvents_trackingId,
    putEvents_sessionId,
    putEvents_eventList,

    -- ** PutItems
    putItems_datasetArn,
    putItems_items,

    -- ** PutUsers
    putUsers_datasetArn,
    putUsers_users,

    -- * Types

    -- ** Event
    event_eventId,
    event_eventValue,
    event_impression,
    event_itemId,
    event_metricAttribution,
    event_properties,
    event_recommendationId,
    event_eventType,
    event_sentAt,

    -- ** Item
    item_properties,
    item_itemId,

    -- ** MetricAttribution
    metricAttribution_eventAttributionSource,

    -- ** User
    user_properties,
    user_userId,
  )
where

import Amazonka.PersonalizeEvents.PutEvents
import Amazonka.PersonalizeEvents.PutItems
import Amazonka.PersonalizeEvents.PutUsers
import Amazonka.PersonalizeEvents.Types.Event
import Amazonka.PersonalizeEvents.Types.Item
import Amazonka.PersonalizeEvents.Types.MetricAttribution
import Amazonka.PersonalizeEvents.Types.User

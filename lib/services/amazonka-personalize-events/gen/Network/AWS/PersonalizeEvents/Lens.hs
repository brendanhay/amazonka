{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.PersonalizeEvents.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.PersonalizeEvents.Lens
  ( -- * Operations

    -- ** PutUsers
    putUsers_datasetArn,
    putUsers_users,

    -- ** PutItems
    putItems_datasetArn,
    putItems_items,

    -- ** PutEvents
    putEvents_userId,
    putEvents_trackingId,
    putEvents_sessionId,
    putEvents_eventList,

    -- * Types

    -- ** Event
    event_recommendationId,
    event_eventValue,
    event_itemId,
    event_impression,
    event_eventId,
    event_properties,
    event_eventType,
    event_sentAt,

    -- ** Item
    item_properties,
    item_itemId,

    -- ** User
    user_properties,
    user_userId,
  )
where

import Network.AWS.PersonalizeEvents.PutEvents
import Network.AWS.PersonalizeEvents.PutItems
import Network.AWS.PersonalizeEvents.PutUsers
import Network.AWS.PersonalizeEvents.Types.Event
import Network.AWS.PersonalizeEvents.Types.Item
import Network.AWS.PersonalizeEvents.Types.User

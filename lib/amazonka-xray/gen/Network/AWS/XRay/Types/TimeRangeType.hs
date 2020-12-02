{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TimeRangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TimeRangeType where

import Network.AWS.Prelude

data TimeRangeType
  = Event
  | TraceId
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText TimeRangeType where
  parser =
    takeLowerText >>= \case
      "event" -> pure Event
      "traceid" -> pure TraceId
      e ->
        fromTextError $
          "Failure parsing TimeRangeType from value: '" <> e
            <> "'. Accepted values: event, traceid"

instance ToText TimeRangeType where
  toText = \case
    Event -> "Event"
    TraceId -> "TraceId"

instance Hashable TimeRangeType

instance NFData TimeRangeType

instance ToByteString TimeRangeType

instance ToQuery TimeRangeType

instance ToHeader TimeRangeType

instance ToJSON TimeRangeType where
  toJSON = toJSONText

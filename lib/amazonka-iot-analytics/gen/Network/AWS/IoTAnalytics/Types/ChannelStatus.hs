{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatus where

import Network.AWS.Prelude

data ChannelStatus
  = CSActive
  | CSCreating
  | CSDeleting
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

instance FromText ChannelStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure CSActive
      "creating" -> pure CSCreating
      "deleting" -> pure CSDeleting
      e ->
        fromTextError $
          "Failure parsing ChannelStatus from value: '" <> e
            <> "'. Accepted values: active, creating, deleting"

instance ToText ChannelStatus where
  toText = \case
    CSActive -> "ACTIVE"
    CSCreating -> "CREATING"
    CSDeleting -> "DELETING"

instance Hashable ChannelStatus

instance NFData ChannelStatus

instance ToByteString ChannelStatus

instance ToQuery ChannelStatus

instance ToHeader ChannelStatus

instance FromJSON ChannelStatus where
  parseJSON = parseJSONText "ChannelStatus"

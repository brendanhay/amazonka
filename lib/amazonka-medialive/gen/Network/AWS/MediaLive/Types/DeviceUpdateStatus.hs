{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DeviceUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DeviceUpdateStatus where

import Network.AWS.Prelude

-- | The status of software on the input device.
data DeviceUpdateStatus
  = NotUpToDate
  | UpToDate
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

instance FromText DeviceUpdateStatus where
  parser =
    takeLowerText >>= \case
      "not_up_to_date" -> pure NotUpToDate
      "up_to_date" -> pure UpToDate
      e ->
        fromTextError $
          "Failure parsing DeviceUpdateStatus from value: '" <> e
            <> "'. Accepted values: not_up_to_date, up_to_date"

instance ToText DeviceUpdateStatus where
  toText = \case
    NotUpToDate -> "NOT_UP_TO_DATE"
    UpToDate -> "UP_TO_DATE"

instance Hashable DeviceUpdateStatus

instance NFData DeviceUpdateStatus

instance ToByteString DeviceUpdateStatus

instance ToQuery DeviceUpdateStatus

instance ToHeader DeviceUpdateStatus

instance FromJSON DeviceUpdateStatus where
  parseJSON = parseJSONText "DeviceUpdateStatus"

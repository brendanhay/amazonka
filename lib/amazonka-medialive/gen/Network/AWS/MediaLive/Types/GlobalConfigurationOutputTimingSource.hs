{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource where

import Network.AWS.Prelude

-- | Global Configuration Output Timing Source
data GlobalConfigurationOutputTimingSource
  = InputClock
  | SystemClock
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

instance FromText GlobalConfigurationOutputTimingSource where
  parser =
    takeLowerText >>= \case
      "input_clock" -> pure InputClock
      "system_clock" -> pure SystemClock
      e ->
        fromTextError $
          "Failure parsing GlobalConfigurationOutputTimingSource from value: '" <> e
            <> "'. Accepted values: input_clock, system_clock"

instance ToText GlobalConfigurationOutputTimingSource where
  toText = \case
    InputClock -> "INPUT_CLOCK"
    SystemClock -> "SYSTEM_CLOCK"

instance Hashable GlobalConfigurationOutputTimingSource

instance NFData GlobalConfigurationOutputTimingSource

instance ToByteString GlobalConfigurationOutputTimingSource

instance ToQuery GlobalConfigurationOutputTimingSource

instance ToHeader GlobalConfigurationOutputTimingSource

instance ToJSON GlobalConfigurationOutputTimingSource where
  toJSON = toJSONText

instance FromJSON GlobalConfigurationOutputTimingSource where
  parseJSON = parseJSONText "GlobalConfigurationOutputTimingSource"

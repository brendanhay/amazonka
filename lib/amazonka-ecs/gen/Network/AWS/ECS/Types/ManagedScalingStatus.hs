{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedScalingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ManagedScalingStatus where

import Network.AWS.Prelude

data ManagedScalingStatus
  = MSSDisabled
  | MSSEnabled
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

instance FromText ManagedScalingStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MSSDisabled
      "enabled" -> pure MSSEnabled
      e ->
        fromTextError $
          "Failure parsing ManagedScalingStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ManagedScalingStatus where
  toText = \case
    MSSDisabled -> "DISABLED"
    MSSEnabled -> "ENABLED"

instance Hashable ManagedScalingStatus

instance NFData ManagedScalingStatus

instance ToByteString ManagedScalingStatus

instance ToQuery ManagedScalingStatus

instance ToHeader ManagedScalingStatus

instance ToJSON ManagedScalingStatus where
  toJSON = toJSONText

instance FromJSON ManagedScalingStatus where
  parseJSON = parseJSONText "ManagedScalingStatus"

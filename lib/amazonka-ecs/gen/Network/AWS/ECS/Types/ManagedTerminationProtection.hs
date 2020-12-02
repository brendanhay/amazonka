{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedTerminationProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ManagedTerminationProtection where

import Network.AWS.Prelude

data ManagedTerminationProtection
  = MTPDisabled
  | MTPEnabled
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

instance FromText ManagedTerminationProtection where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MTPDisabled
      "enabled" -> pure MTPEnabled
      e ->
        fromTextError $
          "Failure parsing ManagedTerminationProtection from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ManagedTerminationProtection where
  toText = \case
    MTPDisabled -> "DISABLED"
    MTPEnabled -> "ENABLED"

instance Hashable ManagedTerminationProtection

instance NFData ManagedTerminationProtection

instance ToByteString ManagedTerminationProtection

instance ToQuery ManagedTerminationProtection

instance ToHeader ManagedTerminationProtection

instance ToJSON ManagedTerminationProtection where
  toJSON = toJSONText

instance FromJSON ManagedTerminationProtection where
  parseJSON = parseJSONText "ManagedTerminationProtection"

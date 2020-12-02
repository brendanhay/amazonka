{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetAction where

import Network.AWS.Prelude

data FleetAction = AutoScaling
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

instance FromText FleetAction where
  parser =
    takeLowerText >>= \case
      "auto_scaling" -> pure AutoScaling
      e ->
        fromTextError $
          "Failure parsing FleetAction from value: '" <> e
            <> "'. Accepted values: auto_scaling"

instance ToText FleetAction where
  toText = \case
    AutoScaling -> "AUTO_SCALING"

instance Hashable FleetAction

instance NFData FleetAction

instance ToByteString FleetAction

instance ToQuery FleetAction

instance ToHeader FleetAction

instance ToJSON FleetAction where
  toJSON = toJSONText

instance FromJSON FleetAction where
  parseJSON = parseJSONText "FleetAction"

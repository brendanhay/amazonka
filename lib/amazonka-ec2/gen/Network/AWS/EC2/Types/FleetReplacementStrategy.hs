{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetReplacementStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetReplacementStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetReplacementStrategy = Launch
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

instance FromText FleetReplacementStrategy where
  parser =
    takeLowerText >>= \case
      "launch" -> pure Launch
      e ->
        fromTextError $
          "Failure parsing FleetReplacementStrategy from value: '" <> e
            <> "'. Accepted values: launch"

instance ToText FleetReplacementStrategy where
  toText = \case
    Launch -> "launch"

instance Hashable FleetReplacementStrategy

instance NFData FleetReplacementStrategy

instance ToByteString FleetReplacementStrategy

instance ToQuery FleetReplacementStrategy

instance ToHeader FleetReplacementStrategy

instance FromXML FleetReplacementStrategy where
  parseXML = parseXMLText "FleetReplacementStrategy"

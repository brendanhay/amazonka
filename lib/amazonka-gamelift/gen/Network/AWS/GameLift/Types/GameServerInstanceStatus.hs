{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerInstanceStatus where

import Network.AWS.Prelude

data GameServerInstanceStatus
  = GSISActive
  | GSISDraining
  | GSISSpotTerminating
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

instance FromText GameServerInstanceStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure GSISActive
      "draining" -> pure GSISDraining
      "spot_terminating" -> pure GSISSpotTerminating
      e ->
        fromTextError $
          "Failure parsing GameServerInstanceStatus from value: '" <> e
            <> "'. Accepted values: active, draining, spot_terminating"

instance ToText GameServerInstanceStatus where
  toText = \case
    GSISActive -> "ACTIVE"
    GSISDraining -> "DRAINING"
    GSISSpotTerminating -> "SPOT_TERMINATING"

instance Hashable GameServerInstanceStatus

instance NFData GameServerInstanceStatus

instance ToByteString GameServerInstanceStatus

instance ToQuery GameServerInstanceStatus

instance ToHeader GameServerInstanceStatus

instance FromJSON GameServerInstanceStatus where
  parseJSON = parseJSONText "GameServerInstanceStatus"

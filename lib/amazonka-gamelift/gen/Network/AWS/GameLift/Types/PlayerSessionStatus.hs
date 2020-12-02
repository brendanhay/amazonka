{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerSessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSessionStatus where

import Network.AWS.Prelude

data PlayerSessionStatus
  = PSSActive
  | PSSCompleted
  | PSSReserved
  | PSSTimedout
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

instance FromText PlayerSessionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure PSSActive
      "completed" -> pure PSSCompleted
      "reserved" -> pure PSSReserved
      "timedout" -> pure PSSTimedout
      e ->
        fromTextError $
          "Failure parsing PlayerSessionStatus from value: '" <> e
            <> "'. Accepted values: active, completed, reserved, timedout"

instance ToText PlayerSessionStatus where
  toText = \case
    PSSActive -> "ACTIVE"
    PSSCompleted -> "COMPLETED"
    PSSReserved -> "RESERVED"
    PSSTimedout -> "TIMEDOUT"

instance Hashable PlayerSessionStatus

instance NFData PlayerSessionStatus

instance ToByteString PlayerSessionStatus

instance ToQuery PlayerSessionStatus

instance ToHeader PlayerSessionStatus

instance FromJSON PlayerSessionStatus where
  parseJSON = parseJSONText "PlayerSessionStatus"

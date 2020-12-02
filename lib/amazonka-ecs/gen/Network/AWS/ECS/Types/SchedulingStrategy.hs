{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SchedulingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SchedulingStrategy where

import Network.AWS.Prelude

data SchedulingStrategy
  = Daemon
  | Replica
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

instance FromText SchedulingStrategy where
  parser =
    takeLowerText >>= \case
      "daemon" -> pure Daemon
      "replica" -> pure Replica
      e ->
        fromTextError $
          "Failure parsing SchedulingStrategy from value: '" <> e
            <> "'. Accepted values: daemon, replica"

instance ToText SchedulingStrategy where
  toText = \case
    Daemon -> "DAEMON"
    Replica -> "REPLICA"

instance Hashable SchedulingStrategy

instance NFData SchedulingStrategy

instance ToByteString SchedulingStrategy

instance ToQuery SchedulingStrategy

instance ToHeader SchedulingStrategy

instance ToJSON SchedulingStrategy where
  toJSON = toJSONText

instance FromJSON SchedulingStrategy where
  parseJSON = parseJSONText "SchedulingStrategy"

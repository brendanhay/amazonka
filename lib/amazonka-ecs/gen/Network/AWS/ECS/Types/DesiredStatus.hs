{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DesiredStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DesiredStatus where

import Network.AWS.Prelude

data DesiredStatus
  = Pending
  | Running
  | Stopped
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

instance FromText DesiredStatus where
  parser =
    takeLowerText >>= \case
      "pending" -> pure Pending
      "running" -> pure Running
      "stopped" -> pure Stopped
      e ->
        fromTextError $
          "Failure parsing DesiredStatus from value: '" <> e
            <> "'. Accepted values: pending, running, stopped"

instance ToText DesiredStatus where
  toText = \case
    Pending -> "PENDING"
    Running -> "RUNNING"
    Stopped -> "STOPPED"

instance Hashable DesiredStatus

instance NFData DesiredStatus

instance ToByteString DesiredStatus

instance ToQuery DesiredStatus

instance ToHeader DesiredStatus

instance ToJSON DesiredStatus where
  toJSON = toJSONText

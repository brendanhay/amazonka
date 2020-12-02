{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueType where

import Network.AWS.Prelude

data QueueType
  = Agent
  | Standard
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

instance FromText QueueType where
  parser =
    takeLowerText >>= \case
      "agent" -> pure Agent
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing QueueType from value: '" <> e
            <> "'. Accepted values: agent, standard"

instance ToText QueueType where
  toText = \case
    Agent -> "AGENT"
    Standard -> "STANDARD"

instance Hashable QueueType

instance NFData QueueType

instance ToByteString QueueType

instance ToQuery QueueType

instance ToHeader QueueType

instance ToJSON QueueType where
  toJSON = toJSONText

instance FromJSON QueueType where
  parseJSON = parseJSONText "QueueType"

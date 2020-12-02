{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Grouping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Grouping where

import Network.AWS.Prelude

data Grouping
  = Channel
  | Queue
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

instance FromText Grouping where
  parser =
    takeLowerText >>= \case
      "channel" -> pure Channel
      "queue" -> pure Queue
      e ->
        fromTextError $
          "Failure parsing Grouping from value: '" <> e
            <> "'. Accepted values: channel, queue"

instance ToText Grouping where
  toText = \case
    Channel -> "CHANNEL"
    Queue -> "QUEUE"

instance Hashable Grouping

instance NFData Grouping

instance ToByteString Grouping

instance ToQuery Grouping

instance ToHeader Grouping

instance ToJSON Grouping where
  toJSON = toJSONText

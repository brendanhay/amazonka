{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ActivityStreamMode where

import Network.AWS.Prelude

data ActivityStreamMode
  = Async
  | Sync
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

instance FromText ActivityStreamMode where
  parser =
    takeLowerText >>= \case
      "async" -> pure Async
      "sync" -> pure Sync
      e ->
        fromTextError $
          "Failure parsing ActivityStreamMode from value: '" <> e
            <> "'. Accepted values: async, sync"

instance ToText ActivityStreamMode where
  toText = \case
    Async -> "async"
    Sync -> "sync"

instance Hashable ActivityStreamMode

instance NFData ActivityStreamMode

instance ToByteString ActivityStreamMode

instance ToQuery ActivityStreamMode

instance ToHeader ActivityStreamMode

instance FromXML ActivityStreamMode where
  parseXML = parseXMLText "ActivityStreamMode"

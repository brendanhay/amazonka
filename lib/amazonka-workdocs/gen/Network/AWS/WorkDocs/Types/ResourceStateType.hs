{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceStateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceStateType where

import Network.AWS.Prelude

data ResourceStateType
  = RSTActive
  | RSTRecycled
  | RSTRecycling
  | RSTRestoring
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

instance FromText ResourceStateType where
  parser =
    takeLowerText >>= \case
      "active" -> pure RSTActive
      "recycled" -> pure RSTRecycled
      "recycling" -> pure RSTRecycling
      "restoring" -> pure RSTRestoring
      e ->
        fromTextError $
          "Failure parsing ResourceStateType from value: '" <> e
            <> "'. Accepted values: active, recycled, recycling, restoring"

instance ToText ResourceStateType where
  toText = \case
    RSTActive -> "ACTIVE"
    RSTRecycled -> "RECYCLED"
    RSTRecycling -> "RECYCLING"
    RSTRestoring -> "RESTORING"

instance Hashable ResourceStateType

instance NFData ResourceStateType

instance ToByteString ResourceStateType

instance ToQuery ResourceStateType

instance ToHeader ResourceStateType

instance ToJSON ResourceStateType where
  toJSON = toJSONText

instance FromJSON ResourceStateType where
  parseJSON = parseJSONText "ResourceStateType"

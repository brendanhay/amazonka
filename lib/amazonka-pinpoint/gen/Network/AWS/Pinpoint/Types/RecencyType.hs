{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecencyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecencyType where

import Network.AWS.Prelude

data RecencyType
  = Active
  | Inactive
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

instance FromText RecencyType where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing RecencyType from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText RecencyType where
  toText = \case
    Active -> "ACTIVE"
    Inactive -> "INACTIVE"

instance Hashable RecencyType

instance NFData RecencyType

instance ToByteString RecencyType

instance ToQuery RecencyType

instance ToHeader RecencyType

instance ToJSON RecencyType where
  toJSON = toJSONText

instance FromJSON RecencyType where
  parseJSON = parseJSONText "RecencyType"

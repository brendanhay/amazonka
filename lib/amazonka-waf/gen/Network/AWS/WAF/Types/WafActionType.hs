{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WafActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WafActionType where

import Network.AWS.Prelude

data WafActionType
  = Allow
  | Block
  | Count
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

instance FromText WafActionType where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "block" -> pure Block
      "count" -> pure Count
      e ->
        fromTextError $
          "Failure parsing WafActionType from value: '" <> e
            <> "'. Accepted values: allow, block, count"

instance ToText WafActionType where
  toText = \case
    Allow -> "ALLOW"
    Block -> "BLOCK"
    Count -> "COUNT"

instance Hashable WafActionType

instance NFData WafActionType

instance ToByteString WafActionType

instance ToQuery WafActionType

instance ToHeader WafActionType

instance ToJSON WafActionType where
  toJSON = toJSONText

instance FromJSON WafActionType where
  parseJSON = parseJSONText "WafActionType"

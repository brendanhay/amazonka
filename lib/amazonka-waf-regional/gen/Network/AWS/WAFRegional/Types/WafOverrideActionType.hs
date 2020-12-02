{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafOverrideActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafOverrideActionType where

import Network.AWS.Prelude

data WafOverrideActionType
  = WOATCount
  | WOATNone
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

instance FromText WafOverrideActionType where
  parser =
    takeLowerText >>= \case
      "count" -> pure WOATCount
      "none" -> pure WOATNone
      e ->
        fromTextError $
          "Failure parsing WafOverrideActionType from value: '" <> e
            <> "'. Accepted values: count, none"

instance ToText WafOverrideActionType where
  toText = \case
    WOATCount -> "COUNT"
    WOATNone -> "NONE"

instance Hashable WafOverrideActionType

instance NFData WafOverrideActionType

instance ToByteString WafOverrideActionType

instance ToQuery WafOverrideActionType

instance ToHeader WafOverrideActionType

instance ToJSON WafOverrideActionType where
  toJSON = toJSONText

instance FromJSON WafOverrideActionType where
  parseJSON = parseJSONText "WafOverrideActionType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WafRuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafRuleType where

import Network.AWS.Prelude

data WafRuleType
  = Group
  | RateBased
  | Regular
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

instance FromText WafRuleType where
  parser =
    takeLowerText >>= \case
      "group" -> pure Group
      "rate_based" -> pure RateBased
      "regular" -> pure Regular
      e ->
        fromTextError $
          "Failure parsing WafRuleType from value: '" <> e
            <> "'. Accepted values: group, rate_based, regular"

instance ToText WafRuleType where
  toText = \case
    Group -> "GROUP"
    RateBased -> "RATE_BASED"
    Regular -> "REGULAR"

instance Hashable WafRuleType

instance NFData WafRuleType

instance ToByteString WafRuleType

instance ToQuery WafRuleType

instance ToHeader WafRuleType

instance ToJSON WafRuleType where
  toJSON = toJSONText

instance FromJSON WafRuleType where
  parseJSON = parseJSONText "WafRuleType"

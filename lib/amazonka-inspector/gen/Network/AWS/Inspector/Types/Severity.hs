{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Severity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Severity where

import Network.AWS.Prelude

data Severity
  = High
  | Informational
  | Low
  | Medium
  | Undefined
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

instance FromText Severity where
  parser =
    takeLowerText >>= \case
      "high" -> pure High
      "informational" -> pure Informational
      "low" -> pure Low
      "medium" -> pure Medium
      "undefined" -> pure Undefined
      e ->
        fromTextError $
          "Failure parsing Severity from value: '" <> e
            <> "'. Accepted values: high, informational, low, medium, undefined"

instance ToText Severity where
  toText = \case
    High -> "High"
    Informational -> "Informational"
    Low -> "Low"
    Medium -> "Medium"
    Undefined -> "Undefined"

instance Hashable Severity

instance NFData Severity

instance ToByteString Severity

instance ToQuery Severity

instance ToHeader Severity

instance ToJSON Severity where
  toJSON = toJSONText

instance FromJSON Severity where
  parseJSON = parseJSONText "Severity"

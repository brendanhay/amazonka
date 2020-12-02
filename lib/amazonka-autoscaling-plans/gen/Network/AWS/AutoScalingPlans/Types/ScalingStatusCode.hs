{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingStatusCode where

import Network.AWS.Prelude

data ScalingStatusCode
  = Active
  | Inactive
  | PartiallyActive
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

instance FromText ScalingStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      "partiallyactive" -> pure PartiallyActive
      e ->
        fromTextError $
          "Failure parsing ScalingStatusCode from value: '" <> e
            <> "'. Accepted values: active, inactive, partiallyactive"

instance ToText ScalingStatusCode where
  toText = \case
    Active -> "Active"
    Inactive -> "Inactive"
    PartiallyActive -> "PartiallyActive"

instance Hashable ScalingStatusCode

instance NFData ScalingStatusCode

instance ToByteString ScalingStatusCode

instance ToQuery ScalingStatusCode

instance ToHeader ScalingStatusCode

instance FromJSON ScalingStatusCode where
  parseJSON = parseJSONText "ScalingStatusCode"

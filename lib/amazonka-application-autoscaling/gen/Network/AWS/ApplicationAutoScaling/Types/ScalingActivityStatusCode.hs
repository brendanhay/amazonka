{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode where

import Network.AWS.Prelude

data ScalingActivityStatusCode
  = Failed
  | InProgress
  | Overridden
  | Pending
  | Successful
  | Unfulfilled
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

instance FromText ScalingActivityStatusCode where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "inprogress" -> pure InProgress
      "overridden" -> pure Overridden
      "pending" -> pure Pending
      "successful" -> pure Successful
      "unfulfilled" -> pure Unfulfilled
      e ->
        fromTextError $
          "Failure parsing ScalingActivityStatusCode from value: '" <> e
            <> "'. Accepted values: failed, inprogress, overridden, pending, successful, unfulfilled"

instance ToText ScalingActivityStatusCode where
  toText = \case
    Failed -> "Failed"
    InProgress -> "InProgress"
    Overridden -> "Overridden"
    Pending -> "Pending"
    Successful -> "Successful"
    Unfulfilled -> "Unfulfilled"

instance Hashable ScalingActivityStatusCode

instance NFData ScalingActivityStatusCode

instance ToByteString ScalingActivityStatusCode

instance ToQuery ScalingActivityStatusCode

instance ToHeader ScalingActivityStatusCode

instance FromJSON ScalingActivityStatusCode where
  parseJSON = parseJSONText "ScalingActivityStatusCode"

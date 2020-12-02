{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PolicyType where

import Network.AWS.Prelude

data PolicyType = TargetTrackingScaling
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

instance FromText PolicyType where
  parser =
    takeLowerText >>= \case
      "targettrackingscaling" -> pure TargetTrackingScaling
      e ->
        fromTextError $
          "Failure parsing PolicyType from value: '" <> e
            <> "'. Accepted values: targettrackingscaling"

instance ToText PolicyType where
  toText = \case
    TargetTrackingScaling -> "TargetTrackingScaling"

instance Hashable PolicyType

instance NFData PolicyType

instance ToByteString PolicyType

instance ToQuery PolicyType

instance ToHeader PolicyType

instance FromJSON PolicyType where
  parseJSON = parseJSONText "PolicyType"

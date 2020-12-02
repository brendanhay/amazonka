{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior where

import Network.AWS.Prelude

data ScalingPolicyUpdateBehavior
  = KeepExternalPolicies
  | ReplaceExternalPolicies
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

instance FromText ScalingPolicyUpdateBehavior where
  parser =
    takeLowerText >>= \case
      "keepexternalpolicies" -> pure KeepExternalPolicies
      "replaceexternalpolicies" -> pure ReplaceExternalPolicies
      e ->
        fromTextError $
          "Failure parsing ScalingPolicyUpdateBehavior from value: '" <> e
            <> "'. Accepted values: keepexternalpolicies, replaceexternalpolicies"

instance ToText ScalingPolicyUpdateBehavior where
  toText = \case
    KeepExternalPolicies -> "KeepExternalPolicies"
    ReplaceExternalPolicies -> "ReplaceExternalPolicies"

instance Hashable ScalingPolicyUpdateBehavior

instance NFData ScalingPolicyUpdateBehavior

instance ToByteString ScalingPolicyUpdateBehavior

instance ToQuery ScalingPolicyUpdateBehavior

instance ToHeader ScalingPolicyUpdateBehavior

instance ToJSON ScalingPolicyUpdateBehavior where
  toJSON = toJSONText

instance FromJSON ScalingPolicyUpdateBehavior where
  parseJSON = parseJSONText "ScalingPolicyUpdateBehavior"

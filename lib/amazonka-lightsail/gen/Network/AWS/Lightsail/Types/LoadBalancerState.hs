{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerState where

import Network.AWS.Prelude

data LoadBalancerState
  = LBSActive
  | LBSActiveImpaired
  | LBSFailed
  | LBSProvisioning
  | LBSUnknown
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

instance FromText LoadBalancerState where
  parser =
    takeLowerText >>= \case
      "active" -> pure LBSActive
      "active_impaired" -> pure LBSActiveImpaired
      "failed" -> pure LBSFailed
      "provisioning" -> pure LBSProvisioning
      "unknown" -> pure LBSUnknown
      e ->
        fromTextError $
          "Failure parsing LoadBalancerState from value: '" <> e
            <> "'. Accepted values: active, active_impaired, failed, provisioning, unknown"

instance ToText LoadBalancerState where
  toText = \case
    LBSActive -> "active"
    LBSActiveImpaired -> "active_impaired"
    LBSFailed -> "failed"
    LBSProvisioning -> "provisioning"
    LBSUnknown -> "unknown"

instance Hashable LoadBalancerState

instance NFData LoadBalancerState

instance ToByteString LoadBalancerState

instance ToQuery LoadBalancerState

instance ToHeader LoadBalancerState

instance FromJSON LoadBalancerState where
  parseJSON = parseJSONText "LoadBalancerState"

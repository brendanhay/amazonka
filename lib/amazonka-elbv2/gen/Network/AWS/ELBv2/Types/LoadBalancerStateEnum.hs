{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerStateEnum where

import Network.AWS.Prelude

data LoadBalancerStateEnum
  = Active
  | ActiveImpaired
  | Failed
  | Provisioning
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

instance FromText LoadBalancerStateEnum where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "active_impaired" -> pure ActiveImpaired
      "failed" -> pure Failed
      "provisioning" -> pure Provisioning
      e ->
        fromTextError $
          "Failure parsing LoadBalancerStateEnum from value: '" <> e
            <> "'. Accepted values: active, active_impaired, failed, provisioning"

instance ToText LoadBalancerStateEnum where
  toText = \case
    Active -> "active"
    ActiveImpaired -> "active_impaired"
    Failed -> "failed"
    Provisioning -> "provisioning"

instance Hashable LoadBalancerStateEnum

instance NFData LoadBalancerStateEnum

instance ToByteString LoadBalancerStateEnum

instance ToQuery LoadBalancerStateEnum

instance ToHeader LoadBalancerStateEnum

instance FromXML LoadBalancerStateEnum where
  parseXML = parseXMLText "LoadBalancerStateEnum"

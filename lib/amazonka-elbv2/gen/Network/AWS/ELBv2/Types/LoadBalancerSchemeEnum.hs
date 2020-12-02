{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum where

import Network.AWS.Prelude

data LoadBalancerSchemeEnum
  = Internal
  | InternetFacing
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

instance FromText LoadBalancerSchemeEnum where
  parser =
    takeLowerText >>= \case
      "internal" -> pure Internal
      "internet-facing" -> pure InternetFacing
      e ->
        fromTextError $
          "Failure parsing LoadBalancerSchemeEnum from value: '" <> e
            <> "'. Accepted values: internal, internet-facing"

instance ToText LoadBalancerSchemeEnum where
  toText = \case
    Internal -> "internal"
    InternetFacing -> "internet-facing"

instance Hashable LoadBalancerSchemeEnum

instance NFData LoadBalancerSchemeEnum

instance ToByteString LoadBalancerSchemeEnum

instance ToQuery LoadBalancerSchemeEnum

instance ToHeader LoadBalancerSchemeEnum

instance FromXML LoadBalancerSchemeEnum where
  parseXML = parseXMLText "LoadBalancerSchemeEnum"

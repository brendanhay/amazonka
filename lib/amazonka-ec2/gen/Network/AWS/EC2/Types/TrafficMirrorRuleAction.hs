{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorRuleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorRuleAction where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorRuleAction
  = Accept
  | Reject
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

instance FromText TrafficMirrorRuleAction where
  parser =
    takeLowerText >>= \case
      "accept" -> pure Accept
      "reject" -> pure Reject
      e ->
        fromTextError $
          "Failure parsing TrafficMirrorRuleAction from value: '" <> e
            <> "'. Accepted values: accept, reject"

instance ToText TrafficMirrorRuleAction where
  toText = \case
    Accept -> "accept"
    Reject -> "reject"

instance Hashable TrafficMirrorRuleAction

instance NFData TrafficMirrorRuleAction

instance ToByteString TrafficMirrorRuleAction

instance ToQuery TrafficMirrorRuleAction

instance ToHeader TrafficMirrorRuleAction

instance FromXML TrafficMirrorRuleAction where
  parseXML = parseXMLText "TrafficMirrorRuleAction"

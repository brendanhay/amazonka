{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilterRuleField where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorFilterRuleField
  = TMFRFDescription
  | TMFRFDestinationPortRange
  | TMFRFProtocol
  | TMFRFSourcePortRange
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

instance FromText TrafficMirrorFilterRuleField where
  parser =
    takeLowerText >>= \case
      "description" -> pure TMFRFDescription
      "destination-port-range" -> pure TMFRFDestinationPortRange
      "protocol" -> pure TMFRFProtocol
      "source-port-range" -> pure TMFRFSourcePortRange
      e ->
        fromTextError $
          "Failure parsing TrafficMirrorFilterRuleField from value: '" <> e
            <> "'. Accepted values: description, destination-port-range, protocol, source-port-range"

instance ToText TrafficMirrorFilterRuleField where
  toText = \case
    TMFRFDescription -> "description"
    TMFRFDestinationPortRange -> "destination-port-range"
    TMFRFProtocol -> "protocol"
    TMFRFSourcePortRange -> "source-port-range"

instance Hashable TrafficMirrorFilterRuleField

instance NFData TrafficMirrorFilterRuleField

instance ToByteString TrafficMirrorFilterRuleField

instance ToQuery TrafficMirrorFilterRuleField

instance ToHeader TrafficMirrorFilterRuleField

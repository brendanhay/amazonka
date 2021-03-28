{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
  ( TrafficMirrorFilterRuleField
    ( TrafficMirrorFilterRuleField'
    , TrafficMirrorFilterRuleFieldDestinationPortRange
    , TrafficMirrorFilterRuleFieldSourcePortRange
    , TrafficMirrorFilterRuleFieldProtocol
    , TrafficMirrorFilterRuleFieldDescription
    , fromTrafficMirrorFilterRuleField
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TrafficMirrorFilterRuleField = TrafficMirrorFilterRuleField'{fromTrafficMirrorFilterRuleField
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern TrafficMirrorFilterRuleFieldDestinationPortRange :: TrafficMirrorFilterRuleField
pattern TrafficMirrorFilterRuleFieldDestinationPortRange = TrafficMirrorFilterRuleField' "destination-port-range"

pattern TrafficMirrorFilterRuleFieldSourcePortRange :: TrafficMirrorFilterRuleField
pattern TrafficMirrorFilterRuleFieldSourcePortRange = TrafficMirrorFilterRuleField' "source-port-range"

pattern TrafficMirrorFilterRuleFieldProtocol :: TrafficMirrorFilterRuleField
pattern TrafficMirrorFilterRuleFieldProtocol = TrafficMirrorFilterRuleField' "protocol"

pattern TrafficMirrorFilterRuleFieldDescription :: TrafficMirrorFilterRuleField
pattern TrafficMirrorFilterRuleFieldDescription = TrafficMirrorFilterRuleField' "description"

{-# COMPLETE 
  TrafficMirrorFilterRuleFieldDestinationPortRange,

  TrafficMirrorFilterRuleFieldSourcePortRange,

  TrafficMirrorFilterRuleFieldProtocol,

  TrafficMirrorFilterRuleFieldDescription,
  TrafficMirrorFilterRuleField'
  #-}

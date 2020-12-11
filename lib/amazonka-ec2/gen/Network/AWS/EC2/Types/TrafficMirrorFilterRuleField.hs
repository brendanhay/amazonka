-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilterRuleField
  ( TrafficMirrorFilterRuleField
      ( TrafficMirrorFilterRuleField',
        TMFRFDescription,
        TMFRFDestinationPortRange,
        TMFRFProtocol,
        TMFRFSourcePortRange
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrafficMirrorFilterRuleField = TrafficMirrorFilterRuleField' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TMFRFDescription :: TrafficMirrorFilterRuleField
pattern TMFRFDescription = TrafficMirrorFilterRuleField' "description"

pattern TMFRFDestinationPortRange :: TrafficMirrorFilterRuleField
pattern TMFRFDestinationPortRange = TrafficMirrorFilterRuleField' "destination-port-range"

pattern TMFRFProtocol :: TrafficMirrorFilterRuleField
pattern TMFRFProtocol = TrafficMirrorFilterRuleField' "protocol"

pattern TMFRFSourcePortRange :: TrafficMirrorFilterRuleField
pattern TMFRFSourcePortRange = TrafficMirrorFilterRuleField' "source-port-range"

{-# COMPLETE
  TMFRFDescription,
  TMFRFDestinationPortRange,
  TMFRFProtocol,
  TMFRFSourcePortRange,
  TrafficMirrorFilterRuleField'
  #-}

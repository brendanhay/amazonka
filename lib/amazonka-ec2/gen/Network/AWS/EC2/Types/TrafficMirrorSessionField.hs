{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSessionField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorSessionField
  ( TrafficMirrorSessionField
      ( TrafficMirrorSessionField',
        TrafficMirrorSessionFieldPacketLength,
        TrafficMirrorSessionFieldDescription,
        TrafficMirrorSessionFieldVirtualNetworkId,
        fromTrafficMirrorSessionField
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TrafficMirrorSessionField = TrafficMirrorSessionField'
  { fromTrafficMirrorSessionField ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TrafficMirrorSessionFieldPacketLength :: TrafficMirrorSessionField
pattern TrafficMirrorSessionFieldPacketLength = TrafficMirrorSessionField' "packet-length"

pattern TrafficMirrorSessionFieldDescription :: TrafficMirrorSessionField
pattern TrafficMirrorSessionFieldDescription = TrafficMirrorSessionField' "description"

pattern TrafficMirrorSessionFieldVirtualNetworkId :: TrafficMirrorSessionField
pattern TrafficMirrorSessionFieldVirtualNetworkId = TrafficMirrorSessionField' "virtual-network-id"

{-# COMPLETE
  TrafficMirrorSessionFieldPacketLength,
  TrafficMirrorSessionFieldDescription,
  TrafficMirrorSessionFieldVirtualNetworkId,
  TrafficMirrorSessionField'
  #-}

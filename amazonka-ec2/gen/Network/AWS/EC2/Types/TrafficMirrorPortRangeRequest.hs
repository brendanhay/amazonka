{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the Traffic Mirror filter rule port range.
--
-- /See:/ 'newTrafficMirrorPortRangeRequest' smart constructor.
data TrafficMirrorPortRangeRequest = TrafficMirrorPortRangeRequest'
  { -- | The first port in the Traffic Mirror port range. This applies to the TCP
    -- and UDP protocols.
    fromPort :: Core.Maybe Core.Int,
    -- | The last port in the Traffic Mirror port range. This applies to the TCP
    -- and UDP protocols.
    toPort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrafficMirrorPortRangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'trafficMirrorPortRangeRequest_fromPort' - The first port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
--
-- 'toPort', 'trafficMirrorPortRangeRequest_toPort' - The last port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
newTrafficMirrorPortRangeRequest ::
  TrafficMirrorPortRangeRequest
newTrafficMirrorPortRangeRequest =
  TrafficMirrorPortRangeRequest'
    { fromPort =
        Core.Nothing,
      toPort = Core.Nothing
    }

-- | The first port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
trafficMirrorPortRangeRequest_fromPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Core.Maybe Core.Int)
trafficMirrorPortRangeRequest_fromPort = Lens.lens (\TrafficMirrorPortRangeRequest' {fromPort} -> fromPort) (\s@TrafficMirrorPortRangeRequest' {} a -> s {fromPort = a} :: TrafficMirrorPortRangeRequest)

-- | The last port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
trafficMirrorPortRangeRequest_toPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Core.Maybe Core.Int)
trafficMirrorPortRangeRequest_toPort = Lens.lens (\TrafficMirrorPortRangeRequest' {toPort} -> toPort) (\s@TrafficMirrorPortRangeRequest' {} a -> s {toPort = a} :: TrafficMirrorPortRangeRequest)

instance Core.Hashable TrafficMirrorPortRangeRequest

instance Core.NFData TrafficMirrorPortRangeRequest

instance Core.ToQuery TrafficMirrorPortRangeRequest where
  toQuery TrafficMirrorPortRangeRequest' {..} =
    Core.mconcat
      [ "FromPort" Core.=: fromPort,
        "ToPort" Core.=: toPort
      ]

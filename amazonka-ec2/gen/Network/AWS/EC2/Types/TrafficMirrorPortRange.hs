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
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRange where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the Traffic Mirror port range.
--
-- /See:/ 'newTrafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
  { -- | The start of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    fromPort :: Core.Maybe Core.Int,
    -- | The end of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    toPort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrafficMirrorPortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'trafficMirrorPortRange_fromPort' - The start of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
--
-- 'toPort', 'trafficMirrorPortRange_toPort' - The end of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
newTrafficMirrorPortRange ::
  TrafficMirrorPortRange
newTrafficMirrorPortRange =
  TrafficMirrorPortRange'
    { fromPort = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The start of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_fromPort :: Lens.Lens' TrafficMirrorPortRange (Core.Maybe Core.Int)
trafficMirrorPortRange_fromPort = Lens.lens (\TrafficMirrorPortRange' {fromPort} -> fromPort) (\s@TrafficMirrorPortRange' {} a -> s {fromPort = a} :: TrafficMirrorPortRange)

-- | The end of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_toPort :: Lens.Lens' TrafficMirrorPortRange (Core.Maybe Core.Int)
trafficMirrorPortRange_toPort = Lens.lens (\TrafficMirrorPortRange' {toPort} -> toPort) (\s@TrafficMirrorPortRange' {} a -> s {toPort = a} :: TrafficMirrorPortRange)

instance Core.FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      Core.<$> (x Core..@? "fromPort")
      Core.<*> (x Core..@? "toPort")

instance Core.Hashable TrafficMirrorPortRange

instance Core.NFData TrafficMirrorPortRange

{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Traffic Mirror port range.
--
-- /See:/ 'newTrafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
  { -- | The start of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The end of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { fromPort = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The start of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_fromPort :: Lens.Lens' TrafficMirrorPortRange (Prelude.Maybe Prelude.Int)
trafficMirrorPortRange_fromPort = Lens.lens (\TrafficMirrorPortRange' {fromPort} -> fromPort) (\s@TrafficMirrorPortRange' {} a -> s {fromPort = a} :: TrafficMirrorPortRange)

-- | The end of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_toPort :: Lens.Lens' TrafficMirrorPortRange (Prelude.Maybe Prelude.Int)
trafficMirrorPortRange_toPort = Lens.lens (\TrafficMirrorPortRange' {toPort} -> toPort) (\s@TrafficMirrorPortRange' {} a -> s {toPort = a} :: TrafficMirrorPortRange)

instance Prelude.FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      Prelude.<$> (x Prelude..@? "fromPort")
      Prelude.<*> (x Prelude..@? "toPort")

instance Prelude.Hashable TrafficMirrorPortRange

instance Prelude.NFData TrafficMirrorPortRange

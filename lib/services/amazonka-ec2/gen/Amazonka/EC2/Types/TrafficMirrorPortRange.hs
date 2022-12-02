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
-- Module      : Amazonka.EC2.Types.TrafficMirrorPortRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorPortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the Traffic Mirror port range.
--
-- /See:/ 'newTrafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
  { -- | The end of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    toPort :: Prelude.Maybe Prelude.Int,
    -- | The start of the Traffic Mirror port range. This applies to the TCP and
    -- UDP protocols.
    fromPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficMirrorPortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'toPort', 'trafficMirrorPortRange_toPort' - The end of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
--
-- 'fromPort', 'trafficMirrorPortRange_fromPort' - The start of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
newTrafficMirrorPortRange ::
  TrafficMirrorPortRange
newTrafficMirrorPortRange =
  TrafficMirrorPortRange'
    { toPort = Prelude.Nothing,
      fromPort = Prelude.Nothing
    }

-- | The end of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_toPort :: Lens.Lens' TrafficMirrorPortRange (Prelude.Maybe Prelude.Int)
trafficMirrorPortRange_toPort = Lens.lens (\TrafficMirrorPortRange' {toPort} -> toPort) (\s@TrafficMirrorPortRange' {} a -> s {toPort = a} :: TrafficMirrorPortRange)

-- | The start of the Traffic Mirror port range. This applies to the TCP and
-- UDP protocols.
trafficMirrorPortRange_fromPort :: Lens.Lens' TrafficMirrorPortRange (Prelude.Maybe Prelude.Int)
trafficMirrorPortRange_fromPort = Lens.lens (\TrafficMirrorPortRange' {fromPort} -> fromPort) (\s@TrafficMirrorPortRange' {} a -> s {fromPort = a} :: TrafficMirrorPortRange)

instance Data.FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      Prelude.<$> (x Data..@? "toPort")
      Prelude.<*> (x Data..@? "fromPort")

instance Prelude.Hashable TrafficMirrorPortRange where
  hashWithSalt _salt TrafficMirrorPortRange' {..} =
    _salt `Prelude.hashWithSalt` toPort
      `Prelude.hashWithSalt` fromPort

instance Prelude.NFData TrafficMirrorPortRange where
  rnf TrafficMirrorPortRange' {..} =
    Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf fromPort

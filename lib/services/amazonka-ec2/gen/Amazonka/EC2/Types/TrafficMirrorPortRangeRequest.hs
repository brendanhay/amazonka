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
-- Module      : Amazonka.EC2.Types.TrafficMirrorPortRangeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorPortRangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the Traffic Mirror filter rule port range.
--
-- /See:/ 'newTrafficMirrorPortRangeRequest' smart constructor.
data TrafficMirrorPortRangeRequest = TrafficMirrorPortRangeRequest'
  { -- | The first port in the Traffic Mirror port range. This applies to the TCP
    -- and UDP protocols.
    fromPort :: Prelude.Maybe Prelude.Int,
    -- | The last port in the Traffic Mirror port range. This applies to the TCP
    -- and UDP protocols.
    toPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
trafficMirrorPortRangeRequest_fromPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Prelude.Maybe Prelude.Int)
trafficMirrorPortRangeRequest_fromPort = Lens.lens (\TrafficMirrorPortRangeRequest' {fromPort} -> fromPort) (\s@TrafficMirrorPortRangeRequest' {} a -> s {fromPort = a} :: TrafficMirrorPortRangeRequest)

-- | The last port in the Traffic Mirror port range. This applies to the TCP
-- and UDP protocols.
trafficMirrorPortRangeRequest_toPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Prelude.Maybe Prelude.Int)
trafficMirrorPortRangeRequest_toPort = Lens.lens (\TrafficMirrorPortRangeRequest' {toPort} -> toPort) (\s@TrafficMirrorPortRangeRequest' {} a -> s {toPort = a} :: TrafficMirrorPortRangeRequest)

instance
  Prelude.Hashable
    TrafficMirrorPortRangeRequest
  where
  hashWithSalt _salt TrafficMirrorPortRangeRequest' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData TrafficMirrorPortRangeRequest where
  rnf TrafficMirrorPortRangeRequest' {..} =
    Prelude.rnf fromPort `Prelude.seq`
      Prelude.rnf toPort

instance Data.ToQuery TrafficMirrorPortRangeRequest where
  toQuery TrafficMirrorPortRangeRequest' {..} =
    Prelude.mconcat
      [ "FromPort" Data.=: fromPort,
        "ToPort" Data.=: toPort
      ]

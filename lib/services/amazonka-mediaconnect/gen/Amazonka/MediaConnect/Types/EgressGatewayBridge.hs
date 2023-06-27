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
-- Module      : Amazonka.MediaConnect.Types.EgressGatewayBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.EgressGatewayBridge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newEgressGatewayBridge' smart constructor.
data EgressGatewayBridge = EgressGatewayBridge'
  { -- | The ID of the instance running this bridge.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum expected bitrate (in bps) of the egress bridge.
    maxBitrate :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EgressGatewayBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'egressGatewayBridge_instanceId' - The ID of the instance running this bridge.
--
-- 'maxBitrate', 'egressGatewayBridge_maxBitrate' - The maximum expected bitrate (in bps) of the egress bridge.
newEgressGatewayBridge ::
  -- | 'maxBitrate'
  Prelude.Int ->
  EgressGatewayBridge
newEgressGatewayBridge pMaxBitrate_ =
  EgressGatewayBridge'
    { instanceId = Prelude.Nothing,
      maxBitrate = pMaxBitrate_
    }

-- | The ID of the instance running this bridge.
egressGatewayBridge_instanceId :: Lens.Lens' EgressGatewayBridge (Prelude.Maybe Prelude.Text)
egressGatewayBridge_instanceId = Lens.lens (\EgressGatewayBridge' {instanceId} -> instanceId) (\s@EgressGatewayBridge' {} a -> s {instanceId = a} :: EgressGatewayBridge)

-- | The maximum expected bitrate (in bps) of the egress bridge.
egressGatewayBridge_maxBitrate :: Lens.Lens' EgressGatewayBridge Prelude.Int
egressGatewayBridge_maxBitrate = Lens.lens (\EgressGatewayBridge' {maxBitrate} -> maxBitrate) (\s@EgressGatewayBridge' {} a -> s {maxBitrate = a} :: EgressGatewayBridge)

instance Data.FromJSON EgressGatewayBridge where
  parseJSON =
    Data.withObject
      "EgressGatewayBridge"
      ( \x ->
          EgressGatewayBridge'
            Prelude.<$> (x Data..:? "instanceId")
            Prelude.<*> (x Data..: "maxBitrate")
      )

instance Prelude.Hashable EgressGatewayBridge where
  hashWithSalt _salt EgressGatewayBridge' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxBitrate

instance Prelude.NFData EgressGatewayBridge where
  rnf EgressGatewayBridge' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxBitrate

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
-- Module      : Amazonka.MediaConnect.Types.IngressGatewayBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.IngressGatewayBridge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newIngressGatewayBridge' smart constructor.
data IngressGatewayBridge = IngressGatewayBridge'
  { -- | The ID of the instance running this bridge.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of outputs on the ingress bridge.
    maxOutputs :: Prelude.Int,
    -- | The maximum expected bitrate (in bps) of the ingress bridge.
    maxBitrate :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngressGatewayBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'ingressGatewayBridge_instanceId' - The ID of the instance running this bridge.
--
-- 'maxOutputs', 'ingressGatewayBridge_maxOutputs' - The maximum number of outputs on the ingress bridge.
--
-- 'maxBitrate', 'ingressGatewayBridge_maxBitrate' - The maximum expected bitrate (in bps) of the ingress bridge.
newIngressGatewayBridge ::
  -- | 'maxOutputs'
  Prelude.Int ->
  -- | 'maxBitrate'
  Prelude.Int ->
  IngressGatewayBridge
newIngressGatewayBridge pMaxOutputs_ pMaxBitrate_ =
  IngressGatewayBridge'
    { instanceId = Prelude.Nothing,
      maxOutputs = pMaxOutputs_,
      maxBitrate = pMaxBitrate_
    }

-- | The ID of the instance running this bridge.
ingressGatewayBridge_instanceId :: Lens.Lens' IngressGatewayBridge (Prelude.Maybe Prelude.Text)
ingressGatewayBridge_instanceId = Lens.lens (\IngressGatewayBridge' {instanceId} -> instanceId) (\s@IngressGatewayBridge' {} a -> s {instanceId = a} :: IngressGatewayBridge)

-- | The maximum number of outputs on the ingress bridge.
ingressGatewayBridge_maxOutputs :: Lens.Lens' IngressGatewayBridge Prelude.Int
ingressGatewayBridge_maxOutputs = Lens.lens (\IngressGatewayBridge' {maxOutputs} -> maxOutputs) (\s@IngressGatewayBridge' {} a -> s {maxOutputs = a} :: IngressGatewayBridge)

-- | The maximum expected bitrate (in bps) of the ingress bridge.
ingressGatewayBridge_maxBitrate :: Lens.Lens' IngressGatewayBridge Prelude.Int
ingressGatewayBridge_maxBitrate = Lens.lens (\IngressGatewayBridge' {maxBitrate} -> maxBitrate) (\s@IngressGatewayBridge' {} a -> s {maxBitrate = a} :: IngressGatewayBridge)

instance Data.FromJSON IngressGatewayBridge where
  parseJSON =
    Data.withObject
      "IngressGatewayBridge"
      ( \x ->
          IngressGatewayBridge'
            Prelude.<$> (x Data..:? "instanceId")
            Prelude.<*> (x Data..: "maxOutputs")
            Prelude.<*> (x Data..: "maxBitrate")
      )

instance Prelude.Hashable IngressGatewayBridge where
  hashWithSalt _salt IngressGatewayBridge' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` maxOutputs
      `Prelude.hashWithSalt` maxBitrate

instance Prelude.NFData IngressGatewayBridge where
  rnf IngressGatewayBridge' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf maxOutputs
      `Prelude.seq` Prelude.rnf maxBitrate

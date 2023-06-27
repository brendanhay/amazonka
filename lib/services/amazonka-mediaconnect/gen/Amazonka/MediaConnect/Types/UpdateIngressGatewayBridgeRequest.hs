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
-- Module      : Amazonka.MediaConnect.Types.UpdateIngressGatewayBridgeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateIngressGatewayBridgeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUpdateIngressGatewayBridgeRequest' smart constructor.
data UpdateIngressGatewayBridgeRequest = UpdateIngressGatewayBridgeRequest'
  { -- | The maximum expected bitrate (in bps).
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of expected outputs.
    maxOutputs :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIngressGatewayBridgeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBitrate', 'updateIngressGatewayBridgeRequest_maxBitrate' - The maximum expected bitrate (in bps).
--
-- 'maxOutputs', 'updateIngressGatewayBridgeRequest_maxOutputs' - The maximum number of expected outputs.
newUpdateIngressGatewayBridgeRequest ::
  UpdateIngressGatewayBridgeRequest
newUpdateIngressGatewayBridgeRequest =
  UpdateIngressGatewayBridgeRequest'
    { maxBitrate =
        Prelude.Nothing,
      maxOutputs = Prelude.Nothing
    }

-- | The maximum expected bitrate (in bps).
updateIngressGatewayBridgeRequest_maxBitrate :: Lens.Lens' UpdateIngressGatewayBridgeRequest (Prelude.Maybe Prelude.Int)
updateIngressGatewayBridgeRequest_maxBitrate = Lens.lens (\UpdateIngressGatewayBridgeRequest' {maxBitrate} -> maxBitrate) (\s@UpdateIngressGatewayBridgeRequest' {} a -> s {maxBitrate = a} :: UpdateIngressGatewayBridgeRequest)

-- | The maximum number of expected outputs.
updateIngressGatewayBridgeRequest_maxOutputs :: Lens.Lens' UpdateIngressGatewayBridgeRequest (Prelude.Maybe Prelude.Int)
updateIngressGatewayBridgeRequest_maxOutputs = Lens.lens (\UpdateIngressGatewayBridgeRequest' {maxOutputs} -> maxOutputs) (\s@UpdateIngressGatewayBridgeRequest' {} a -> s {maxOutputs = a} :: UpdateIngressGatewayBridgeRequest)

instance
  Prelude.Hashable
    UpdateIngressGatewayBridgeRequest
  where
  hashWithSalt
    _salt
    UpdateIngressGatewayBridgeRequest' {..} =
      _salt
        `Prelude.hashWithSalt` maxBitrate
        `Prelude.hashWithSalt` maxOutputs

instance
  Prelude.NFData
    UpdateIngressGatewayBridgeRequest
  where
  rnf UpdateIngressGatewayBridgeRequest' {..} =
    Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf maxOutputs

instance
  Data.ToJSON
    UpdateIngressGatewayBridgeRequest
  where
  toJSON UpdateIngressGatewayBridgeRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("maxOutputs" Data..=) Prelude.<$> maxOutputs
          ]
      )

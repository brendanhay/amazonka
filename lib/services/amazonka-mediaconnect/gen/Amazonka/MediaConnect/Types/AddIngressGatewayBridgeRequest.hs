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
-- Module      : Amazonka.MediaConnect.Types.AddIngressGatewayBridgeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddIngressGatewayBridgeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAddIngressGatewayBridgeRequest' smart constructor.
data AddIngressGatewayBridgeRequest = AddIngressGatewayBridgeRequest'
  { -- | The maximum number of expected outputs.
    maxOutputs :: Prelude.Int,
    -- | The maximum expected bitrate (in bps).
    maxBitrate :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddIngressGatewayBridgeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxOutputs', 'addIngressGatewayBridgeRequest_maxOutputs' - The maximum number of expected outputs.
--
-- 'maxBitrate', 'addIngressGatewayBridgeRequest_maxBitrate' - The maximum expected bitrate (in bps).
newAddIngressGatewayBridgeRequest ::
  -- | 'maxOutputs'
  Prelude.Int ->
  -- | 'maxBitrate'
  Prelude.Int ->
  AddIngressGatewayBridgeRequest
newAddIngressGatewayBridgeRequest
  pMaxOutputs_
  pMaxBitrate_ =
    AddIngressGatewayBridgeRequest'
      { maxOutputs =
          pMaxOutputs_,
        maxBitrate = pMaxBitrate_
      }

-- | The maximum number of expected outputs.
addIngressGatewayBridgeRequest_maxOutputs :: Lens.Lens' AddIngressGatewayBridgeRequest Prelude.Int
addIngressGatewayBridgeRequest_maxOutputs = Lens.lens (\AddIngressGatewayBridgeRequest' {maxOutputs} -> maxOutputs) (\s@AddIngressGatewayBridgeRequest' {} a -> s {maxOutputs = a} :: AddIngressGatewayBridgeRequest)

-- | The maximum expected bitrate (in bps).
addIngressGatewayBridgeRequest_maxBitrate :: Lens.Lens' AddIngressGatewayBridgeRequest Prelude.Int
addIngressGatewayBridgeRequest_maxBitrate = Lens.lens (\AddIngressGatewayBridgeRequest' {maxBitrate} -> maxBitrate) (\s@AddIngressGatewayBridgeRequest' {} a -> s {maxBitrate = a} :: AddIngressGatewayBridgeRequest)

instance
  Prelude.Hashable
    AddIngressGatewayBridgeRequest
  where
  hashWithSalt
    _salt
    AddIngressGatewayBridgeRequest' {..} =
      _salt
        `Prelude.hashWithSalt` maxOutputs
        `Prelude.hashWithSalt` maxBitrate

instance
  Prelude.NFData
    AddIngressGatewayBridgeRequest
  where
  rnf AddIngressGatewayBridgeRequest' {..} =
    Prelude.rnf maxOutputs
      `Prelude.seq` Prelude.rnf maxBitrate

instance Data.ToJSON AddIngressGatewayBridgeRequest where
  toJSON AddIngressGatewayBridgeRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("maxOutputs" Data..= maxOutputs),
            Prelude.Just ("maxBitrate" Data..= maxBitrate)
          ]
      )

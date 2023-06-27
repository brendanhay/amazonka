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
-- Module      : Amazonka.MediaConnect.Types.UpdateEgressGatewayBridgeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateEgressGatewayBridgeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUpdateEgressGatewayBridgeRequest' smart constructor.
data UpdateEgressGatewayBridgeRequest = UpdateEgressGatewayBridgeRequest'
  { -- | Update an existing egress-type bridge.
    maxBitrate :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEgressGatewayBridgeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBitrate', 'updateEgressGatewayBridgeRequest_maxBitrate' - Update an existing egress-type bridge.
newUpdateEgressGatewayBridgeRequest ::
  UpdateEgressGatewayBridgeRequest
newUpdateEgressGatewayBridgeRequest =
  UpdateEgressGatewayBridgeRequest'
    { maxBitrate =
        Prelude.Nothing
    }

-- | Update an existing egress-type bridge.
updateEgressGatewayBridgeRequest_maxBitrate :: Lens.Lens' UpdateEgressGatewayBridgeRequest (Prelude.Maybe Prelude.Int)
updateEgressGatewayBridgeRequest_maxBitrate = Lens.lens (\UpdateEgressGatewayBridgeRequest' {maxBitrate} -> maxBitrate) (\s@UpdateEgressGatewayBridgeRequest' {} a -> s {maxBitrate = a} :: UpdateEgressGatewayBridgeRequest)

instance
  Prelude.Hashable
    UpdateEgressGatewayBridgeRequest
  where
  hashWithSalt
    _salt
    UpdateEgressGatewayBridgeRequest' {..} =
      _salt `Prelude.hashWithSalt` maxBitrate

instance
  Prelude.NFData
    UpdateEgressGatewayBridgeRequest
  where
  rnf UpdateEgressGatewayBridgeRequest' {..} =
    Prelude.rnf maxBitrate

instance Data.ToJSON UpdateEgressGatewayBridgeRequest where
  toJSON UpdateEgressGatewayBridgeRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("maxBitrate" Data..=) Prelude.<$> maxBitrate]
      )

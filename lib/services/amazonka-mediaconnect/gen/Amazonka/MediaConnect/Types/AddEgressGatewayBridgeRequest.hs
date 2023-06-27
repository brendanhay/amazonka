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
-- Module      : Amazonka.MediaConnect.Types.AddEgressGatewayBridgeRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddEgressGatewayBridgeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newAddEgressGatewayBridgeRequest' smart constructor.
data AddEgressGatewayBridgeRequest = AddEgressGatewayBridgeRequest'
  { -- | The maximum expected bitrate (in bps).
    maxBitrate :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddEgressGatewayBridgeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBitrate', 'addEgressGatewayBridgeRequest_maxBitrate' - The maximum expected bitrate (in bps).
newAddEgressGatewayBridgeRequest ::
  -- | 'maxBitrate'
  Prelude.Int ->
  AddEgressGatewayBridgeRequest
newAddEgressGatewayBridgeRequest pMaxBitrate_ =
  AddEgressGatewayBridgeRequest'
    { maxBitrate =
        pMaxBitrate_
    }

-- | The maximum expected bitrate (in bps).
addEgressGatewayBridgeRequest_maxBitrate :: Lens.Lens' AddEgressGatewayBridgeRequest Prelude.Int
addEgressGatewayBridgeRequest_maxBitrate = Lens.lens (\AddEgressGatewayBridgeRequest' {maxBitrate} -> maxBitrate) (\s@AddEgressGatewayBridgeRequest' {} a -> s {maxBitrate = a} :: AddEgressGatewayBridgeRequest)

instance
  Prelude.Hashable
    AddEgressGatewayBridgeRequest
  where
  hashWithSalt _salt AddEgressGatewayBridgeRequest' {..} =
    _salt `Prelude.hashWithSalt` maxBitrate

instance Prelude.NFData AddEgressGatewayBridgeRequest where
  rnf AddEgressGatewayBridgeRequest' {..} =
    Prelude.rnf maxBitrate

instance Data.ToJSON AddEgressGatewayBridgeRequest where
  toJSON AddEgressGatewayBridgeRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxBitrate" Data..= maxBitrate)]
      )

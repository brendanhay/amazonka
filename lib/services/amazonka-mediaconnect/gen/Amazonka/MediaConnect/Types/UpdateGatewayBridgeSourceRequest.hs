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
-- Module      : Amazonka.MediaConnect.Types.UpdateGatewayBridgeSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateGatewayBridgeSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The source configuration for cloud flows receiving a stream from a
-- bridge.
--
-- /See:/ 'newUpdateGatewayBridgeSourceRequest' smart constructor.
data UpdateGatewayBridgeSourceRequest = UpdateGatewayBridgeSourceRequest'
  { -- | The ARN of the bridge feeding this flow.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC interface attachment to use for this bridge source.
    vpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayBridgeSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'updateGatewayBridgeSourceRequest_bridgeArn' - The ARN of the bridge feeding this flow.
--
-- 'vpcInterfaceAttachment', 'updateGatewayBridgeSourceRequest_vpcInterfaceAttachment' - The name of the VPC interface attachment to use for this bridge source.
newUpdateGatewayBridgeSourceRequest ::
  UpdateGatewayBridgeSourceRequest
newUpdateGatewayBridgeSourceRequest =
  UpdateGatewayBridgeSourceRequest'
    { bridgeArn =
        Prelude.Nothing,
      vpcInterfaceAttachment = Prelude.Nothing
    }

-- | The ARN of the bridge feeding this flow.
updateGatewayBridgeSourceRequest_bridgeArn :: Lens.Lens' UpdateGatewayBridgeSourceRequest (Prelude.Maybe Prelude.Text)
updateGatewayBridgeSourceRequest_bridgeArn = Lens.lens (\UpdateGatewayBridgeSourceRequest' {bridgeArn} -> bridgeArn) (\s@UpdateGatewayBridgeSourceRequest' {} a -> s {bridgeArn = a} :: UpdateGatewayBridgeSourceRequest)

-- | The name of the VPC interface attachment to use for this bridge source.
updateGatewayBridgeSourceRequest_vpcInterfaceAttachment :: Lens.Lens' UpdateGatewayBridgeSourceRequest (Prelude.Maybe VpcInterfaceAttachment)
updateGatewayBridgeSourceRequest_vpcInterfaceAttachment = Lens.lens (\UpdateGatewayBridgeSourceRequest' {vpcInterfaceAttachment} -> vpcInterfaceAttachment) (\s@UpdateGatewayBridgeSourceRequest' {} a -> s {vpcInterfaceAttachment = a} :: UpdateGatewayBridgeSourceRequest)

instance
  Prelude.Hashable
    UpdateGatewayBridgeSourceRequest
  where
  hashWithSalt
    _salt
    UpdateGatewayBridgeSourceRequest' {..} =
      _salt
        `Prelude.hashWithSalt` bridgeArn
        `Prelude.hashWithSalt` vpcInterfaceAttachment

instance
  Prelude.NFData
    UpdateGatewayBridgeSourceRequest
  where
  rnf UpdateGatewayBridgeSourceRequest' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf vpcInterfaceAttachment

instance Data.ToJSON UpdateGatewayBridgeSourceRequest where
  toJSON UpdateGatewayBridgeSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bridgeArn" Data..=) Prelude.<$> bridgeArn,
            ("vpcInterfaceAttachment" Data..=)
              Prelude.<$> vpcInterfaceAttachment
          ]
      )

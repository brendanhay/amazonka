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
-- Module      : Amazonka.MediaConnect.Types.SetGatewayBridgeSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.SetGatewayBridgeSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The source configuration for cloud flows receiving a stream from a
-- bridge.
--
-- /See:/ 'newSetGatewayBridgeSourceRequest' smart constructor.
data SetGatewayBridgeSourceRequest = SetGatewayBridgeSourceRequest'
  { -- | The name of the VPC interface attachment to use for this bridge source.
    vpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | The ARN of the bridge feeding this flow.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetGatewayBridgeSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcInterfaceAttachment', 'setGatewayBridgeSourceRequest_vpcInterfaceAttachment' - The name of the VPC interface attachment to use for this bridge source.
--
-- 'bridgeArn', 'setGatewayBridgeSourceRequest_bridgeArn' - The ARN of the bridge feeding this flow.
newSetGatewayBridgeSourceRequest ::
  -- | 'bridgeArn'
  Prelude.Text ->
  SetGatewayBridgeSourceRequest
newSetGatewayBridgeSourceRequest pBridgeArn_ =
  SetGatewayBridgeSourceRequest'
    { vpcInterfaceAttachment =
        Prelude.Nothing,
      bridgeArn = pBridgeArn_
    }

-- | The name of the VPC interface attachment to use for this bridge source.
setGatewayBridgeSourceRequest_vpcInterfaceAttachment :: Lens.Lens' SetGatewayBridgeSourceRequest (Prelude.Maybe VpcInterfaceAttachment)
setGatewayBridgeSourceRequest_vpcInterfaceAttachment = Lens.lens (\SetGatewayBridgeSourceRequest' {vpcInterfaceAttachment} -> vpcInterfaceAttachment) (\s@SetGatewayBridgeSourceRequest' {} a -> s {vpcInterfaceAttachment = a} :: SetGatewayBridgeSourceRequest)

-- | The ARN of the bridge feeding this flow.
setGatewayBridgeSourceRequest_bridgeArn :: Lens.Lens' SetGatewayBridgeSourceRequest Prelude.Text
setGatewayBridgeSourceRequest_bridgeArn = Lens.lens (\SetGatewayBridgeSourceRequest' {bridgeArn} -> bridgeArn) (\s@SetGatewayBridgeSourceRequest' {} a -> s {bridgeArn = a} :: SetGatewayBridgeSourceRequest)

instance
  Prelude.Hashable
    SetGatewayBridgeSourceRequest
  where
  hashWithSalt _salt SetGatewayBridgeSourceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` vpcInterfaceAttachment
      `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData SetGatewayBridgeSourceRequest where
  rnf SetGatewayBridgeSourceRequest' {..} =
    Prelude.rnf vpcInterfaceAttachment
      `Prelude.seq` Prelude.rnf bridgeArn

instance Data.ToJSON SetGatewayBridgeSourceRequest where
  toJSON SetGatewayBridgeSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vpcInterfaceAttachment" Data..=)
              Prelude.<$> vpcInterfaceAttachment,
            Prelude.Just ("bridgeArn" Data..= bridgeArn)
          ]
      )

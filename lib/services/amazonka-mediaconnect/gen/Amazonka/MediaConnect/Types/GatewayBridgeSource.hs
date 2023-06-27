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
-- Module      : Amazonka.MediaConnect.Types.GatewayBridgeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.GatewayBridgeSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The source configuration for cloud flows receiving a stream from a
-- bridge.
--
-- /See:/ 'newGatewayBridgeSource' smart constructor.
data GatewayBridgeSource = GatewayBridgeSource'
  { -- | The name of the VPC interface attachment to use for this bridge source.
    vpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | The ARN of the bridge feeding this flow.
    bridgeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayBridgeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcInterfaceAttachment', 'gatewayBridgeSource_vpcInterfaceAttachment' - The name of the VPC interface attachment to use for this bridge source.
--
-- 'bridgeArn', 'gatewayBridgeSource_bridgeArn' - The ARN of the bridge feeding this flow.
newGatewayBridgeSource ::
  -- | 'bridgeArn'
  Prelude.Text ->
  GatewayBridgeSource
newGatewayBridgeSource pBridgeArn_ =
  GatewayBridgeSource'
    { vpcInterfaceAttachment =
        Prelude.Nothing,
      bridgeArn = pBridgeArn_
    }

-- | The name of the VPC interface attachment to use for this bridge source.
gatewayBridgeSource_vpcInterfaceAttachment :: Lens.Lens' GatewayBridgeSource (Prelude.Maybe VpcInterfaceAttachment)
gatewayBridgeSource_vpcInterfaceAttachment = Lens.lens (\GatewayBridgeSource' {vpcInterfaceAttachment} -> vpcInterfaceAttachment) (\s@GatewayBridgeSource' {} a -> s {vpcInterfaceAttachment = a} :: GatewayBridgeSource)

-- | The ARN of the bridge feeding this flow.
gatewayBridgeSource_bridgeArn :: Lens.Lens' GatewayBridgeSource Prelude.Text
gatewayBridgeSource_bridgeArn = Lens.lens (\GatewayBridgeSource' {bridgeArn} -> bridgeArn) (\s@GatewayBridgeSource' {} a -> s {bridgeArn = a} :: GatewayBridgeSource)

instance Data.FromJSON GatewayBridgeSource where
  parseJSON =
    Data.withObject
      "GatewayBridgeSource"
      ( \x ->
          GatewayBridgeSource'
            Prelude.<$> (x Data..:? "vpcInterfaceAttachment")
            Prelude.<*> (x Data..: "bridgeArn")
      )

instance Prelude.Hashable GatewayBridgeSource where
  hashWithSalt _salt GatewayBridgeSource' {..} =
    _salt
      `Prelude.hashWithSalt` vpcInterfaceAttachment
      `Prelude.hashWithSalt` bridgeArn

instance Prelude.NFData GatewayBridgeSource where
  rnf GatewayBridgeSource' {..} =
    Prelude.rnf vpcInterfaceAttachment
      `Prelude.seq` Prelude.rnf bridgeArn

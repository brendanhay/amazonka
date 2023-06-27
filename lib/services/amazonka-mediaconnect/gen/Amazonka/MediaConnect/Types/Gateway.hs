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
-- Module      : Amazonka.MediaConnect.Types.Gateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Gateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.GatewayNetwork
import Amazonka.MediaConnect.Types.GatewayState
import Amazonka.MediaConnect.Types.MessageDetail
import qualified Amazonka.Prelude as Prelude

-- | The settings for a gateway, including its networks.
--
-- /See:/ 'newGateway' smart constructor.
data Gateway = Gateway'
  { gatewayMessages :: Prelude.Maybe [MessageDetail],
    -- | The current status of the gateway.
    gatewayState :: Prelude.Maybe GatewayState,
    -- | The Amazon Resource Name (ARN) of the gateway.
    gatewayArn :: Prelude.Text,
    -- | The list of networks in the gateway.
    networks :: [GatewayNetwork],
    -- | The range of IP addresses that contribute content or initiate output
    -- requests for flows communicating with this gateway. These IP addresses
    -- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
    -- for example, 10.0.0.0\/16.
    egressCidrBlocks :: [Prelude.Text],
    -- | The name of the gateway. This name can not be modified after the gateway
    -- is created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Gateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayMessages', 'gateway_gatewayMessages' - Undocumented member.
--
-- 'gatewayState', 'gateway_gatewayState' - The current status of the gateway.
--
-- 'gatewayArn', 'gateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway.
--
-- 'networks', 'gateway_networks' - The list of networks in the gateway.
--
-- 'egressCidrBlocks', 'gateway_egressCidrBlocks' - The range of IP addresses that contribute content or initiate output
-- requests for flows communicating with this gateway. These IP addresses
-- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
-- for example, 10.0.0.0\/16.
--
-- 'name', 'gateway_name' - The name of the gateway. This name can not be modified after the gateway
-- is created.
newGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Gateway
newGateway pGatewayArn_ pName_ =
  Gateway'
    { gatewayMessages = Prelude.Nothing,
      gatewayState = Prelude.Nothing,
      gatewayArn = pGatewayArn_,
      networks = Prelude.mempty,
      egressCidrBlocks = Prelude.mempty,
      name = pName_
    }

-- | Undocumented member.
gateway_gatewayMessages :: Lens.Lens' Gateway (Prelude.Maybe [MessageDetail])
gateway_gatewayMessages = Lens.lens (\Gateway' {gatewayMessages} -> gatewayMessages) (\s@Gateway' {} a -> s {gatewayMessages = a} :: Gateway) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the gateway.
gateway_gatewayState :: Lens.Lens' Gateway (Prelude.Maybe GatewayState)
gateway_gatewayState = Lens.lens (\Gateway' {gatewayState} -> gatewayState) (\s@Gateway' {} a -> s {gatewayState = a} :: Gateway)

-- | The Amazon Resource Name (ARN) of the gateway.
gateway_gatewayArn :: Lens.Lens' Gateway Prelude.Text
gateway_gatewayArn = Lens.lens (\Gateway' {gatewayArn} -> gatewayArn) (\s@Gateway' {} a -> s {gatewayArn = a} :: Gateway)

-- | The list of networks in the gateway.
gateway_networks :: Lens.Lens' Gateway [GatewayNetwork]
gateway_networks = Lens.lens (\Gateway' {networks} -> networks) (\s@Gateway' {} a -> s {networks = a} :: Gateway) Prelude.. Lens.coerced

-- | The range of IP addresses that contribute content or initiate output
-- requests for flows communicating with this gateway. These IP addresses
-- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
-- for example, 10.0.0.0\/16.
gateway_egressCidrBlocks :: Lens.Lens' Gateway [Prelude.Text]
gateway_egressCidrBlocks = Lens.lens (\Gateway' {egressCidrBlocks} -> egressCidrBlocks) (\s@Gateway' {} a -> s {egressCidrBlocks = a} :: Gateway) Prelude.. Lens.coerced

-- | The name of the gateway. This name can not be modified after the gateway
-- is created.
gateway_name :: Lens.Lens' Gateway Prelude.Text
gateway_name = Lens.lens (\Gateway' {name} -> name) (\s@Gateway' {} a -> s {name = a} :: Gateway)

instance Data.FromJSON Gateway where
  parseJSON =
    Data.withObject
      "Gateway"
      ( \x ->
          Gateway'
            Prelude.<$> ( x
                            Data..:? "gatewayMessages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "gatewayState")
            Prelude.<*> (x Data..: "gatewayArn")
            Prelude.<*> (x Data..:? "networks" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "egressCidrBlocks"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Gateway where
  hashWithSalt _salt Gateway' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayMessages
      `Prelude.hashWithSalt` gatewayState
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` networks
      `Prelude.hashWithSalt` egressCidrBlocks
      `Prelude.hashWithSalt` name

instance Prelude.NFData Gateway where
  rnf Gateway' {..} =
    Prelude.rnf gatewayMessages
      `Prelude.seq` Prelude.rnf gatewayState
      `Prelude.seq` Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf networks
      `Prelude.seq` Prelude.rnf egressCidrBlocks
      `Prelude.seq` Prelude.rnf name

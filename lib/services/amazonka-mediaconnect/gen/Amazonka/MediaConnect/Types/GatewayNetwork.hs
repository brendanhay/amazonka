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
-- Module      : Amazonka.MediaConnect.Types.GatewayNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.GatewayNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The network settings for a gateway.
--
-- /See:/ 'newGatewayNetwork' smart constructor.
data GatewayNetwork = GatewayNetwork'
  { -- | A unique IP address range to use for this network. These IP addresses
    -- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
    -- for example, 10.0.0.0\/16.
    cidrBlock :: Prelude.Text,
    -- | The name of the network. This name is used to reference the network and
    -- must be unique among networks in this gateway.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'gatewayNetwork_cidrBlock' - A unique IP address range to use for this network. These IP addresses
-- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
-- for example, 10.0.0.0\/16.
--
-- 'name', 'gatewayNetwork_name' - The name of the network. This name is used to reference the network and
-- must be unique among networks in this gateway.
newGatewayNetwork ::
  -- | 'cidrBlock'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GatewayNetwork
newGatewayNetwork pCidrBlock_ pName_ =
  GatewayNetwork'
    { cidrBlock = pCidrBlock_,
      name = pName_
    }

-- | A unique IP address range to use for this network. These IP addresses
-- should be in the form of a Classless Inter-Domain Routing (CIDR) block;
-- for example, 10.0.0.0\/16.
gatewayNetwork_cidrBlock :: Lens.Lens' GatewayNetwork Prelude.Text
gatewayNetwork_cidrBlock = Lens.lens (\GatewayNetwork' {cidrBlock} -> cidrBlock) (\s@GatewayNetwork' {} a -> s {cidrBlock = a} :: GatewayNetwork)

-- | The name of the network. This name is used to reference the network and
-- must be unique among networks in this gateway.
gatewayNetwork_name :: Lens.Lens' GatewayNetwork Prelude.Text
gatewayNetwork_name = Lens.lens (\GatewayNetwork' {name} -> name) (\s@GatewayNetwork' {} a -> s {name = a} :: GatewayNetwork)

instance Data.FromJSON GatewayNetwork where
  parseJSON =
    Data.withObject
      "GatewayNetwork"
      ( \x ->
          GatewayNetwork'
            Prelude.<$> (x Data..: "cidrBlock")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable GatewayNetwork where
  hashWithSalt _salt GatewayNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` name

instance Prelude.NFData GatewayNetwork where
  rnf GatewayNetwork' {..} =
    Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON GatewayNetwork where
  toJSON GatewayNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cidrBlock" Data..= cidrBlock),
            Prelude.Just ("name" Data..= name)
          ]
      )

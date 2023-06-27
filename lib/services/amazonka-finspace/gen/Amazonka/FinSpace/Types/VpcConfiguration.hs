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
-- Module      : Amazonka.FinSpace.Types.VpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.IPAddressType
import qualified Amazonka.Prelude as Prelude

-- | Configuration details about the network where the Privatelink endpoint
-- of the cluster resides.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | The IP address type for cluster network configuration parameters. The
    -- following type is available:
    --
    -- -   IP_V4 – IP address version 4
    ipAddressType :: Prelude.Maybe IPAddressType,
    -- | The unique identifier of the VPC security group applied to the VPC
    -- endpoint ENI for the cluster.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the subnet that the Privatelink VPC endpoint uses to
    -- connect to the cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the VPC endpoint.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'vpcConfiguration_ipAddressType' - The IP address type for cluster network configuration parameters. The
-- following type is available:
--
-- -   IP_V4 – IP address version 4
--
-- 'securityGroupIds', 'vpcConfiguration_securityGroupIds' - The unique identifier of the VPC security group applied to the VPC
-- endpoint ENI for the cluster.
--
-- 'subnetIds', 'vpcConfiguration_subnetIds' - The identifier of the subnet that the Privatelink VPC endpoint uses to
-- connect to the cluster.
--
-- 'vpcId', 'vpcConfiguration_vpcId' - The identifier of the VPC endpoint.
newVpcConfiguration ::
  VpcConfiguration
newVpcConfiguration =
  VpcConfiguration'
    { ipAddressType = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IP address type for cluster network configuration parameters. The
-- following type is available:
--
-- -   IP_V4 – IP address version 4
vpcConfiguration_ipAddressType :: Lens.Lens' VpcConfiguration (Prelude.Maybe IPAddressType)
vpcConfiguration_ipAddressType = Lens.lens (\VpcConfiguration' {ipAddressType} -> ipAddressType) (\s@VpcConfiguration' {} a -> s {ipAddressType = a} :: VpcConfiguration)

-- | The unique identifier of the VPC security group applied to the VPC
-- endpoint ENI for the cluster.
vpcConfiguration_securityGroupIds :: Lens.Lens' VpcConfiguration (Prelude.Maybe [Prelude.Text])
vpcConfiguration_securityGroupIds = Lens.lens (\VpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@VpcConfiguration' {} a -> s {securityGroupIds = a} :: VpcConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the subnet that the Privatelink VPC endpoint uses to
-- connect to the cluster.
vpcConfiguration_subnetIds :: Lens.Lens' VpcConfiguration (Prelude.Maybe [Prelude.Text])
vpcConfiguration_subnetIds = Lens.lens (\VpcConfiguration' {subnetIds} -> subnetIds) (\s@VpcConfiguration' {} a -> s {subnetIds = a} :: VpcConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC endpoint.
vpcConfiguration_vpcId :: Lens.Lens' VpcConfiguration (Prelude.Maybe Prelude.Text)
vpcConfiguration_vpcId = Lens.lens (\VpcConfiguration' {vpcId} -> vpcId) (\s@VpcConfiguration' {} a -> s {vpcId = a} :: VpcConfiguration)

instance Data.FromJSON VpcConfiguration where
  parseJSON =
    Data.withObject
      "VpcConfiguration"
      ( \x ->
          VpcConfiguration'
            Prelude.<$> (x Data..:? "ipAddressType")
            Prelude.<*> ( x
                            Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ipAddressType" Data..=) Prelude.<$> ipAddressType,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            ("vpcId" Data..=) Prelude.<$> vpcId
          ]
      )

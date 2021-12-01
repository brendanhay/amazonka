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
-- Module      : Amazonka.Synthetics.Types.VpcConfigOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.VpcConfigOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | If this canary is to test an endpoint in a VPC, this structure contains
-- information about the subnets and security groups of the VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_VPC.html Running a Canary in a VPC>.
--
-- /See:/ 'newVpcConfigOutput' smart constructor.
data VpcConfigOutput = VpcConfigOutput'
  { -- | The IDs of the security groups for this canary.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the subnets where this canary is to run.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the VPC where this canary is to run.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigOutput_securityGroupIds' - The IDs of the security groups for this canary.
--
-- 'subnetIds', 'vpcConfigOutput_subnetIds' - The IDs of the subnets where this canary is to run.
--
-- 'vpcId', 'vpcConfigOutput_vpcId' - The IDs of the VPC where this canary is to run.
newVpcConfigOutput ::
  VpcConfigOutput
newVpcConfigOutput =
  VpcConfigOutput'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IDs of the security groups for this canary.
vpcConfigOutput_securityGroupIds :: Lens.Lens' VpcConfigOutput (Prelude.Maybe [Prelude.Text])
vpcConfigOutput_securityGroupIds = Lens.lens (\VpcConfigOutput' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigOutput' {} a -> s {securityGroupIds = a} :: VpcConfigOutput) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the subnets where this canary is to run.
vpcConfigOutput_subnetIds :: Lens.Lens' VpcConfigOutput (Prelude.Maybe [Prelude.Text])
vpcConfigOutput_subnetIds = Lens.lens (\VpcConfigOutput' {subnetIds} -> subnetIds) (\s@VpcConfigOutput' {} a -> s {subnetIds = a} :: VpcConfigOutput) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the VPC where this canary is to run.
vpcConfigOutput_vpcId :: Lens.Lens' VpcConfigOutput (Prelude.Maybe Prelude.Text)
vpcConfigOutput_vpcId = Lens.lens (\VpcConfigOutput' {vpcId} -> vpcId) (\s@VpcConfigOutput' {} a -> s {vpcId = a} :: VpcConfigOutput)

instance Core.FromJSON VpcConfigOutput where
  parseJSON =
    Core.withObject
      "VpcConfigOutput"
      ( \x ->
          VpcConfigOutput'
            Prelude.<$> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SubnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VpcId")
      )

instance Prelude.Hashable VpcConfigOutput where
  hashWithSalt salt' VpcConfigOutput' {..} =
    salt' `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData VpcConfigOutput where
  rnf VpcConfigOutput' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds

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
-- Module      : Amazonka.Synthetics.Types.VpcConfigInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.VpcConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | If this canary is to test an endpoint in a VPC, this structure contains
-- information about the subnets and security groups of the VPC endpoint.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_VPC.html Running a Canary in a VPC>.
--
-- /See:/ 'newVpcConfigInput' smart constructor.
data VpcConfigInput = VpcConfigInput'
  { -- | The IDs of the security groups for this canary.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the subnets where this canary is to run.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigInput_securityGroupIds' - The IDs of the security groups for this canary.
--
-- 'subnetIds', 'vpcConfigInput_subnetIds' - The IDs of the subnets where this canary is to run.
newVpcConfigInput ::
  VpcConfigInput
newVpcConfigInput =
  VpcConfigInput'
    { securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The IDs of the security groups for this canary.
vpcConfigInput_securityGroupIds :: Lens.Lens' VpcConfigInput (Prelude.Maybe [Prelude.Text])
vpcConfigInput_securityGroupIds = Lens.lens (\VpcConfigInput' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigInput' {} a -> s {securityGroupIds = a} :: VpcConfigInput) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the subnets where this canary is to run.
vpcConfigInput_subnetIds :: Lens.Lens' VpcConfigInput (Prelude.Maybe [Prelude.Text])
vpcConfigInput_subnetIds = Lens.lens (\VpcConfigInput' {subnetIds} -> subnetIds) (\s@VpcConfigInput' {} a -> s {subnetIds = a} :: VpcConfigInput) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable VpcConfigInput where
  hashWithSalt _salt VpcConfigInput' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcConfigInput where
  rnf VpcConfigInput' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Core.ToJSON VpcConfigInput where
  toJSON VpcConfigInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Core..=) Prelude.<$> subnetIds
          ]
      )

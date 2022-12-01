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
-- Module      : Amazonka.SageMaker.Types.WorkforceVpcConfigRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.WorkforceVpcConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC object you use to create or update a workforce.
--
-- /See:/ 'newWorkforceVpcConfigRequest' smart constructor.
data WorkforceVpcConfigRequest = WorkforceVpcConfigRequest'
  { -- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
    -- must be for the same VPC as specified in the subnet.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the subnets in the VPC that you want to connect.
    subnets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the VPC that the workforce uses for communication.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkforceVpcConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'workforceVpcConfigRequest_securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
--
-- 'subnets', 'workforceVpcConfigRequest_subnets' - The ID of the subnets in the VPC that you want to connect.
--
-- 'vpcId', 'workforceVpcConfigRequest_vpcId' - The ID of the VPC that the workforce uses for communication.
newWorkforceVpcConfigRequest ::
  WorkforceVpcConfigRequest
newWorkforceVpcConfigRequest =
  WorkforceVpcConfigRequest'
    { securityGroupIds =
        Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
workforceVpcConfigRequest_securityGroupIds :: Lens.Lens' WorkforceVpcConfigRequest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
workforceVpcConfigRequest_securityGroupIds = Lens.lens (\WorkforceVpcConfigRequest' {securityGroupIds} -> securityGroupIds) (\s@WorkforceVpcConfigRequest' {} a -> s {securityGroupIds = a} :: WorkforceVpcConfigRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnets in the VPC that you want to connect.
workforceVpcConfigRequest_subnets :: Lens.Lens' WorkforceVpcConfigRequest (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
workforceVpcConfigRequest_subnets = Lens.lens (\WorkforceVpcConfigRequest' {subnets} -> subnets) (\s@WorkforceVpcConfigRequest' {} a -> s {subnets = a} :: WorkforceVpcConfigRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC that the workforce uses for communication.
workforceVpcConfigRequest_vpcId :: Lens.Lens' WorkforceVpcConfigRequest (Prelude.Maybe Prelude.Text)
workforceVpcConfigRequest_vpcId = Lens.lens (\WorkforceVpcConfigRequest' {vpcId} -> vpcId) (\s@WorkforceVpcConfigRequest' {} a -> s {vpcId = a} :: WorkforceVpcConfigRequest)

instance Prelude.Hashable WorkforceVpcConfigRequest where
  hashWithSalt _salt WorkforceVpcConfigRequest' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData WorkforceVpcConfigRequest where
  rnf WorkforceVpcConfigRequest' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId

instance Core.ToJSON WorkforceVpcConfigRequest where
  toJSON WorkforceVpcConfigRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("Subnets" Core..=) Prelude.<$> subnets,
            ("VpcId" Core..=) Prelude.<$> vpcId
          ]
      )

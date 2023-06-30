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
-- Module      : Amazonka.SageMaker.Types.WorkforceVpcConfigResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.WorkforceVpcConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A VpcConfig object that specifies the VPC that you want your workforce
-- to connect to.
--
-- /See:/ 'newWorkforceVpcConfigResponse' smart constructor.
data WorkforceVpcConfigResponse = WorkforceVpcConfigResponse'
  { -- | The IDs for the VPC service endpoints of your VPC workforce when it is
    -- created and updated.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that the workforce uses for communication.
    vpcId :: Prelude.Text,
    -- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
    -- must be for the same VPC as specified in the subnet.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the subnets in the VPC that you want to connect.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkforceVpcConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointId', 'workforceVpcConfigResponse_vpcEndpointId' - The IDs for the VPC service endpoints of your VPC workforce when it is
-- created and updated.
--
-- 'vpcId', 'workforceVpcConfigResponse_vpcId' - The ID of the VPC that the workforce uses for communication.
--
-- 'securityGroupIds', 'workforceVpcConfigResponse_securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
--
-- 'subnets', 'workforceVpcConfigResponse_subnets' - The ID of the subnets in the VPC that you want to connect.
newWorkforceVpcConfigResponse ::
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  WorkforceVpcConfigResponse
newWorkforceVpcConfigResponse
  pVpcId_
  pSecurityGroupIds_
  pSubnets_ =
    WorkforceVpcConfigResponse'
      { vpcEndpointId =
          Prelude.Nothing,
        vpcId = pVpcId_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        subnets = Lens.coerced Lens.# pSubnets_
      }

-- | The IDs for the VPC service endpoints of your VPC workforce when it is
-- created and updated.
workforceVpcConfigResponse_vpcEndpointId :: Lens.Lens' WorkforceVpcConfigResponse (Prelude.Maybe Prelude.Text)
workforceVpcConfigResponse_vpcEndpointId = Lens.lens (\WorkforceVpcConfigResponse' {vpcEndpointId} -> vpcEndpointId) (\s@WorkforceVpcConfigResponse' {} a -> s {vpcEndpointId = a} :: WorkforceVpcConfigResponse)

-- | The ID of the VPC that the workforce uses for communication.
workforceVpcConfigResponse_vpcId :: Lens.Lens' WorkforceVpcConfigResponse Prelude.Text
workforceVpcConfigResponse_vpcId = Lens.lens (\WorkforceVpcConfigResponse' {vpcId} -> vpcId) (\s@WorkforceVpcConfigResponse' {} a -> s {vpcId = a} :: WorkforceVpcConfigResponse)

-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
workforceVpcConfigResponse_securityGroupIds :: Lens.Lens' WorkforceVpcConfigResponse (Prelude.NonEmpty Prelude.Text)
workforceVpcConfigResponse_securityGroupIds = Lens.lens (\WorkforceVpcConfigResponse' {securityGroupIds} -> securityGroupIds) (\s@WorkforceVpcConfigResponse' {} a -> s {securityGroupIds = a} :: WorkforceVpcConfigResponse) Prelude.. Lens.coerced

-- | The ID of the subnets in the VPC that you want to connect.
workforceVpcConfigResponse_subnets :: Lens.Lens' WorkforceVpcConfigResponse (Prelude.NonEmpty Prelude.Text)
workforceVpcConfigResponse_subnets = Lens.lens (\WorkforceVpcConfigResponse' {subnets} -> subnets) (\s@WorkforceVpcConfigResponse' {} a -> s {subnets = a} :: WorkforceVpcConfigResponse) Prelude.. Lens.coerced

instance Data.FromJSON WorkforceVpcConfigResponse where
  parseJSON =
    Data.withObject
      "WorkforceVpcConfigResponse"
      ( \x ->
          WorkforceVpcConfigResponse'
            Prelude.<$> (x Data..:? "VpcEndpointId")
            Prelude.<*> (x Data..: "VpcId")
            Prelude.<*> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable WorkforceVpcConfigResponse where
  hashWithSalt _salt WorkforceVpcConfigResponse' {..} =
    _salt
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData WorkforceVpcConfigResponse where
  rnf WorkforceVpcConfigResponse' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets

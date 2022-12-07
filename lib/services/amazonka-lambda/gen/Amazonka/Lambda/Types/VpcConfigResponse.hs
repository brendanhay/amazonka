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
-- Module      : Amazonka.Lambda.Types.VpcConfigResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.VpcConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VPC security groups and subnets that are attached to a Lambda
-- function.
--
-- /See:/ 'newVpcConfigResponse' smart constructor.
data VpcConfigResponse = VpcConfigResponse'
  { -- | A list of VPC security groups IDs.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC subnet IDs.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfigResponse_securityGroupIds' - A list of VPC security groups IDs.
--
-- 'vpcId', 'vpcConfigResponse_vpcId' - The ID of the VPC.
--
-- 'subnetIds', 'vpcConfigResponse_subnetIds' - A list of VPC subnet IDs.
newVpcConfigResponse ::
  VpcConfigResponse
newVpcConfigResponse =
  VpcConfigResponse'
    { securityGroupIds =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | A list of VPC security groups IDs.
vpcConfigResponse_securityGroupIds :: Lens.Lens' VpcConfigResponse (Prelude.Maybe [Prelude.Text])
vpcConfigResponse_securityGroupIds = Lens.lens (\VpcConfigResponse' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigResponse' {} a -> s {securityGroupIds = a} :: VpcConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
vpcConfigResponse_vpcId :: Lens.Lens' VpcConfigResponse (Prelude.Maybe Prelude.Text)
vpcConfigResponse_vpcId = Lens.lens (\VpcConfigResponse' {vpcId} -> vpcId) (\s@VpcConfigResponse' {} a -> s {vpcId = a} :: VpcConfigResponse)

-- | A list of VPC subnet IDs.
vpcConfigResponse_subnetIds :: Lens.Lens' VpcConfigResponse (Prelude.Maybe [Prelude.Text])
vpcConfigResponse_subnetIds = Lens.lens (\VpcConfigResponse' {subnetIds} -> subnetIds) (\s@VpcConfigResponse' {} a -> s {subnetIds = a} :: VpcConfigResponse) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VpcConfigResponse where
  parseJSON =
    Data.withObject
      "VpcConfigResponse"
      ( \x ->
          VpcConfigResponse'
            Prelude.<$> ( x Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable VpcConfigResponse where
  hashWithSalt _salt VpcConfigResponse' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcConfigResponse where
  rnf VpcConfigResponse' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds

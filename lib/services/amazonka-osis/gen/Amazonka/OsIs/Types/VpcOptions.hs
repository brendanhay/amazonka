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
-- Module      : Amazonka.OsIs.Types.VpcOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.VpcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options that specify the subnets and security groups for an OpenSearch
-- Ingestion VPC endpoint.
--
-- /See:/ 'newVpcOptions' smart constructor.
data VpcOptions = VpcOptions'
  { -- | A list of security groups associated with the VPC endpoint.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of subnet IDs associated with the VPC endpoint.
    subnetIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcOptions_securityGroupIds' - A list of security groups associated with the VPC endpoint.
--
-- 'subnetIds', 'vpcOptions_subnetIds' - A list of subnet IDs associated with the VPC endpoint.
newVpcOptions ::
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcOptions
newVpcOptions pSubnetIds_ =
  VpcOptions'
    { securityGroupIds = Prelude.Nothing,
      subnetIds = Lens.coerced Lens.# pSubnetIds_
    }

-- | A list of security groups associated with the VPC endpoint.
vpcOptions_securityGroupIds :: Lens.Lens' VpcOptions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vpcOptions_securityGroupIds = Lens.lens (\VpcOptions' {securityGroupIds} -> securityGroupIds) (\s@VpcOptions' {} a -> s {securityGroupIds = a} :: VpcOptions) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs associated with the VPC endpoint.
vpcOptions_subnetIds :: Lens.Lens' VpcOptions (Prelude.NonEmpty Prelude.Text)
vpcOptions_subnetIds = Lens.lens (\VpcOptions' {subnetIds} -> subnetIds) (\s@VpcOptions' {} a -> s {subnetIds = a} :: VpcOptions) Prelude.. Lens.coerced

instance Data.FromJSON VpcOptions where
  parseJSON =
    Data.withObject
      "VpcOptions"
      ( \x ->
          VpcOptions'
            Prelude.<$> (x Data..:? "SecurityGroupIds")
            Prelude.<*> (x Data..: "SubnetIds")
      )

instance Prelude.Hashable VpcOptions where
  hashWithSalt _salt VpcOptions' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcOptions where
  rnf VpcOptions' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON VpcOptions where
  toJSON VpcOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.VpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the parameters of a VPC used by the application.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | The array of
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
    -- IDs used by the VPC configuration.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The array of
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
    -- IDs used by the VPC configuration.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text
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
-- 'subnetIds', 'vpcConfiguration_subnetIds' - The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
--
-- 'securityGroupIds', 'vpcConfiguration_securityGroupIds' - The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
newVpcConfiguration ::
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfiguration
newVpcConfiguration pSubnetIds_ pSecurityGroupIds_ =
  VpcConfiguration'
    { subnetIds =
        Lens.coerced Lens.# pSubnetIds_,
      securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_
    }

-- | The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
vpcConfiguration_subnetIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_subnetIds = Lens.lens (\VpcConfiguration' {subnetIds} -> subnetIds) (\s@VpcConfiguration' {} a -> s {subnetIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

-- | The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
vpcConfiguration_securityGroupIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_securityGroupIds = Lens.lens (\VpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@VpcConfiguration' {} a -> s {securityGroupIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds)
          ]
      )

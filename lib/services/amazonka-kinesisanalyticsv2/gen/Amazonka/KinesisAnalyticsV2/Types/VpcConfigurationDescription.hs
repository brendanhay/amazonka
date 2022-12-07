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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the parameters of a VPC used by the application.
--
-- /See:/ 'newVpcConfigurationDescription' smart constructor.
data VpcConfigurationDescription = VpcConfigurationDescription'
  { -- | The ID of the VPC configuration.
    vpcConfigurationId :: Prelude.Text,
    -- | The ID of the associated VPC.
    vpcId :: Prelude.Text,
    -- | The array of
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
-- Create a value of 'VpcConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfigurationId', 'vpcConfigurationDescription_vpcConfigurationId' - The ID of the VPC configuration.
--
-- 'vpcId', 'vpcConfigurationDescription_vpcId' - The ID of the associated VPC.
--
-- 'subnetIds', 'vpcConfigurationDescription_subnetIds' - The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
--
-- 'securityGroupIds', 'vpcConfigurationDescription_securityGroupIds' - The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
newVpcConfigurationDescription ::
  -- | 'vpcConfigurationId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfigurationDescription
newVpcConfigurationDescription
  pVpcConfigurationId_
  pVpcId_
  pSubnetIds_
  pSecurityGroupIds_ =
    VpcConfigurationDescription'
      { vpcConfigurationId =
          pVpcConfigurationId_,
        vpcId = pVpcId_,
        subnetIds = Lens.coerced Lens.# pSubnetIds_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_
      }

-- | The ID of the VPC configuration.
vpcConfigurationDescription_vpcConfigurationId :: Lens.Lens' VpcConfigurationDescription Prelude.Text
vpcConfigurationDescription_vpcConfigurationId = Lens.lens (\VpcConfigurationDescription' {vpcConfigurationId} -> vpcConfigurationId) (\s@VpcConfigurationDescription' {} a -> s {vpcConfigurationId = a} :: VpcConfigurationDescription)

-- | The ID of the associated VPC.
vpcConfigurationDescription_vpcId :: Lens.Lens' VpcConfigurationDescription Prelude.Text
vpcConfigurationDescription_vpcId = Lens.lens (\VpcConfigurationDescription' {vpcId} -> vpcId) (\s@VpcConfigurationDescription' {} a -> s {vpcId = a} :: VpcConfigurationDescription)

-- | The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
vpcConfigurationDescription_subnetIds :: Lens.Lens' VpcConfigurationDescription (Prelude.NonEmpty Prelude.Text)
vpcConfigurationDescription_subnetIds = Lens.lens (\VpcConfigurationDescription' {subnetIds} -> subnetIds) (\s@VpcConfigurationDescription' {} a -> s {subnetIds = a} :: VpcConfigurationDescription) Prelude.. Lens.coerced

-- | The array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
vpcConfigurationDescription_securityGroupIds :: Lens.Lens' VpcConfigurationDescription (Prelude.NonEmpty Prelude.Text)
vpcConfigurationDescription_securityGroupIds = Lens.lens (\VpcConfigurationDescription' {securityGroupIds} -> securityGroupIds) (\s@VpcConfigurationDescription' {} a -> s {securityGroupIds = a} :: VpcConfigurationDescription) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfigurationDescription where
  parseJSON =
    Data.withObject
      "VpcConfigurationDescription"
      ( \x ->
          VpcConfigurationDescription'
            Prelude.<$> (x Data..: "VpcConfigurationId")
            Prelude.<*> (x Data..: "VpcId")
            Prelude.<*> (x Data..: "SubnetIds")
            Prelude.<*> (x Data..: "SecurityGroupIds")
      )

instance Prelude.Hashable VpcConfigurationDescription where
  hashWithSalt _salt VpcConfigurationDescription' {..} =
    _salt `Prelude.hashWithSalt` vpcConfigurationId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData VpcConfigurationDescription where
  rnf VpcConfigurationDescription' {..} =
    Prelude.rnf vpcConfigurationId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds

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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to the VPC configuration used by the application.
--
-- /See:/ 'newVpcConfigurationUpdate' smart constructor.
data VpcConfigurationUpdate = VpcConfigurationUpdate'
  { -- | Describes updates to the array of
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
    -- IDs used by the VPC configuration.
    securityGroupIdUpdates :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Describes updates to the array of
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
    -- IDs used by the VPC configuration.
    subnetIdUpdates :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Describes an update to the ID of the VPC configuration.
    vpcConfigurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIdUpdates', 'vpcConfigurationUpdate_securityGroupIdUpdates' - Describes updates to the array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
--
-- 'subnetIdUpdates', 'vpcConfigurationUpdate_subnetIdUpdates' - Describes updates to the array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
--
-- 'vpcConfigurationId', 'vpcConfigurationUpdate_vpcConfigurationId' - Describes an update to the ID of the VPC configuration.
newVpcConfigurationUpdate ::
  -- | 'vpcConfigurationId'
  Prelude.Text ->
  VpcConfigurationUpdate
newVpcConfigurationUpdate pVpcConfigurationId_ =
  VpcConfigurationUpdate'
    { securityGroupIdUpdates =
        Prelude.Nothing,
      subnetIdUpdates = Prelude.Nothing,
      vpcConfigurationId = pVpcConfigurationId_
    }

-- | Describes updates to the array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SecurityGroup.html SecurityGroup>
-- IDs used by the VPC configuration.
vpcConfigurationUpdate_securityGroupIdUpdates :: Lens.Lens' VpcConfigurationUpdate (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vpcConfigurationUpdate_securityGroupIdUpdates = Lens.lens (\VpcConfigurationUpdate' {securityGroupIdUpdates} -> securityGroupIdUpdates) (\s@VpcConfigurationUpdate' {} a -> s {securityGroupIdUpdates = a} :: VpcConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Describes updates to the array of
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_Subnet.html Subnet>
-- IDs used by the VPC configuration.
vpcConfigurationUpdate_subnetIdUpdates :: Lens.Lens' VpcConfigurationUpdate (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vpcConfigurationUpdate_subnetIdUpdates = Lens.lens (\VpcConfigurationUpdate' {subnetIdUpdates} -> subnetIdUpdates) (\s@VpcConfigurationUpdate' {} a -> s {subnetIdUpdates = a} :: VpcConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Describes an update to the ID of the VPC configuration.
vpcConfigurationUpdate_vpcConfigurationId :: Lens.Lens' VpcConfigurationUpdate Prelude.Text
vpcConfigurationUpdate_vpcConfigurationId = Lens.lens (\VpcConfigurationUpdate' {vpcConfigurationId} -> vpcConfigurationId) (\s@VpcConfigurationUpdate' {} a -> s {vpcConfigurationId = a} :: VpcConfigurationUpdate)

instance Prelude.Hashable VpcConfigurationUpdate where
  hashWithSalt _salt VpcConfigurationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIdUpdates
      `Prelude.hashWithSalt` subnetIdUpdates
      `Prelude.hashWithSalt` vpcConfigurationId

instance Prelude.NFData VpcConfigurationUpdate where
  rnf VpcConfigurationUpdate' {..} =
    Prelude.rnf securityGroupIdUpdates
      `Prelude.seq` Prelude.rnf subnetIdUpdates
      `Prelude.seq` Prelude.rnf vpcConfigurationId

instance Data.ToJSON VpcConfigurationUpdate where
  toJSON VpcConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIdUpdates" Data..=)
              Prelude.<$> securityGroupIdUpdates,
            ("SubnetIdUpdates" Data..=)
              Prelude.<$> subnetIdUpdates,
            Prelude.Just
              ("VpcConfigurationId" Data..= vpcConfigurationId)
          ]
      )

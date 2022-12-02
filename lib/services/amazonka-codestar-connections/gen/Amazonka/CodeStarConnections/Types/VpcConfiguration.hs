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
-- Module      : Amazonka.CodeStarConnections.Types.VpcConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The VPC configuration provisioned for the host.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | The value of the Transport Layer Security (TLS) certificate associated
    -- with the infrastructure where your provider type is installed.
    tlsCertificate :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon VPC connected to the infrastructure where your
    -- provider type is installed.
    vpcId :: Prelude.Text,
    -- | The ID of the subnet or subnets associated with the Amazon VPC connected
    -- to the infrastructure where your provider type is installed.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the security group or security groups associated with the
    -- Amazon VPC connected to the infrastructure where your provider type is
    -- installed.
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
-- 'tlsCertificate', 'vpcConfiguration_tlsCertificate' - The value of the Transport Layer Security (TLS) certificate associated
-- with the infrastructure where your provider type is installed.
--
-- 'vpcId', 'vpcConfiguration_vpcId' - The ID of the Amazon VPC connected to the infrastructure where your
-- provider type is installed.
--
-- 'subnetIds', 'vpcConfiguration_subnetIds' - The ID of the subnet or subnets associated with the Amazon VPC connected
-- to the infrastructure where your provider type is installed.
--
-- 'securityGroupIds', 'vpcConfiguration_securityGroupIds' - The ID of the security group or security groups associated with the
-- Amazon VPC connected to the infrastructure where your provider type is
-- installed.
newVpcConfiguration ::
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfiguration
newVpcConfiguration
  pVpcId_
  pSubnetIds_
  pSecurityGroupIds_ =
    VpcConfiguration'
      { tlsCertificate = Prelude.Nothing,
        vpcId = pVpcId_,
        subnetIds = Lens.coerced Lens.# pSubnetIds_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_
      }

-- | The value of the Transport Layer Security (TLS) certificate associated
-- with the infrastructure where your provider type is installed.
vpcConfiguration_tlsCertificate :: Lens.Lens' VpcConfiguration (Prelude.Maybe Prelude.Text)
vpcConfiguration_tlsCertificate = Lens.lens (\VpcConfiguration' {tlsCertificate} -> tlsCertificate) (\s@VpcConfiguration' {} a -> s {tlsCertificate = a} :: VpcConfiguration)

-- | The ID of the Amazon VPC connected to the infrastructure where your
-- provider type is installed.
vpcConfiguration_vpcId :: Lens.Lens' VpcConfiguration Prelude.Text
vpcConfiguration_vpcId = Lens.lens (\VpcConfiguration' {vpcId} -> vpcId) (\s@VpcConfiguration' {} a -> s {vpcId = a} :: VpcConfiguration)

-- | The ID of the subnet or subnets associated with the Amazon VPC connected
-- to the infrastructure where your provider type is installed.
vpcConfiguration_subnetIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_subnetIds = Lens.lens (\VpcConfiguration' {subnetIds} -> subnetIds) (\s@VpcConfiguration' {} a -> s {subnetIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

-- | The ID of the security group or security groups associated with the
-- Amazon VPC connected to the infrastructure where your provider type is
-- installed.
vpcConfiguration_securityGroupIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_securityGroupIds = Lens.lens (\VpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@VpcConfiguration' {} a -> s {securityGroupIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfiguration where
  parseJSON =
    Data.withObject
      "VpcConfiguration"
      ( \x ->
          VpcConfiguration'
            Prelude.<$> (x Data..:? "TlsCertificate")
            Prelude.<*> (x Data..: "VpcId")
            Prelude.<*> (x Data..: "SubnetIds")
            Prelude.<*> (x Data..: "SecurityGroupIds")
      )

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` tlsCertificate
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf tlsCertificate
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TlsCertificate" Data..=)
              Prelude.<$> tlsCertificate,
            Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds)
          ]
      )

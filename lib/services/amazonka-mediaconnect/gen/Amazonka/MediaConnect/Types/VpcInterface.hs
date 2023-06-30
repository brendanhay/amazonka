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
-- Module      : Amazonka.MediaConnect.Types.VpcInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.VpcInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.NetworkInterfaceType
import qualified Amazonka.Prelude as Prelude

-- | The settings for a VPC Source.
--
-- /See:/ 'newVpcInterface' smart constructor.
data VpcInterface = VpcInterface'
  { -- | The type of network interface.
    networkInterfaceType :: NetworkInterfaceType,
    -- | IDs of the network interfaces created in customer\'s account by
    -- MediaConnect.
    networkInterfaceIds :: [Prelude.Text],
    -- | Subnet must be in the AZ of the Flow
    subnetId :: Prelude.Text,
    -- | Security Group IDs to be used on ENI.
    securityGroupIds :: [Prelude.Text],
    -- | Role Arn MediaConnect can assumes to create ENIs in customer\'s account
    roleArn :: Prelude.Text,
    -- | Immutable and has to be a unique against other VpcInterfaces in this
    -- Flow.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceType', 'vpcInterface_networkInterfaceType' - The type of network interface.
--
-- 'networkInterfaceIds', 'vpcInterface_networkInterfaceIds' - IDs of the network interfaces created in customer\'s account by
-- MediaConnect.
--
-- 'subnetId', 'vpcInterface_subnetId' - Subnet must be in the AZ of the Flow
--
-- 'securityGroupIds', 'vpcInterface_securityGroupIds' - Security Group IDs to be used on ENI.
--
-- 'roleArn', 'vpcInterface_roleArn' - Role Arn MediaConnect can assumes to create ENIs in customer\'s account
--
-- 'name', 'vpcInterface_name' - Immutable and has to be a unique against other VpcInterfaces in this
-- Flow.
newVpcInterface ::
  -- | 'networkInterfaceType'
  NetworkInterfaceType ->
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  VpcInterface
newVpcInterface
  pNetworkInterfaceType_
  pSubnetId_
  pRoleArn_
  pName_ =
    VpcInterface'
      { networkInterfaceType =
          pNetworkInterfaceType_,
        networkInterfaceIds = Prelude.mempty,
        subnetId = pSubnetId_,
        securityGroupIds = Prelude.mempty,
        roleArn = pRoleArn_,
        name = pName_
      }

-- | The type of network interface.
vpcInterface_networkInterfaceType :: Lens.Lens' VpcInterface NetworkInterfaceType
vpcInterface_networkInterfaceType = Lens.lens (\VpcInterface' {networkInterfaceType} -> networkInterfaceType) (\s@VpcInterface' {} a -> s {networkInterfaceType = a} :: VpcInterface)

-- | IDs of the network interfaces created in customer\'s account by
-- MediaConnect.
vpcInterface_networkInterfaceIds :: Lens.Lens' VpcInterface [Prelude.Text]
vpcInterface_networkInterfaceIds = Lens.lens (\VpcInterface' {networkInterfaceIds} -> networkInterfaceIds) (\s@VpcInterface' {} a -> s {networkInterfaceIds = a} :: VpcInterface) Prelude.. Lens.coerced

-- | Subnet must be in the AZ of the Flow
vpcInterface_subnetId :: Lens.Lens' VpcInterface Prelude.Text
vpcInterface_subnetId = Lens.lens (\VpcInterface' {subnetId} -> subnetId) (\s@VpcInterface' {} a -> s {subnetId = a} :: VpcInterface)

-- | Security Group IDs to be used on ENI.
vpcInterface_securityGroupIds :: Lens.Lens' VpcInterface [Prelude.Text]
vpcInterface_securityGroupIds = Lens.lens (\VpcInterface' {securityGroupIds} -> securityGroupIds) (\s@VpcInterface' {} a -> s {securityGroupIds = a} :: VpcInterface) Prelude.. Lens.coerced

-- | Role Arn MediaConnect can assumes to create ENIs in customer\'s account
vpcInterface_roleArn :: Lens.Lens' VpcInterface Prelude.Text
vpcInterface_roleArn = Lens.lens (\VpcInterface' {roleArn} -> roleArn) (\s@VpcInterface' {} a -> s {roleArn = a} :: VpcInterface)

-- | Immutable and has to be a unique against other VpcInterfaces in this
-- Flow.
vpcInterface_name :: Lens.Lens' VpcInterface Prelude.Text
vpcInterface_name = Lens.lens (\VpcInterface' {name} -> name) (\s@VpcInterface' {} a -> s {name = a} :: VpcInterface)

instance Data.FromJSON VpcInterface where
  parseJSON =
    Data.withObject
      "VpcInterface"
      ( \x ->
          VpcInterface'
            Prelude.<$> (x Data..: "networkInterfaceType")
            Prelude.<*> ( x
                            Data..:? "networkInterfaceIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "subnetId")
            Prelude.<*> ( x
                            Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable VpcInterface where
  hashWithSalt _salt VpcInterface' {..} =
    _salt
      `Prelude.hashWithSalt` networkInterfaceType
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData VpcInterface where
  rnf VpcInterface' {..} =
    Prelude.rnf networkInterfaceType
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf name

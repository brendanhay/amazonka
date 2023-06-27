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
-- Module      : Amazonka.IoT.Types.VpcDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.VpcDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration information for a virtual private cloud (VPC)
-- destination.
--
-- /See:/ 'newVpcDestinationConfiguration' smart constructor.
data VpcDestinationConfiguration = VpcDestinationConfiguration'
  { -- | The security groups of the VPC destination.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The subnet IDs of the VPC destination.
    subnetIds :: [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Text,
    -- | The ARN of a role that has permission to create and attach to elastic
    -- network interfaces (ENIs).
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'vpcDestinationConfiguration_securityGroups' - The security groups of the VPC destination.
--
-- 'subnetIds', 'vpcDestinationConfiguration_subnetIds' - The subnet IDs of the VPC destination.
--
-- 'vpcId', 'vpcDestinationConfiguration_vpcId' - The ID of the VPC.
--
-- 'roleArn', 'vpcDestinationConfiguration_roleArn' - The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
newVpcDestinationConfiguration ::
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  VpcDestinationConfiguration
newVpcDestinationConfiguration pVpcId_ pRoleArn_ =
  VpcDestinationConfiguration'
    { securityGroups =
        Prelude.Nothing,
      subnetIds = Prelude.mempty,
      vpcId = pVpcId_,
      roleArn = pRoleArn_
    }

-- | The security groups of the VPC destination.
vpcDestinationConfiguration_securityGroups :: Lens.Lens' VpcDestinationConfiguration (Prelude.Maybe [Prelude.Text])
vpcDestinationConfiguration_securityGroups = Lens.lens (\VpcDestinationConfiguration' {securityGroups} -> securityGroups) (\s@VpcDestinationConfiguration' {} a -> s {securityGroups = a} :: VpcDestinationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The subnet IDs of the VPC destination.
vpcDestinationConfiguration_subnetIds :: Lens.Lens' VpcDestinationConfiguration [Prelude.Text]
vpcDestinationConfiguration_subnetIds = Lens.lens (\VpcDestinationConfiguration' {subnetIds} -> subnetIds) (\s@VpcDestinationConfiguration' {} a -> s {subnetIds = a} :: VpcDestinationConfiguration) Prelude.. Lens.coerced

-- | The ID of the VPC.
vpcDestinationConfiguration_vpcId :: Lens.Lens' VpcDestinationConfiguration Prelude.Text
vpcDestinationConfiguration_vpcId = Lens.lens (\VpcDestinationConfiguration' {vpcId} -> vpcId) (\s@VpcDestinationConfiguration' {} a -> s {vpcId = a} :: VpcDestinationConfiguration)

-- | The ARN of a role that has permission to create and attach to elastic
-- network interfaces (ENIs).
vpcDestinationConfiguration_roleArn :: Lens.Lens' VpcDestinationConfiguration Prelude.Text
vpcDestinationConfiguration_roleArn = Lens.lens (\VpcDestinationConfiguration' {roleArn} -> roleArn) (\s@VpcDestinationConfiguration' {} a -> s {roleArn = a} :: VpcDestinationConfiguration)

instance Prelude.Hashable VpcDestinationConfiguration where
  hashWithSalt _salt VpcDestinationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData VpcDestinationConfiguration where
  rnf VpcDestinationConfiguration' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON VpcDestinationConfiguration where
  toJSON VpcDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("securityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnetIds" Data..= subnetIds),
            Prelude.Just ("vpcId" Data..= vpcId),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

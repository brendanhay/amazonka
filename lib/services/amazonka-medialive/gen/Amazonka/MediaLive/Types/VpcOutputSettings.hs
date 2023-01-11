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
-- Module      : Amazonka.MediaLive.Types.VpcOutputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VpcOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties for a private VPC Output When this property is specified,
-- the output egress addresses will be created in a user specified VPC
--
-- /See:/ 'newVpcOutputSettings' smart constructor.
data VpcOutputSettings = VpcOutputSettings'
  { -- | List of public address allocation ids to associate with ENIs that will
    -- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
    -- STANDARD channels
    publicAddressAllocationIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
    -- network interfaces. If none are specified then the VPC default security
    -- group will be used
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
    -- IDs must be mapped to two unique availability zones (AZ).
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicAddressAllocationIds', 'vpcOutputSettings_publicAddressAllocationIds' - List of public address allocation ids to associate with ENIs that will
-- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
-- STANDARD channels
--
-- 'securityGroupIds', 'vpcOutputSettings_securityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
-- network interfaces. If none are specified then the VPC default security
-- group will be used
--
-- 'subnetIds', 'vpcOutputSettings_subnetIds' - A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
newVpcOutputSettings ::
  VpcOutputSettings
newVpcOutputSettings =
  VpcOutputSettings'
    { publicAddressAllocationIds =
        Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.mempty
    }

-- | List of public address allocation ids to associate with ENIs that will
-- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
-- STANDARD channels
vpcOutputSettings_publicAddressAllocationIds :: Lens.Lens' VpcOutputSettings (Prelude.Maybe [Prelude.Text])
vpcOutputSettings_publicAddressAllocationIds = Lens.lens (\VpcOutputSettings' {publicAddressAllocationIds} -> publicAddressAllocationIds) (\s@VpcOutputSettings' {} a -> s {publicAddressAllocationIds = a} :: VpcOutputSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
-- network interfaces. If none are specified then the VPC default security
-- group will be used
vpcOutputSettings_securityGroupIds :: Lens.Lens' VpcOutputSettings (Prelude.Maybe [Prelude.Text])
vpcOutputSettings_securityGroupIds = Lens.lens (\VpcOutputSettings' {securityGroupIds} -> securityGroupIds) (\s@VpcOutputSettings' {} a -> s {securityGroupIds = a} :: VpcOutputSettings) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
vpcOutputSettings_subnetIds :: Lens.Lens' VpcOutputSettings [Prelude.Text]
vpcOutputSettings_subnetIds = Lens.lens (\VpcOutputSettings' {subnetIds} -> subnetIds) (\s@VpcOutputSettings' {} a -> s {subnetIds = a} :: VpcOutputSettings) Prelude.. Lens.coerced

instance Prelude.Hashable VpcOutputSettings where
  hashWithSalt _salt VpcOutputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` publicAddressAllocationIds
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcOutputSettings where
  rnf VpcOutputSettings' {..} =
    Prelude.rnf publicAddressAllocationIds
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON VpcOutputSettings where
  toJSON VpcOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("publicAddressAllocationIds" Data..=)
              Prelude.<$> publicAddressAllocationIds,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            Prelude.Just ("subnetIds" Data..= subnetIds)
          ]
      )

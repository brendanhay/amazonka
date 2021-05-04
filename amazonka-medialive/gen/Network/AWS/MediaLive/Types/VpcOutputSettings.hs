{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.VpcOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VpcOutputSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties for a private VPC Output When this property is specified,
-- the output egress addresses will be created in a user specified VPC
--
-- /See:/ 'newVpcOutputSettings' smart constructor.
data VpcOutputSettings = VpcOutputSettings'
  { -- | A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
    -- network interfaces. If none are specified then the VPC default security
    -- group will be used
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | List of public address allocation ids to associate with ENIs that will
    -- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
    -- STANDARD channels
    publicAddressAllocationIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
    -- IDs must be mapped to two unique availability zones (AZ).
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcOutputSettings_securityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
-- network interfaces. If none are specified then the VPC default security
-- group will be used
--
-- 'publicAddressAllocationIds', 'vpcOutputSettings_publicAddressAllocationIds' - List of public address allocation ids to associate with ENIs that will
-- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
-- STANDARD channels
--
-- 'subnetIds', 'vpcOutputSettings_subnetIds' - A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
newVpcOutputSettings ::
  VpcOutputSettings
newVpcOutputSettings =
  VpcOutputSettings'
    { securityGroupIds =
        Prelude.Nothing,
      publicAddressAllocationIds = Prelude.Nothing,
      subnetIds = Prelude.mempty
    }

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Output VPC
-- network interfaces. If none are specified then the VPC default security
-- group will be used
vpcOutputSettings_securityGroupIds :: Lens.Lens' VpcOutputSettings (Prelude.Maybe [Prelude.Text])
vpcOutputSettings_securityGroupIds = Lens.lens (\VpcOutputSettings' {securityGroupIds} -> securityGroupIds) (\s@VpcOutputSettings' {} a -> s {securityGroupIds = a} :: VpcOutputSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | List of public address allocation ids to associate with ENIs that will
-- be created in Output VPC. Must specify one for SINGLE_PIPELINE, two for
-- STANDARD channels
vpcOutputSettings_publicAddressAllocationIds :: Lens.Lens' VpcOutputSettings (Prelude.Maybe [Prelude.Text])
vpcOutputSettings_publicAddressAllocationIds = Lens.lens (\VpcOutputSettings' {publicAddressAllocationIds} -> publicAddressAllocationIds) (\s@VpcOutputSettings' {} a -> s {publicAddressAllocationIds = a} :: VpcOutputSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
vpcOutputSettings_subnetIds :: Lens.Lens' VpcOutputSettings [Prelude.Text]
vpcOutputSettings_subnetIds = Lens.lens (\VpcOutputSettings' {subnetIds} -> subnetIds) (\s@VpcOutputSettings' {} a -> s {subnetIds = a} :: VpcOutputSettings) Prelude.. Prelude._Coerce

instance Prelude.FromJSON VpcOutputSettings where
  parseJSON =
    Prelude.withObject
      "VpcOutputSettings"
      ( \x ->
          VpcOutputSettings'
            Prelude.<$> ( x Prelude..:? "securityGroupIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "publicAddressAllocationIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "subnetIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VpcOutputSettings

instance Prelude.NFData VpcOutputSettings

instance Prelude.ToJSON VpcOutputSettings where
  toJSON VpcOutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Prelude..=)
              Prelude.<$> securityGroupIds,
            ("publicAddressAllocationIds" Prelude..=)
              Prelude.<$> publicAddressAllocationIds,
            Prelude.Just ("subnetIds" Prelude..= subnetIds)
          ]
      )

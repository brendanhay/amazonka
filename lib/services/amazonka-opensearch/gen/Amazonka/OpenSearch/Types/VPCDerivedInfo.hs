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
-- Module      : Amazonka.OpenSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VPCDerivedInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the subnets and security groups for an Amazon
-- OpenSearch Service domain provisioned within a virtual private cloud
-- (VPC). For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/vpc.html Launching your Amazon OpenSearch Service domains using a VPC>.
-- This information only exists if the domain was created with
-- @VPCOptions@.
--
-- /See:/ 'newVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { -- | The list of Availability Zones associated with the VPC subnets.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The list of security group IDs associated with the VPC endpoints for the
    -- domain.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of subnet IDs associated with the VPC endpoints for the domain.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID for your VPC. Amazon VPC generates this value when you create a
    -- VPC.
    vPCId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCDerivedInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'vPCDerivedInfo_availabilityZones' - The list of Availability Zones associated with the VPC subnets.
--
-- 'securityGroupIds', 'vPCDerivedInfo_securityGroupIds' - The list of security group IDs associated with the VPC endpoints for the
-- domain.
--
-- 'subnetIds', 'vPCDerivedInfo_subnetIds' - A list of subnet IDs associated with the VPC endpoints for the domain.
--
-- 'vPCId', 'vPCDerivedInfo_vPCId' - The ID for your VPC. Amazon VPC generates this value when you create a
-- VPC.
newVPCDerivedInfo ::
  VPCDerivedInfo
newVPCDerivedInfo =
  VPCDerivedInfo'
    { availabilityZones =
        Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vPCId = Prelude.Nothing
    }

-- | The list of Availability Zones associated with the VPC subnets.
vPCDerivedInfo_availabilityZones :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_availabilityZones = Lens.lens (\VPCDerivedInfo' {availabilityZones} -> availabilityZones) (\s@VPCDerivedInfo' {} a -> s {availabilityZones = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | The list of security group IDs associated with the VPC endpoints for the
-- domain.
vPCDerivedInfo_securityGroupIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_securityGroupIds = Lens.lens (\VPCDerivedInfo' {securityGroupIds} -> securityGroupIds) (\s@VPCDerivedInfo' {} a -> s {securityGroupIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs associated with the VPC endpoints for the domain.
vPCDerivedInfo_subnetIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_subnetIds = Lens.lens (\VPCDerivedInfo' {subnetIds} -> subnetIds) (\s@VPCDerivedInfo' {} a -> s {subnetIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | The ID for your VPC. Amazon VPC generates this value when you create a
-- VPC.
vPCDerivedInfo_vPCId :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe Prelude.Text)
vPCDerivedInfo_vPCId = Lens.lens (\VPCDerivedInfo' {vPCId} -> vPCId) (\s@VPCDerivedInfo' {} a -> s {vPCId = a} :: VPCDerivedInfo)

instance Data.FromJSON VPCDerivedInfo where
  parseJSON =
    Data.withObject
      "VPCDerivedInfo"
      ( \x ->
          VPCDerivedInfo'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VPCId")
      )

instance Prelude.Hashable VPCDerivedInfo where
  hashWithSalt _salt VPCDerivedInfo' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vPCId

instance Prelude.NFData VPCDerivedInfo where
  rnf VPCDerivedInfo' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vPCId

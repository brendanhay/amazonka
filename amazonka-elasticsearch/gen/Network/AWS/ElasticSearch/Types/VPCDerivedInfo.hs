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
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- /See:/ 'newVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The availability zones for the Elasticsearch domain. Exists only if the
    -- domain was created with VPCOptions.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
    -- created with VPCOptions.
    vPCId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VPCDerivedInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vPCDerivedInfo_securityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- 'availabilityZones', 'vPCDerivedInfo_availabilityZones' - The availability zones for the Elasticsearch domain. Exists only if the
-- domain was created with VPCOptions.
--
-- 'subnetIds', 'vPCDerivedInfo_subnetIds' - Specifies the subnets for VPC endpoint.
--
-- 'vPCId', 'vPCDerivedInfo_vPCId' - The VPC Id for the Elasticsearch domain. Exists only if the domain was
-- created with VPCOptions.
newVPCDerivedInfo ::
  VPCDerivedInfo
newVPCDerivedInfo =
  VPCDerivedInfo'
    { securityGroupIds = Core.Nothing,
      availabilityZones = Core.Nothing,
      subnetIds = Core.Nothing,
      vPCId = Core.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
vPCDerivedInfo_securityGroupIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vPCDerivedInfo_securityGroupIds = Lens.lens (\VPCDerivedInfo' {securityGroupIds} -> securityGroupIds) (\s@VPCDerivedInfo' {} a -> s {securityGroupIds = a} :: VPCDerivedInfo) Core.. Lens.mapping Lens._Coerce

-- | The availability zones for the Elasticsearch domain. Exists only if the
-- domain was created with VPCOptions.
vPCDerivedInfo_availabilityZones :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vPCDerivedInfo_availabilityZones = Lens.lens (\VPCDerivedInfo' {availabilityZones} -> availabilityZones) (\s@VPCDerivedInfo' {} a -> s {availabilityZones = a} :: VPCDerivedInfo) Core.. Lens.mapping Lens._Coerce

-- | Specifies the subnets for VPC endpoint.
vPCDerivedInfo_subnetIds :: Lens.Lens' VPCDerivedInfo (Core.Maybe [Core.Text])
vPCDerivedInfo_subnetIds = Lens.lens (\VPCDerivedInfo' {subnetIds} -> subnetIds) (\s@VPCDerivedInfo' {} a -> s {subnetIds = a} :: VPCDerivedInfo) Core.. Lens.mapping Lens._Coerce

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
-- created with VPCOptions.
vPCDerivedInfo_vPCId :: Lens.Lens' VPCDerivedInfo (Core.Maybe Core.Text)
vPCDerivedInfo_vPCId = Lens.lens (\VPCDerivedInfo' {vPCId} -> vPCId) (\s@VPCDerivedInfo' {} a -> s {vPCId = a} :: VPCDerivedInfo)

instance Core.FromJSON VPCDerivedInfo where
  parseJSON =
    Core.withObject
      "VPCDerivedInfo"
      ( \x ->
          VPCDerivedInfo'
            Core.<$> (x Core..:? "SecurityGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AvailabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VPCId")
      )

instance Core.Hashable VPCDerivedInfo

instance Core.NFData VPCDerivedInfo

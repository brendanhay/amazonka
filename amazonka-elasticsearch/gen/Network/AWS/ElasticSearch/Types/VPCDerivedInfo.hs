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
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- /See:/ 'newVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The availability zones for the Elasticsearch domain. Exists only if the
    -- domain was created with VPCOptions.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
    -- created with VPCOptions.
    vPCId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { securityGroupIds = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vPCId = Prelude.Nothing
    }

-- | Specifies the security groups for VPC endpoint.
vPCDerivedInfo_securityGroupIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_securityGroupIds = Lens.lens (\VPCDerivedInfo' {securityGroupIds} -> securityGroupIds) (\s@VPCDerivedInfo' {} a -> s {securityGroupIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The availability zones for the Elasticsearch domain. Exists only if the
-- domain was created with VPCOptions.
vPCDerivedInfo_availabilityZones :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_availabilityZones = Lens.lens (\VPCDerivedInfo' {availabilityZones} -> availabilityZones) (\s@VPCDerivedInfo' {} a -> s {availabilityZones = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the subnets for VPC endpoint.
vPCDerivedInfo_subnetIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_subnetIds = Lens.lens (\VPCDerivedInfo' {subnetIds} -> subnetIds) (\s@VPCDerivedInfo' {} a -> s {subnetIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
-- created with VPCOptions.
vPCDerivedInfo_vPCId :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe Prelude.Text)
vPCDerivedInfo_vPCId = Lens.lens (\VPCDerivedInfo' {vPCId} -> vPCId) (\s@VPCDerivedInfo' {} a -> s {vPCId = a} :: VPCDerivedInfo)

instance Prelude.FromJSON VPCDerivedInfo where
  parseJSON =
    Prelude.withObject
      "VPCDerivedInfo"
      ( \x ->
          VPCDerivedInfo'
            Prelude.<$> ( x Prelude..:? "SecurityGroupIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "AvailabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "VPCId")
      )

instance Prelude.Hashable VPCDerivedInfo

instance Prelude.NFData VPCDerivedInfo

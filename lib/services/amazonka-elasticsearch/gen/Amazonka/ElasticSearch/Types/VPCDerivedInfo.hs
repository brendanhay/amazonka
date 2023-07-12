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
-- Module      : Amazonka.ElasticSearch.Types.VPCDerivedInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.VPCDerivedInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options to specify the subnets and security groups for VPC endpoint. For
-- more information, see
-- <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains>.
--
-- /See:/ 'newVPCDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { -- | The availability zones for the Elasticsearch domain. Exists only if the
    -- domain was created with VPCOptions.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the security groups for VPC endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the subnets for VPC endpoint.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
    -- created with VPCOptions.
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
-- 'availabilityZones', 'vPCDerivedInfo_availabilityZones' - The availability zones for the Elasticsearch domain. Exists only if the
-- domain was created with VPCOptions.
--
-- 'securityGroupIds', 'vPCDerivedInfo_securityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- 'subnetIds', 'vPCDerivedInfo_subnetIds' - Specifies the subnets for VPC endpoint.
--
-- 'vPCId', 'vPCDerivedInfo_vPCId' - The VPC Id for the Elasticsearch domain. Exists only if the domain was
-- created with VPCOptions.
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

-- | The availability zones for the Elasticsearch domain. Exists only if the
-- domain was created with VPCOptions.
vPCDerivedInfo_availabilityZones :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_availabilityZones = Lens.lens (\VPCDerivedInfo' {availabilityZones} -> availabilityZones) (\s@VPCDerivedInfo' {} a -> s {availabilityZones = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the security groups for VPC endpoint.
vPCDerivedInfo_securityGroupIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_securityGroupIds = Lens.lens (\VPCDerivedInfo' {securityGroupIds} -> securityGroupIds) (\s@VPCDerivedInfo' {} a -> s {securityGroupIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the subnets for VPC endpoint.
vPCDerivedInfo_subnetIds :: Lens.Lens' VPCDerivedInfo (Prelude.Maybe [Prelude.Text])
vPCDerivedInfo_subnetIds = Lens.lens (\VPCDerivedInfo' {subnetIds} -> subnetIds) (\s@VPCDerivedInfo' {} a -> s {subnetIds = a} :: VPCDerivedInfo) Prelude.. Lens.mapping Lens.coerced

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was
-- created with VPCOptions.
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

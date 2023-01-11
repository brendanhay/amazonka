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
-- Module      : Amazonka.SecurityHub.Types.AwsEksClusterResourcesVpcConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterResourcesVpcConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the VPC configuration used by the cluster control
-- plane.
--
-- /See:/ 'newAwsEksClusterResourcesVpcConfigDetails' smart constructor.
data AwsEksClusterResourcesVpcConfigDetails = AwsEksClusterResourcesVpcConfigDetails'
  { -- | The security groups that are associated with the cross-account elastic
    -- network interfaces that are used to allow communication between your
    -- nodes and the Amazon EKS control plane.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The subnets that are associated with the cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEksClusterResourcesVpcConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'awsEksClusterResourcesVpcConfigDetails_securityGroupIds' - The security groups that are associated with the cross-account elastic
-- network interfaces that are used to allow communication between your
-- nodes and the Amazon EKS control plane.
--
-- 'subnetIds', 'awsEksClusterResourcesVpcConfigDetails_subnetIds' - The subnets that are associated with the cluster.
newAwsEksClusterResourcesVpcConfigDetails ::
  AwsEksClusterResourcesVpcConfigDetails
newAwsEksClusterResourcesVpcConfigDetails =
  AwsEksClusterResourcesVpcConfigDetails'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The security groups that are associated with the cross-account elastic
-- network interfaces that are used to allow communication between your
-- nodes and the Amazon EKS control plane.
awsEksClusterResourcesVpcConfigDetails_securityGroupIds :: Lens.Lens' AwsEksClusterResourcesVpcConfigDetails (Prelude.Maybe [Prelude.Text])
awsEksClusterResourcesVpcConfigDetails_securityGroupIds = Lens.lens (\AwsEksClusterResourcesVpcConfigDetails' {securityGroupIds} -> securityGroupIds) (\s@AwsEksClusterResourcesVpcConfigDetails' {} a -> s {securityGroupIds = a} :: AwsEksClusterResourcesVpcConfigDetails) Prelude.. Lens.mapping Lens.coerced

-- | The subnets that are associated with the cluster.
awsEksClusterResourcesVpcConfigDetails_subnetIds :: Lens.Lens' AwsEksClusterResourcesVpcConfigDetails (Prelude.Maybe [Prelude.Text])
awsEksClusterResourcesVpcConfigDetails_subnetIds = Lens.lens (\AwsEksClusterResourcesVpcConfigDetails' {subnetIds} -> subnetIds) (\s@AwsEksClusterResourcesVpcConfigDetails' {} a -> s {subnetIds = a} :: AwsEksClusterResourcesVpcConfigDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEksClusterResourcesVpcConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsEksClusterResourcesVpcConfigDetails"
      ( \x ->
          AwsEksClusterResourcesVpcConfigDetails'
            Prelude.<$> ( x Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEksClusterResourcesVpcConfigDetails
  where
  hashWithSalt
    _salt
    AwsEksClusterResourcesVpcConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    AwsEksClusterResourcesVpcConfigDetails
  where
  rnf AwsEksClusterResourcesVpcConfigDetails' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance
  Data.ToJSON
    AwsEksClusterResourcesVpcConfigDetails
  where
  toJSON AwsEksClusterResourcesVpcConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds
          ]
      )

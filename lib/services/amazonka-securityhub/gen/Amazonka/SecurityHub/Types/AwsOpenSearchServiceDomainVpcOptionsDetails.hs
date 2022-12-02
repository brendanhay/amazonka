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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainVpcOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainVpcOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information that OpenSearch Service derives based on the
-- @VPCOptions@ for the domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainVpcOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainVpcOptionsDetails = AwsOpenSearchServiceDomainVpcOptionsDetails'
  { -- | The list of security group IDs that are associated with the VPC
    -- endpoints for the domain.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of subnet IDs that are associated with the VPC endpoints for the
    -- domain.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainVpcOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds' - The list of security group IDs that are associated with the VPC
-- endpoints for the domain.
--
-- 'subnetIds', 'awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds' - A list of subnet IDs that are associated with the VPC endpoints for the
-- domain.
newAwsOpenSearchServiceDomainVpcOptionsDetails ::
  AwsOpenSearchServiceDomainVpcOptionsDetails
newAwsOpenSearchServiceDomainVpcOptionsDetails =
  AwsOpenSearchServiceDomainVpcOptionsDetails'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The list of security group IDs that are associated with the VPC
-- endpoints for the domain.
awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds :: Lens.Lens' AwsOpenSearchServiceDomainVpcOptionsDetails (Prelude.Maybe [Prelude.Text])
awsOpenSearchServiceDomainVpcOptionsDetails_securityGroupIds = Lens.lens (\AwsOpenSearchServiceDomainVpcOptionsDetails' {securityGroupIds} -> securityGroupIds) (\s@AwsOpenSearchServiceDomainVpcOptionsDetails' {} a -> s {securityGroupIds = a} :: AwsOpenSearchServiceDomainVpcOptionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs that are associated with the VPC endpoints for the
-- domain.
awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds :: Lens.Lens' AwsOpenSearchServiceDomainVpcOptionsDetails (Prelude.Maybe [Prelude.Text])
awsOpenSearchServiceDomainVpcOptionsDetails_subnetIds = Lens.lens (\AwsOpenSearchServiceDomainVpcOptionsDetails' {subnetIds} -> subnetIds) (\s@AwsOpenSearchServiceDomainVpcOptionsDetails' {} a -> s {subnetIds = a} :: AwsOpenSearchServiceDomainVpcOptionsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainVpcOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainVpcOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainVpcOptionsDetails'
            Prelude.<$> ( x Data..:? "SecurityGroupIds"
                            Data..!= Prelude.mempty
                        )
              Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainVpcOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainVpcOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` subnetIds

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainVpcOptionsDetails
  where
  rnf AwsOpenSearchServiceDomainVpcOptionsDetails' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainVpcOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainVpcOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("SecurityGroupIds" Data..=)
                Prelude.<$> securityGroupIds,
              ("SubnetIds" Data..=) Prelude.<$> subnetIds
            ]
        )

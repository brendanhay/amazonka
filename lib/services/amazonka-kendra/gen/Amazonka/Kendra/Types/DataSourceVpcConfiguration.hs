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
-- Module      : Amazonka.Kendra.Types.DataSourceVpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceVpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to an Amazon VPC.
--
-- /See:/ 'newDataSourceVpcConfiguration' smart constructor.
data DataSourceVpcConfiguration = DataSourceVpcConfiguration'
  { -- | A list of identifiers for subnets within your Amazon VPC. The subnets
    -- should be able to connect to each other in the VPC, and they should have
    -- outgoing access to the Internet through a NAT device.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | A list of identifiers of security groups within your Amazon VPC. The
    -- security groups should enable Amazon Kendra to connect to the data
    -- source.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIds', 'dataSourceVpcConfiguration_subnetIds' - A list of identifiers for subnets within your Amazon VPC. The subnets
-- should be able to connect to each other in the VPC, and they should have
-- outgoing access to the Internet through a NAT device.
--
-- 'securityGroupIds', 'dataSourceVpcConfiguration_securityGroupIds' - A list of identifiers of security groups within your Amazon VPC. The
-- security groups should enable Amazon Kendra to connect to the data
-- source.
newDataSourceVpcConfiguration ::
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  DataSourceVpcConfiguration
newDataSourceVpcConfiguration
  pSubnetIds_
  pSecurityGroupIds_ =
    DataSourceVpcConfiguration'
      { subnetIds =
          Lens.coerced Lens.# pSubnetIds_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_
      }

-- | A list of identifiers for subnets within your Amazon VPC. The subnets
-- should be able to connect to each other in the VPC, and they should have
-- outgoing access to the Internet through a NAT device.
dataSourceVpcConfiguration_subnetIds :: Lens.Lens' DataSourceVpcConfiguration (Prelude.NonEmpty Prelude.Text)
dataSourceVpcConfiguration_subnetIds = Lens.lens (\DataSourceVpcConfiguration' {subnetIds} -> subnetIds) (\s@DataSourceVpcConfiguration' {} a -> s {subnetIds = a} :: DataSourceVpcConfiguration) Prelude.. Lens.coerced

-- | A list of identifiers of security groups within your Amazon VPC. The
-- security groups should enable Amazon Kendra to connect to the data
-- source.
dataSourceVpcConfiguration_securityGroupIds :: Lens.Lens' DataSourceVpcConfiguration (Prelude.NonEmpty Prelude.Text)
dataSourceVpcConfiguration_securityGroupIds = Lens.lens (\DataSourceVpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@DataSourceVpcConfiguration' {} a -> s {securityGroupIds = a} :: DataSourceVpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON DataSourceVpcConfiguration where
  parseJSON =
    Data.withObject
      "DataSourceVpcConfiguration"
      ( \x ->
          DataSourceVpcConfiguration'
            Prelude.<$> (x Data..: "SubnetIds")
            Prelude.<*> (x Data..: "SecurityGroupIds")
      )

instance Prelude.Hashable DataSourceVpcConfiguration where
  hashWithSalt _salt DataSourceVpcConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds

instance Prelude.NFData DataSourceVpcConfiguration where
  rnf DataSourceVpcConfiguration' {..} =
    Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds

instance Data.ToJSON DataSourceVpcConfiguration where
  toJSON DataSourceVpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds)
          ]
      )

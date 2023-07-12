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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- /See:/ 'newAwsEcsServiceNetworkConfigurationDetails' smart constructor.
data AwsEcsServiceNetworkConfigurationDetails = AwsEcsServiceNetworkConfigurationDetails'
  { -- | The VPC subnet and security group configuration.
    awsVpcConfiguration :: Prelude.Maybe AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceNetworkConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsVpcConfiguration', 'awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration' - The VPC subnet and security group configuration.
newAwsEcsServiceNetworkConfigurationDetails ::
  AwsEcsServiceNetworkConfigurationDetails
newAwsEcsServiceNetworkConfigurationDetails =
  AwsEcsServiceNetworkConfigurationDetails'
    { awsVpcConfiguration =
        Prelude.Nothing
    }

-- | The VPC subnet and security group configuration.
awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration :: Lens.Lens' AwsEcsServiceNetworkConfigurationDetails (Prelude.Maybe AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails)
awsEcsServiceNetworkConfigurationDetails_awsVpcConfiguration = Lens.lens (\AwsEcsServiceNetworkConfigurationDetails' {awsVpcConfiguration} -> awsVpcConfiguration) (\s@AwsEcsServiceNetworkConfigurationDetails' {} a -> s {awsVpcConfiguration = a} :: AwsEcsServiceNetworkConfigurationDetails)

instance
  Data.FromJSON
    AwsEcsServiceNetworkConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsServiceNetworkConfigurationDetails"
      ( \x ->
          AwsEcsServiceNetworkConfigurationDetails'
            Prelude.<$> (x Data..:? "AwsVpcConfiguration")
      )

instance
  Prelude.Hashable
    AwsEcsServiceNetworkConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceNetworkConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` awsVpcConfiguration

instance
  Prelude.NFData
    AwsEcsServiceNetworkConfigurationDetails
  where
  rnf AwsEcsServiceNetworkConfigurationDetails' {..} =
    Prelude.rnf awsVpcConfiguration

instance
  Data.ToJSON
    AwsEcsServiceNetworkConfigurationDetails
  where
  toJSON AwsEcsServiceNetworkConfigurationDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsVpcConfiguration" Data..=)
              Prelude.<$> awsVpcConfiguration
          ]
      )

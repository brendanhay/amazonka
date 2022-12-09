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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- /See:/ 'newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' smart constructor.
data AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails = AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
  { -- | Whether the task\'s elastic network interface receives a public IP
    -- address. The default value is @DISABLED@.
    --
    -- Valid values: @ENABLED@ | @DISABLED@
    assignPublicIp :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups associated with the task or service.
    --
    -- You can provide up to five security groups.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the subnets associated with the task or service.
    --
    -- You can provide up to 16 subnets.
    subnets :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignPublicIp', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp' - Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- Valid values: @ENABLED@ | @DISABLED@
--
-- 'securityGroups', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups' - The IDs of the security groups associated with the task or service.
--
-- You can provide up to five security groups.
--
-- 'subnets', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets' - The IDs of the subnets associated with the task or service.
--
-- You can provide up to 16 subnets.
newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails ::
  AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails =
  AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
    { assignPublicIp =
        Prelude.Nothing,
      securityGroups =
        Prelude.Nothing,
      subnets =
        Prelude.Nothing
    }

-- | Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- Valid values: @ENABLED@ | @DISABLED@
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {assignPublicIp} -> assignPublicIp) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {assignPublicIp = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails)

-- | The IDs of the security groups associated with the task or service.
--
-- You can provide up to five security groups.
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {securityGroups} -> securityGroups) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {securityGroups = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the subnets associated with the task or service.
--
-- You can provide up to 16 subnets.
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {subnets} -> subnets) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {subnets = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails"
      ( \x ->
          AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
            Prelude.<$> (x Data..:? "AssignPublicIp")
              Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` assignPublicIp
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` subnets

instance
  Prelude.NFData
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  rnf
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      Prelude.rnf assignPublicIp
        `Prelude.seq` Prelude.rnf securityGroups
        `Prelude.seq` Prelude.rnf subnets

instance
  Data.ToJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  toJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AssignPublicIp" Data..=)
                Prelude.<$> assignPublicIp,
              ("SecurityGroups" Data..=)
                Prelude.<$> securityGroups,
              ("Subnets" Data..=) Prelude.<$> subnets
            ]
        )

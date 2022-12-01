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
import qualified Amazonka.Prelude as Prelude

-- | For tasks that use the @awsvpc@ networking mode, the VPC subnet and
-- security group configuration.
--
-- /See:/ 'newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' smart constructor.
data AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails = AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
  { -- | The IDs of the subnets associated with the task or service.
    --
    -- You can provide up to 16 subnets.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the security groups associated with the task or service.
    --
    -- You can provide up to five security groups.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Whether the task\'s elastic network interface receives a public IP
    -- address. The default value is @DISABLED@.
    --
    -- Valid values: @ENABLED@ | @DISABLED@
    assignPublicIp :: Prelude.Maybe Prelude.Text
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
-- 'subnets', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets' - The IDs of the subnets associated with the task or service.
--
-- You can provide up to 16 subnets.
--
-- 'securityGroups', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups' - The IDs of the security groups associated with the task or service.
--
-- You can provide up to five security groups.
--
-- 'assignPublicIp', 'awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp' - Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- Valid values: @ENABLED@ | @DISABLED@
newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails ::
  AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
newAwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails =
  AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
    { subnets =
        Prelude.Nothing,
      securityGroups =
        Prelude.Nothing,
      assignPublicIp =
        Prelude.Nothing
    }

-- | The IDs of the subnets associated with the task or service.
--
-- You can provide up to 16 subnets.
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_subnets = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {subnets} -> subnets) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {subnets = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the security groups associated with the task or service.
--
-- You can provide up to five security groups.
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_securityGroups = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {securityGroups} -> securityGroups) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {securityGroups = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the task\'s elastic network interface receives a public IP
-- address. The default value is @DISABLED@.
--
-- Valid values: @ENABLED@ | @DISABLED@
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp :: Lens.Lens' AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails_assignPublicIp = Lens.lens (\AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {assignPublicIp} -> assignPublicIp) (\s@AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {} a -> s {assignPublicIp = a} :: AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails)

instance
  Core.FromJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails"
      ( \x ->
          AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails'
            Prelude.<$> (x Core..:? "Subnets" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "AssignPublicIp")
      )

instance
  Prelude.Hashable
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` subnets
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` assignPublicIp

instance
  Prelude.NFData
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  rnf
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      Prelude.rnf subnets
        `Prelude.seq` Prelude.rnf securityGroups
        `Prelude.seq` Prelude.rnf assignPublicIp

instance
  Core.ToJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails
  where
  toJSON
    AwsEcsServiceNetworkConfigurationAwsVpcConfigurationDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Subnets" Core..=) Prelude.<$> subnets,
              ("SecurityGroups" Core..=)
                Prelude.<$> securityGroups,
              ("AssignPublicIp" Core..=)
                Prelude.<$> assignPublicIp
            ]
        )

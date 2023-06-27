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
-- Module      : Amazonka.MwAA.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.NetworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the VPC networking components used to secure and enable
-- network traffic between the Amazon Web Services resources for your
-- environment. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | A list of security group IDs. For more information, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of subnet IDs. For more information, see
    -- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'networkConfiguration_securityGroupIds' - A list of security group IDs. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
--
-- 'subnetIds', 'networkConfiguration_subnetIds' - A list of subnet IDs. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | A list of security group IDs. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/vpc-security.html Security in your VPC on Amazon MWAA>.
networkConfiguration_securityGroupIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkConfiguration_securityGroupIds = Lens.lens (\NetworkConfiguration' {securityGroupIds} -> securityGroupIds) (\s@NetworkConfiguration' {} a -> s {securityGroupIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs. For more information, see
-- <https://docs.aws.amazon.com/mwaa/latest/userguide/networking-about.html About networking on Amazon MWAA>.
networkConfiguration_subnetIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkConfiguration_subnetIds = Lens.lens (\NetworkConfiguration' {subnetIds} -> subnetIds) (\s@NetworkConfiguration' {} a -> s {subnetIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkConfiguration where
  parseJSON =
    Data.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Data..:? "SecurityGroupIds")
            Prelude.<*> (x Data..:? "SubnetIds")
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt _salt NetworkConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Data..=) Prelude.<$> subnetIds
          ]
      )

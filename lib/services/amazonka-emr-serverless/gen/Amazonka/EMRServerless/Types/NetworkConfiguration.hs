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
-- Module      : Amazonka.EMRServerless.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.NetworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The network configuration for customer VPC connectivity.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | The array of security group Ids for customer VPC connectivity.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The array of subnet Ids for customer VPC connectivity.
    subnetIds :: Prelude.Maybe [Prelude.Text]
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
-- 'securityGroupIds', 'networkConfiguration_securityGroupIds' - The array of security group Ids for customer VPC connectivity.
--
-- 'subnetIds', 'networkConfiguration_subnetIds' - The array of subnet Ids for customer VPC connectivity.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The array of security group Ids for customer VPC connectivity.
networkConfiguration_securityGroupIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe [Prelude.Text])
networkConfiguration_securityGroupIds = Lens.lens (\NetworkConfiguration' {securityGroupIds} -> securityGroupIds) (\s@NetworkConfiguration' {} a -> s {securityGroupIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The array of subnet Ids for customer VPC connectivity.
networkConfiguration_subnetIds :: Lens.Lens' NetworkConfiguration (Prelude.Maybe [Prelude.Text])
networkConfiguration_subnetIds = Lens.lens (\NetworkConfiguration' {subnetIds} -> subnetIds) (\s@NetworkConfiguration' {} a -> s {subnetIds = a} :: NetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkConfiguration where
  parseJSON =
    Data.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> ( x Data..:? "securityGroupIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt _salt NetworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("subnetIds" Data..=) Prelude.<$> subnetIds
          ]
      )

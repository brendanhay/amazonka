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
-- Module      : Network.AWS.ECS.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkConfiguration where

import Network.AWS.ECS.Types.AwsVpcConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the network configuration for a task or service.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | The VPC subnets and security groups associated with a task.
    --
    -- All specified subnets and security groups must be from the same VPC.
    awsvpcConfiguration :: Prelude.Maybe AwsVpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsvpcConfiguration', 'networkConfiguration_awsvpcConfiguration' - The VPC subnets and security groups associated with a task.
--
-- All specified subnets and security groups must be from the same VPC.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { awsvpcConfiguration =
        Prelude.Nothing
    }

-- | The VPC subnets and security groups associated with a task.
--
-- All specified subnets and security groups must be from the same VPC.
networkConfiguration_awsvpcConfiguration :: Lens.Lens' NetworkConfiguration (Prelude.Maybe AwsVpcConfiguration)
networkConfiguration_awsvpcConfiguration = Lens.lens (\NetworkConfiguration' {awsvpcConfiguration} -> awsvpcConfiguration) (\s@NetworkConfiguration' {} a -> s {awsvpcConfiguration = a} :: NetworkConfiguration)

instance Prelude.FromJSON NetworkConfiguration where
  parseJSON =
    Prelude.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Prelude..:? "awsvpcConfiguration")
      )

instance Prelude.Hashable NetworkConfiguration

instance Prelude.NFData NetworkConfiguration

instance Prelude.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("awsvpcConfiguration" Prelude..=)
              Prelude.<$> awsvpcConfiguration
          ]
      )

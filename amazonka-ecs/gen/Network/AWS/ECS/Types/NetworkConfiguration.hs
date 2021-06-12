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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.AwsVpcConfiguration
import qualified Network.AWS.Lens as Lens

-- | An object representing the network configuration for a task or service.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | The VPC subnets and security groups associated with a task.
    --
    -- All specified subnets and security groups must be from the same VPC.
    awsvpcConfiguration :: Core.Maybe AwsVpcConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The VPC subnets and security groups associated with a task.
--
-- All specified subnets and security groups must be from the same VPC.
networkConfiguration_awsvpcConfiguration :: Lens.Lens' NetworkConfiguration (Core.Maybe AwsVpcConfiguration)
networkConfiguration_awsvpcConfiguration = Lens.lens (\NetworkConfiguration' {awsvpcConfiguration} -> awsvpcConfiguration) (\s@NetworkConfiguration' {} a -> s {awsvpcConfiguration = a} :: NetworkConfiguration)

instance Core.FromJSON NetworkConfiguration where
  parseJSON =
    Core.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Core.<$> (x Core..:? "awsvpcConfiguration")
      )

instance Core.Hashable NetworkConfiguration

instance Core.NFData NetworkConfiguration

instance Core.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("awsvpcConfiguration" Core..=)
              Core.<$> awsvpcConfiguration
          ]
      )

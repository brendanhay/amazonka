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
-- Module      : Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.NetworkConfiguration where

import Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure specifies the network configuration for an ECS task.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | Use this structure to specify the VPC subnets and security groups for
    -- the task, and whether a public IP address is to be used. This structure
    -- is relevant only for ECS tasks that use the @awsvpc@ network mode.
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
-- 'awsvpcConfiguration', 'networkConfiguration_awsvpcConfiguration' - Use this structure to specify the VPC subnets and security groups for
-- the task, and whether a public IP address is to be used. This structure
-- is relevant only for ECS tasks that use the @awsvpc@ network mode.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { awsvpcConfiguration =
        Prelude.Nothing
    }

-- | Use this structure to specify the VPC subnets and security groups for
-- the task, and whether a public IP address is to be used. This structure
-- is relevant only for ECS tasks that use the @awsvpc@ network mode.
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

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
-- Module      : Network.AWS.Batch.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NetworkConfiguration where

import Network.AWS.Batch.Types.AssignPublicIp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The network configuration for jobs running on Fargate resources. Jobs
-- running on EC2 resources must not specify this parameter.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | Indicates whether the job should have a public IP address. For a job
    -- running on Fargate resources in a private subnet to send outbound
    -- traffic to the internet (for example, in order to pull container
    -- images), the private subnet requires a NAT gateway be attached to route
    -- requests to the internet. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>.
    -- The default value is \"DISABLED\".
    assignPublicIp :: Prelude.Maybe AssignPublicIp
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
-- 'assignPublicIp', 'networkConfiguration_assignPublicIp' - Indicates whether the job should have a public IP address. For a job
-- running on Fargate resources in a private subnet to send outbound
-- traffic to the internet (for example, in order to pull container
-- images), the private subnet requires a NAT gateway be attached to route
-- requests to the internet. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>.
-- The default value is \"DISABLED\".
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { assignPublicIp =
        Prelude.Nothing
    }

-- | Indicates whether the job should have a public IP address. For a job
-- running on Fargate resources in a private subnet to send outbound
-- traffic to the internet (for example, in order to pull container
-- images), the private subnet requires a NAT gateway be attached to route
-- requests to the internet. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>.
-- The default value is \"DISABLED\".
networkConfiguration_assignPublicIp :: Lens.Lens' NetworkConfiguration (Prelude.Maybe AssignPublicIp)
networkConfiguration_assignPublicIp = Lens.lens (\NetworkConfiguration' {assignPublicIp} -> assignPublicIp) (\s@NetworkConfiguration' {} a -> s {assignPublicIp = a} :: NetworkConfiguration)

instance Prelude.FromJSON NetworkConfiguration where
  parseJSON =
    Prelude.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Prelude..:? "assignPublicIp")
      )

instance Prelude.Hashable NetworkConfiguration

instance Prelude.NFData NetworkConfiguration

instance Prelude.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Prelude..=)
              Prelude.<$> assignPublicIp
          ]
      )

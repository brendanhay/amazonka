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
-- Module      : Amazonka.Batch.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NetworkConfiguration where

import Amazonka.Batch.Types.AssignPublicIp
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The network configuration for jobs that are running on Fargate
-- resources. Jobs that are running on EC2 resources must not specify this
-- parameter.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | Indicates whether the job has a public IP address. For a job that\'s
    -- running on Fargate resources in a private subnet to send outbound
    -- traffic to the internet (for example, to pull container images), the
    -- private subnet requires a NAT gateway be attached to route requests to
    -- the internet. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>
    -- in the /Amazon Elastic Container Service Developer Guide/. The default
    -- value is \"@DISABLED@\".
    assignPublicIp :: Prelude.Maybe AssignPublicIp
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
-- 'assignPublicIp', 'networkConfiguration_assignPublicIp' - Indicates whether the job has a public IP address. For a job that\'s
-- running on Fargate resources in a private subnet to send outbound
-- traffic to the internet (for example, to pull container images), the
-- private subnet requires a NAT gateway be attached to route requests to
-- the internet. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>
-- in the /Amazon Elastic Container Service Developer Guide/. The default
-- value is \"@DISABLED@\".
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { assignPublicIp =
        Prelude.Nothing
    }

-- | Indicates whether the job has a public IP address. For a job that\'s
-- running on Fargate resources in a private subnet to send outbound
-- traffic to the internet (for example, to pull container images), the
-- private subnet requires a NAT gateway be attached to route requests to
-- the internet. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Amazon ECS task networking>
-- in the /Amazon Elastic Container Service Developer Guide/. The default
-- value is \"@DISABLED@\".
networkConfiguration_assignPublicIp :: Lens.Lens' NetworkConfiguration (Prelude.Maybe AssignPublicIp)
networkConfiguration_assignPublicIp = Lens.lens (\NetworkConfiguration' {assignPublicIp} -> assignPublicIp) (\s@NetworkConfiguration' {} a -> s {assignPublicIp = a} :: NetworkConfiguration)

instance Data.FromJSON NetworkConfiguration where
  parseJSON =
    Data.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Data..:? "assignPublicIp")
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt _salt NetworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` assignPublicIp

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf assignPublicIp

instance Data.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Data..=)
              Prelude.<$> assignPublicIp
          ]
      )

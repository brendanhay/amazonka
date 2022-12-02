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
-- Module      : Amazonka.ECS.Types.ContainerDependency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerDependency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ContainerCondition
import qualified Amazonka.Prelude as Prelude

-- | The dependencies defined for container startup and shutdown. A container
-- can contain multiple dependencies. When a dependency is defined for
-- container startup, for container shutdown it is reversed.
--
-- Your Amazon ECS container instances require at least version 1.26.0 of
-- the container agent to use container dependencies. However, we recommend
-- using the latest container agent version. For information about checking
-- your agent version and updating to the latest version, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html Updating the Amazon ECS Container Agent>
-- in the /Amazon Elastic Container Service Developer Guide/. If you\'re
-- using an Amazon ECS-optimized Linux AMI, your instance needs at least
-- version 1.26.0-1 of the @ecs-init@ package. If your container instances
-- are launched from version @20190301@ or later, then they contain the
-- required versions of the container agent and @ecs-init@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized Linux AMI>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For tasks that use the Fargate launch type, the task or service requires
-- the following platforms:
--
-- -   Linux platform version @1.3.0@ or later.
--
-- -   Windows platform version @1.0.0@ or later.
--
-- /See:/ 'newContainerDependency' smart constructor.
data ContainerDependency = ContainerDependency'
  { -- | The name of a container.
    containerName :: Prelude.Text,
    -- | The dependency condition of the container. The following are the
    -- available conditions and their behavior:
    --
    -- -   @START@ - This condition emulates the behavior of links and volumes
    --     today. It validates that a dependent container is started before
    --     permitting other containers to start.
    --
    -- -   @COMPLETE@ - This condition validates that a dependent container
    --     runs to completion (exits) before permitting other containers to
    --     start. This can be useful for nonessential containers that run a
    --     script and then exit. This condition can\'t be set on an essential
    --     container.
    --
    -- -   @SUCCESS@ - This condition is the same as @COMPLETE@, but it also
    --     requires that the container exits with a @zero@ status. This
    --     condition can\'t be set on an essential container.
    --
    -- -   @HEALTHY@ - This condition validates that the dependent container
    --     passes its Docker health check before permitting other containers to
    --     start. This requires that the dependent container has health checks
    --     configured. This condition is confirmed only at task startup.
    condition :: ContainerCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDependency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'containerDependency_containerName' - The name of a container.
--
-- 'condition', 'containerDependency_condition' - The dependency condition of the container. The following are the
-- available conditions and their behavior:
--
-- -   @START@ - This condition emulates the behavior of links and volumes
--     today. It validates that a dependent container is started before
--     permitting other containers to start.
--
-- -   @COMPLETE@ - This condition validates that a dependent container
--     runs to completion (exits) before permitting other containers to
--     start. This can be useful for nonessential containers that run a
--     script and then exit. This condition can\'t be set on an essential
--     container.
--
-- -   @SUCCESS@ - This condition is the same as @COMPLETE@, but it also
--     requires that the container exits with a @zero@ status. This
--     condition can\'t be set on an essential container.
--
-- -   @HEALTHY@ - This condition validates that the dependent container
--     passes its Docker health check before permitting other containers to
--     start. This requires that the dependent container has health checks
--     configured. This condition is confirmed only at task startup.
newContainerDependency ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'condition'
  ContainerCondition ->
  ContainerDependency
newContainerDependency pContainerName_ pCondition_ =
  ContainerDependency'
    { containerName =
        pContainerName_,
      condition = pCondition_
    }

-- | The name of a container.
containerDependency_containerName :: Lens.Lens' ContainerDependency Prelude.Text
containerDependency_containerName = Lens.lens (\ContainerDependency' {containerName} -> containerName) (\s@ContainerDependency' {} a -> s {containerName = a} :: ContainerDependency)

-- | The dependency condition of the container. The following are the
-- available conditions and their behavior:
--
-- -   @START@ - This condition emulates the behavior of links and volumes
--     today. It validates that a dependent container is started before
--     permitting other containers to start.
--
-- -   @COMPLETE@ - This condition validates that a dependent container
--     runs to completion (exits) before permitting other containers to
--     start. This can be useful for nonessential containers that run a
--     script and then exit. This condition can\'t be set on an essential
--     container.
--
-- -   @SUCCESS@ - This condition is the same as @COMPLETE@, but it also
--     requires that the container exits with a @zero@ status. This
--     condition can\'t be set on an essential container.
--
-- -   @HEALTHY@ - This condition validates that the dependent container
--     passes its Docker health check before permitting other containers to
--     start. This requires that the dependent container has health checks
--     configured. This condition is confirmed only at task startup.
containerDependency_condition :: Lens.Lens' ContainerDependency ContainerCondition
containerDependency_condition = Lens.lens (\ContainerDependency' {condition} -> condition) (\s@ContainerDependency' {} a -> s {condition = a} :: ContainerDependency)

instance Data.FromJSON ContainerDependency where
  parseJSON =
    Data.withObject
      "ContainerDependency"
      ( \x ->
          ContainerDependency'
            Prelude.<$> (x Data..: "containerName")
            Prelude.<*> (x Data..: "condition")
      )

instance Prelude.Hashable ContainerDependency where
  hashWithSalt _salt ContainerDependency' {..} =
    _salt `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` condition

instance Prelude.NFData ContainerDependency where
  rnf ContainerDependency' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf condition

instance Data.ToJSON ContainerDependency where
  toJSON ContainerDependency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("containerName" Data..= containerName),
            Prelude.Just ("condition" Data..= condition)
          ]
      )

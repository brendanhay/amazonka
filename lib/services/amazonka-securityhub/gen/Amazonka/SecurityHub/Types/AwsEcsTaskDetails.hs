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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsContainerDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeDetails

-- | Provides details about a task in a cluster.
--
-- /See:/ 'newAwsEcsTaskDetails' smart constructor.
data AwsEcsTaskDetails = AwsEcsTaskDetails'
  { -- | The Amazon Resource Name (ARN) of the cluster that hosts the task.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The containers that are associated with the task.
    containers :: Prelude.Maybe [AwsEcsContainerDetails],
    -- | The Unix timestamp for the time when the task was created. More
    -- specifically, it\'s for the time when the task entered the @PENDING@
    -- state.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The name of the task group that\'s associated with the task.
    group' :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time when the task started. More
    -- specifically, it\'s for the time when the task transitioned from the
    -- @PENDING@ state to the @RUNNING@ state.
    startedAt :: Prelude.Maybe Prelude.Text,
    -- | The tag specified when a task is started. If an Amazon ECS service
    -- started the task, the @startedBy@ parameter contains the deployment ID
    -- of that service.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the task definition that creates the task.
    taskDefinitionArn :: Prelude.Maybe Prelude.Text,
    -- | The version counter for the task.
    version :: Prelude.Maybe Prelude.Text,
    -- | Details about the data volume that is used in a task definition.
    volumes :: Prelude.Maybe [AwsEcsTaskVolumeDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'awsEcsTaskDetails_clusterArn' - The Amazon Resource Name (ARN) of the cluster that hosts the task.
--
-- 'containers', 'awsEcsTaskDetails_containers' - The containers that are associated with the task.
--
-- 'createdAt', 'awsEcsTaskDetails_createdAt' - The Unix timestamp for the time when the task was created. More
-- specifically, it\'s for the time when the task entered the @PENDING@
-- state.
--
-- 'group'', 'awsEcsTaskDetails_group' - The name of the task group that\'s associated with the task.
--
-- 'startedAt', 'awsEcsTaskDetails_startedAt' - The Unix timestamp for the time when the task started. More
-- specifically, it\'s for the time when the task transitioned from the
-- @PENDING@ state to the @RUNNING@ state.
--
-- 'startedBy', 'awsEcsTaskDetails_startedBy' - The tag specified when a task is started. If an Amazon ECS service
-- started the task, the @startedBy@ parameter contains the deployment ID
-- of that service.
--
-- 'taskDefinitionArn', 'awsEcsTaskDetails_taskDefinitionArn' - The ARN of the task definition that creates the task.
--
-- 'version', 'awsEcsTaskDetails_version' - The version counter for the task.
--
-- 'volumes', 'awsEcsTaskDetails_volumes' - Details about the data volume that is used in a task definition.
newAwsEcsTaskDetails ::
  AwsEcsTaskDetails
newAwsEcsTaskDetails =
  AwsEcsTaskDetails'
    { clusterArn = Prelude.Nothing,
      containers = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      group' = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      taskDefinitionArn = Prelude.Nothing,
      version = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster that hosts the task.
awsEcsTaskDetails_clusterArn :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_clusterArn = Lens.lens (\AwsEcsTaskDetails' {clusterArn} -> clusterArn) (\s@AwsEcsTaskDetails' {} a -> s {clusterArn = a} :: AwsEcsTaskDetails)

-- | The containers that are associated with the task.
awsEcsTaskDetails_containers :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe [AwsEcsContainerDetails])
awsEcsTaskDetails_containers = Lens.lens (\AwsEcsTaskDetails' {containers} -> containers) (\s@AwsEcsTaskDetails' {} a -> s {containers = a} :: AwsEcsTaskDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for the time when the task was created. More
-- specifically, it\'s for the time when the task entered the @PENDING@
-- state.
awsEcsTaskDetails_createdAt :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_createdAt = Lens.lens (\AwsEcsTaskDetails' {createdAt} -> createdAt) (\s@AwsEcsTaskDetails' {} a -> s {createdAt = a} :: AwsEcsTaskDetails)

-- | The name of the task group that\'s associated with the task.
awsEcsTaskDetails_group :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_group = Lens.lens (\AwsEcsTaskDetails' {group'} -> group') (\s@AwsEcsTaskDetails' {} a -> s {group' = a} :: AwsEcsTaskDetails)

-- | The Unix timestamp for the time when the task started. More
-- specifically, it\'s for the time when the task transitioned from the
-- @PENDING@ state to the @RUNNING@ state.
awsEcsTaskDetails_startedAt :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_startedAt = Lens.lens (\AwsEcsTaskDetails' {startedAt} -> startedAt) (\s@AwsEcsTaskDetails' {} a -> s {startedAt = a} :: AwsEcsTaskDetails)

-- | The tag specified when a task is started. If an Amazon ECS service
-- started the task, the @startedBy@ parameter contains the deployment ID
-- of that service.
awsEcsTaskDetails_startedBy :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_startedBy = Lens.lens (\AwsEcsTaskDetails' {startedBy} -> startedBy) (\s@AwsEcsTaskDetails' {} a -> s {startedBy = a} :: AwsEcsTaskDetails)

-- | The ARN of the task definition that creates the task.
awsEcsTaskDetails_taskDefinitionArn :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_taskDefinitionArn = Lens.lens (\AwsEcsTaskDetails' {taskDefinitionArn} -> taskDefinitionArn) (\s@AwsEcsTaskDetails' {} a -> s {taskDefinitionArn = a} :: AwsEcsTaskDetails)

-- | The version counter for the task.
awsEcsTaskDetails_version :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDetails_version = Lens.lens (\AwsEcsTaskDetails' {version} -> version) (\s@AwsEcsTaskDetails' {} a -> s {version = a} :: AwsEcsTaskDetails)

-- | Details about the data volume that is used in a task definition.
awsEcsTaskDetails_volumes :: Lens.Lens' AwsEcsTaskDetails (Prelude.Maybe [AwsEcsTaskVolumeDetails])
awsEcsTaskDetails_volumes = Lens.lens (\AwsEcsTaskDetails' {volumes} -> volumes) (\s@AwsEcsTaskDetails' {} a -> s {volumes = a} :: AwsEcsTaskDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsEcsTaskDetails where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDetails"
      ( \x ->
          AwsEcsTaskDetails'
            Prelude.<$> (x Data..:? "ClusterArn")
            Prelude.<*> (x Data..:? "Containers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Group")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "StartedBy")
            Prelude.<*> (x Data..:? "TaskDefinitionArn")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "Volumes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsEcsTaskDetails where
  hashWithSalt _salt AwsEcsTaskDetails' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` taskDefinitionArn
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData AwsEcsTaskDetails where
  rnf AwsEcsTaskDetails' {..} =
    Prelude.rnf clusterArn `Prelude.seq`
      Prelude.rnf containers `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf group' `Prelude.seq`
            Prelude.rnf startedAt `Prelude.seq`
              Prelude.rnf startedBy `Prelude.seq`
                Prelude.rnf taskDefinitionArn `Prelude.seq`
                  Prelude.rnf version `Prelude.seq`
                    Prelude.rnf volumes

instance Data.ToJSON AwsEcsTaskDetails where
  toJSON AwsEcsTaskDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterArn" Data..=) Prelude.<$> clusterArn,
            ("Containers" Data..=) Prelude.<$> containers,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("Group" Data..=) Prelude.<$> group',
            ("StartedAt" Data..=) Prelude.<$> startedAt,
            ("StartedBy" Data..=) Prelude.<$> startedBy,
            ("TaskDefinitionArn" Data..=)
              Prelude.<$> taskDefinitionArn,
            ("Version" Data..=) Prelude.<$> version,
            ("Volumes" Data..=) Prelude.<$> volumes
          ]
      )

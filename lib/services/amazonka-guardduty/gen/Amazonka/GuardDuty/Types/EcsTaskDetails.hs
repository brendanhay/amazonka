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
-- Module      : Amazonka.GuardDuty.Types.EcsTaskDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.EcsTaskDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.Container
import Amazonka.GuardDuty.Types.Tag
import Amazonka.GuardDuty.Types.Volume
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the task in an ECS cluster.
--
-- /See:/ 'newEcsTaskDetails' smart constructor.
data EcsTaskDetails = EcsTaskDetails'
  { -- | The tags of the ECS Task.
    tags :: Prelude.Maybe [Tag],
    -- | The containers that\'s associated with the task.
    containers :: Prelude.Maybe [Container],
    -- | The Amazon Resource Name (ARN) of the task.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Contains the tag specified when a task is started.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The list of data volume definitions for the task.
    volumes :: Prelude.Maybe [Volume],
    -- | The Unix timestamp for the time when the task was created.
    taskCreatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix timestamp for the time when the task started.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the task definition that creates the task.
    definitionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the task group that\'s associated with the task.
    group' :: Prelude.Maybe Prelude.Text,
    -- | The version counter for the task.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsTaskDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ecsTaskDetails_tags' - The tags of the ECS Task.
--
-- 'containers', 'ecsTaskDetails_containers' - The containers that\'s associated with the task.
--
-- 'arn', 'ecsTaskDetails_arn' - The Amazon Resource Name (ARN) of the task.
--
-- 'startedBy', 'ecsTaskDetails_startedBy' - Contains the tag specified when a task is started.
--
-- 'volumes', 'ecsTaskDetails_volumes' - The list of data volume definitions for the task.
--
-- 'taskCreatedAt', 'ecsTaskDetails_taskCreatedAt' - The Unix timestamp for the time when the task was created.
--
-- 'startedAt', 'ecsTaskDetails_startedAt' - The Unix timestamp for the time when the task started.
--
-- 'definitionArn', 'ecsTaskDetails_definitionArn' - The ARN of the task definition that creates the task.
--
-- 'group'', 'ecsTaskDetails_group' - The name of the task group that\'s associated with the task.
--
-- 'version', 'ecsTaskDetails_version' - The version counter for the task.
newEcsTaskDetails ::
  EcsTaskDetails
newEcsTaskDetails =
  EcsTaskDetails'
    { tags = Prelude.Nothing,
      containers = Prelude.Nothing,
      arn = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      volumes = Prelude.Nothing,
      taskCreatedAt = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      definitionArn = Prelude.Nothing,
      group' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The tags of the ECS Task.
ecsTaskDetails_tags :: Lens.Lens' EcsTaskDetails (Prelude.Maybe [Tag])
ecsTaskDetails_tags = Lens.lens (\EcsTaskDetails' {tags} -> tags) (\s@EcsTaskDetails' {} a -> s {tags = a} :: EcsTaskDetails) Prelude.. Lens.mapping Lens.coerced

-- | The containers that\'s associated with the task.
ecsTaskDetails_containers :: Lens.Lens' EcsTaskDetails (Prelude.Maybe [Container])
ecsTaskDetails_containers = Lens.lens (\EcsTaskDetails' {containers} -> containers) (\s@EcsTaskDetails' {} a -> s {containers = a} :: EcsTaskDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the task.
ecsTaskDetails_arn :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.Text)
ecsTaskDetails_arn = Lens.lens (\EcsTaskDetails' {arn} -> arn) (\s@EcsTaskDetails' {} a -> s {arn = a} :: EcsTaskDetails)

-- | Contains the tag specified when a task is started.
ecsTaskDetails_startedBy :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.Text)
ecsTaskDetails_startedBy = Lens.lens (\EcsTaskDetails' {startedBy} -> startedBy) (\s@EcsTaskDetails' {} a -> s {startedBy = a} :: EcsTaskDetails)

-- | The list of data volume definitions for the task.
ecsTaskDetails_volumes :: Lens.Lens' EcsTaskDetails (Prelude.Maybe [Volume])
ecsTaskDetails_volumes = Lens.lens (\EcsTaskDetails' {volumes} -> volumes) (\s@EcsTaskDetails' {} a -> s {volumes = a} :: EcsTaskDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for the time when the task was created.
ecsTaskDetails_taskCreatedAt :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.UTCTime)
ecsTaskDetails_taskCreatedAt = Lens.lens (\EcsTaskDetails' {taskCreatedAt} -> taskCreatedAt) (\s@EcsTaskDetails' {} a -> s {taskCreatedAt = a} :: EcsTaskDetails) Prelude.. Lens.mapping Core._Time

-- | The Unix timestamp for the time when the task started.
ecsTaskDetails_startedAt :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.UTCTime)
ecsTaskDetails_startedAt = Lens.lens (\EcsTaskDetails' {startedAt} -> startedAt) (\s@EcsTaskDetails' {} a -> s {startedAt = a} :: EcsTaskDetails) Prelude.. Lens.mapping Core._Time

-- | The ARN of the task definition that creates the task.
ecsTaskDetails_definitionArn :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.Text)
ecsTaskDetails_definitionArn = Lens.lens (\EcsTaskDetails' {definitionArn} -> definitionArn) (\s@EcsTaskDetails' {} a -> s {definitionArn = a} :: EcsTaskDetails)

-- | The name of the task group that\'s associated with the task.
ecsTaskDetails_group :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.Text)
ecsTaskDetails_group = Lens.lens (\EcsTaskDetails' {group'} -> group') (\s@EcsTaskDetails' {} a -> s {group' = a} :: EcsTaskDetails)

-- | The version counter for the task.
ecsTaskDetails_version :: Lens.Lens' EcsTaskDetails (Prelude.Maybe Prelude.Text)
ecsTaskDetails_version = Lens.lens (\EcsTaskDetails' {version} -> version) (\s@EcsTaskDetails' {} a -> s {version = a} :: EcsTaskDetails)

instance Core.FromJSON EcsTaskDetails where
  parseJSON =
    Core.withObject
      "EcsTaskDetails"
      ( \x ->
          EcsTaskDetails'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "containers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "startedBy")
            Prelude.<*> (x Core..:? "volumes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> (x Core..:? "definitionArn")
            Prelude.<*> (x Core..:? "group")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable EcsTaskDetails where
  hashWithSalt _salt EcsTaskDetails' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` taskCreatedAt
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` definitionArn
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` version

instance Prelude.NFData EcsTaskDetails where
  rnf EcsTaskDetails' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf taskCreatedAt
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf definitionArn
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf version

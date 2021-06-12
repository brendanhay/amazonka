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
-- Module      : Network.AWS.Batch.Types.Volume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Volume where

import Network.AWS.Batch.Types.Host
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A data volume used in a job\'s container properties.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | The name of the volume. Up to 255 letters (uppercase and lowercase),
    -- numbers, hyphens, and underscores are allowed. This name is referenced
    -- in the @sourceVolume@ parameter of container definition @mountPoints@.
    name :: Core.Maybe Core.Text,
    -- | The contents of the @host@ parameter determine whether your data volume
    -- persists on the host container instance and where it is stored. If the
    -- host parameter is empty, then the Docker daemon assigns a host path for
    -- your data volume. However, the data isn\'t guaranteed to persist after
    -- the containers associated with it stop running.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided.
    host :: Core.Maybe Host
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'volume_name' - The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed. This name is referenced
-- in the @sourceVolume@ parameter of container definition @mountPoints@.
--
-- 'host', 'volume_host' - The contents of the @host@ parameter determine whether your data volume
-- persists on the host container instance and where it is stored. If the
-- host parameter is empty, then the Docker daemon assigns a host path for
-- your data volume. However, the data isn\'t guaranteed to persist after
-- the containers associated with it stop running.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
newVolume ::
  Volume
newVolume =
  Volume' {name = Core.Nothing, host = Core.Nothing}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, hyphens, and underscores are allowed. This name is referenced
-- in the @sourceVolume@ parameter of container definition @mountPoints@.
volume_name :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | The contents of the @host@ parameter determine whether your data volume
-- persists on the host container instance and where it is stored. If the
-- host parameter is empty, then the Docker daemon assigns a host path for
-- your data volume. However, the data isn\'t guaranteed to persist after
-- the containers associated with it stop running.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided.
volume_host :: Lens.Lens' Volume (Core.Maybe Host)
volume_host = Lens.lens (\Volume' {host} -> host) (\s@Volume' {} a -> s {host = a} :: Volume)

instance Core.FromJSON Volume where
  parseJSON =
    Core.withObject
      "Volume"
      ( \x ->
          Volume'
            Core.<$> (x Core..:? "name") Core.<*> (x Core..:? "host")
      )

instance Core.Hashable Volume

instance Core.NFData Volume

instance Core.ToJSON Volume where
  toJSON Volume' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("host" Core..=) Core.<$> host
          ]
      )

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
-- Module      : Amazonka.ImageBuilder.Types.FastLaunchConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.FastLaunchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types.FastLaunchLaunchTemplateSpecification
import Amazonka.ImageBuilder.Types.FastLaunchSnapshotConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Define and configure faster launching for output Windows AMIs.
--
-- /See:/ 'newFastLaunchConfiguration' smart constructor.
data FastLaunchConfiguration = FastLaunchConfiguration'
  { -- | The launch template that the fast-launch enabled Windows AMI uses when
    -- it launches Windows instances to create pre-provisioned snapshots.
    launchTemplate :: Prelude.Maybe FastLaunchLaunchTemplateSpecification,
    -- | Configuration settings for managing the number of snapshots that are
    -- created from pre-provisioned instances for the Windows AMI when faster
    -- launching is enabled.
    snapshotConfiguration :: Prelude.Maybe FastLaunchSnapshotConfiguration,
    -- | The owner account ID for the fast-launch enabled Windows AMI.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of parallel instances that are launched for creating
    -- resources.
    maxParallelLaunches :: Prelude.Maybe Prelude.Natural,
    -- | A Boolean that represents the current state of faster launching for the
    -- Windows AMI. Set to @true@ to start using Windows faster launching, or
    -- @false@ to stop using it.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'fastLaunchConfiguration_launchTemplate' - The launch template that the fast-launch enabled Windows AMI uses when
-- it launches Windows instances to create pre-provisioned snapshots.
--
-- 'snapshotConfiguration', 'fastLaunchConfiguration_snapshotConfiguration' - Configuration settings for managing the number of snapshots that are
-- created from pre-provisioned instances for the Windows AMI when faster
-- launching is enabled.
--
-- 'accountId', 'fastLaunchConfiguration_accountId' - The owner account ID for the fast-launch enabled Windows AMI.
--
-- 'maxParallelLaunches', 'fastLaunchConfiguration_maxParallelLaunches' - The maximum number of parallel instances that are launched for creating
-- resources.
--
-- 'enabled', 'fastLaunchConfiguration_enabled' - A Boolean that represents the current state of faster launching for the
-- Windows AMI. Set to @true@ to start using Windows faster launching, or
-- @false@ to stop using it.
newFastLaunchConfiguration ::
  -- | 'enabled'
  Prelude.Bool ->
  FastLaunchConfiguration
newFastLaunchConfiguration pEnabled_ =
  FastLaunchConfiguration'
    { launchTemplate =
        Prelude.Nothing,
      snapshotConfiguration = Prelude.Nothing,
      accountId = Prelude.Nothing,
      maxParallelLaunches = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The launch template that the fast-launch enabled Windows AMI uses when
-- it launches Windows instances to create pre-provisioned snapshots.
fastLaunchConfiguration_launchTemplate :: Lens.Lens' FastLaunchConfiguration (Prelude.Maybe FastLaunchLaunchTemplateSpecification)
fastLaunchConfiguration_launchTemplate = Lens.lens (\FastLaunchConfiguration' {launchTemplate} -> launchTemplate) (\s@FastLaunchConfiguration' {} a -> s {launchTemplate = a} :: FastLaunchConfiguration)

-- | Configuration settings for managing the number of snapshots that are
-- created from pre-provisioned instances for the Windows AMI when faster
-- launching is enabled.
fastLaunchConfiguration_snapshotConfiguration :: Lens.Lens' FastLaunchConfiguration (Prelude.Maybe FastLaunchSnapshotConfiguration)
fastLaunchConfiguration_snapshotConfiguration = Lens.lens (\FastLaunchConfiguration' {snapshotConfiguration} -> snapshotConfiguration) (\s@FastLaunchConfiguration' {} a -> s {snapshotConfiguration = a} :: FastLaunchConfiguration)

-- | The owner account ID for the fast-launch enabled Windows AMI.
fastLaunchConfiguration_accountId :: Lens.Lens' FastLaunchConfiguration (Prelude.Maybe Prelude.Text)
fastLaunchConfiguration_accountId = Lens.lens (\FastLaunchConfiguration' {accountId} -> accountId) (\s@FastLaunchConfiguration' {} a -> s {accountId = a} :: FastLaunchConfiguration)

-- | The maximum number of parallel instances that are launched for creating
-- resources.
fastLaunchConfiguration_maxParallelLaunches :: Lens.Lens' FastLaunchConfiguration (Prelude.Maybe Prelude.Natural)
fastLaunchConfiguration_maxParallelLaunches = Lens.lens (\FastLaunchConfiguration' {maxParallelLaunches} -> maxParallelLaunches) (\s@FastLaunchConfiguration' {} a -> s {maxParallelLaunches = a} :: FastLaunchConfiguration)

-- | A Boolean that represents the current state of faster launching for the
-- Windows AMI. Set to @true@ to start using Windows faster launching, or
-- @false@ to stop using it.
fastLaunchConfiguration_enabled :: Lens.Lens' FastLaunchConfiguration Prelude.Bool
fastLaunchConfiguration_enabled = Lens.lens (\FastLaunchConfiguration' {enabled} -> enabled) (\s@FastLaunchConfiguration' {} a -> s {enabled = a} :: FastLaunchConfiguration)

instance Core.FromJSON FastLaunchConfiguration where
  parseJSON =
    Core.withObject
      "FastLaunchConfiguration"
      ( \x ->
          FastLaunchConfiguration'
            Prelude.<$> (x Core..:? "launchTemplate")
            Prelude.<*> (x Core..:? "snapshotConfiguration")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "maxParallelLaunches")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable FastLaunchConfiguration where
  hashWithSalt _salt FastLaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` snapshotConfiguration
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` maxParallelLaunches
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData FastLaunchConfiguration where
  rnf FastLaunchConfiguration' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf snapshotConfiguration
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf maxParallelLaunches
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON FastLaunchConfiguration where
  toJSON FastLaunchConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("launchTemplate" Core..=)
              Prelude.<$> launchTemplate,
            ("snapshotConfiguration" Core..=)
              Prelude.<$> snapshotConfiguration,
            ("accountId" Core..=) Prelude.<$> accountId,
            ("maxParallelLaunches" Core..=)
              Prelude.<$> maxParallelLaunches,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )

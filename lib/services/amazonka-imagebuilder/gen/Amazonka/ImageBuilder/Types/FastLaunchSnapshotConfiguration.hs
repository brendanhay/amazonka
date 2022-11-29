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
-- Module      : Amazonka.ImageBuilder.Types.FastLaunchSnapshotConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.FastLaunchSnapshotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for creating and managing pre-provisioned
-- snapshots for a fast-launch enabled Windows AMI.
--
-- /See:/ 'newFastLaunchSnapshotConfiguration' smart constructor.
data FastLaunchSnapshotConfiguration = FastLaunchSnapshotConfiguration'
  { -- | The number of pre-provisioned snapshots to keep on hand for a
    -- fast-launch enabled Windows AMI.
    targetResourceCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetResourceCount', 'fastLaunchSnapshotConfiguration_targetResourceCount' - The number of pre-provisioned snapshots to keep on hand for a
-- fast-launch enabled Windows AMI.
newFastLaunchSnapshotConfiguration ::
  FastLaunchSnapshotConfiguration
newFastLaunchSnapshotConfiguration =
  FastLaunchSnapshotConfiguration'
    { targetResourceCount =
        Prelude.Nothing
    }

-- | The number of pre-provisioned snapshots to keep on hand for a
-- fast-launch enabled Windows AMI.
fastLaunchSnapshotConfiguration_targetResourceCount :: Lens.Lens' FastLaunchSnapshotConfiguration (Prelude.Maybe Prelude.Natural)
fastLaunchSnapshotConfiguration_targetResourceCount = Lens.lens (\FastLaunchSnapshotConfiguration' {targetResourceCount} -> targetResourceCount) (\s@FastLaunchSnapshotConfiguration' {} a -> s {targetResourceCount = a} :: FastLaunchSnapshotConfiguration)

instance
  Core.FromJSON
    FastLaunchSnapshotConfiguration
  where
  parseJSON =
    Core.withObject
      "FastLaunchSnapshotConfiguration"
      ( \x ->
          FastLaunchSnapshotConfiguration'
            Prelude.<$> (x Core..:? "targetResourceCount")
      )

instance
  Prelude.Hashable
    FastLaunchSnapshotConfiguration
  where
  hashWithSalt
    _salt
    FastLaunchSnapshotConfiguration' {..} =
      _salt `Prelude.hashWithSalt` targetResourceCount

instance
  Prelude.NFData
    FastLaunchSnapshotConfiguration
  where
  rnf FastLaunchSnapshotConfiguration' {..} =
    Prelude.rnf targetResourceCount

instance Core.ToJSON FastLaunchSnapshotConfiguration where
  toJSON FastLaunchSnapshotConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetResourceCount" Core..=)
              Prelude.<$> targetResourceCount
          ]
      )

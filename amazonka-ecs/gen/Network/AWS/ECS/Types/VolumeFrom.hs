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
-- Module      : Network.AWS.ECS.Types.VolumeFrom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.VolumeFrom where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details on a data volume from another container in the same task
-- definition.
--
-- /See:/ 'newVolumeFrom' smart constructor.
data VolumeFrom = VolumeFrom'
  { -- | If this value is @true@, the container has read-only access to the
    -- volume. If this value is @false@, then the container can write to the
    -- volume. The default value is @false@.
    readOnly :: Core.Maybe Core.Bool,
    -- | The name of another container within the same task definition from which
    -- to mount volumes.
    sourceContainer :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeFrom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOnly', 'volumeFrom_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
--
-- 'sourceContainer', 'volumeFrom_sourceContainer' - The name of another container within the same task definition from which
-- to mount volumes.
newVolumeFrom ::
  VolumeFrom
newVolumeFrom =
  VolumeFrom'
    { readOnly = Core.Nothing,
      sourceContainer = Core.Nothing
    }

-- | If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
volumeFrom_readOnly :: Lens.Lens' VolumeFrom (Core.Maybe Core.Bool)
volumeFrom_readOnly = Lens.lens (\VolumeFrom' {readOnly} -> readOnly) (\s@VolumeFrom' {} a -> s {readOnly = a} :: VolumeFrom)

-- | The name of another container within the same task definition from which
-- to mount volumes.
volumeFrom_sourceContainer :: Lens.Lens' VolumeFrom (Core.Maybe Core.Text)
volumeFrom_sourceContainer = Lens.lens (\VolumeFrom' {sourceContainer} -> sourceContainer) (\s@VolumeFrom' {} a -> s {sourceContainer = a} :: VolumeFrom)

instance Core.FromJSON VolumeFrom where
  parseJSON =
    Core.withObject
      "VolumeFrom"
      ( \x ->
          VolumeFrom'
            Core.<$> (x Core..:? "readOnly")
            Core.<*> (x Core..:? "sourceContainer")
      )

instance Core.Hashable VolumeFrom

instance Core.NFData VolumeFrom

instance Core.ToJSON VolumeFrom where
  toJSON VolumeFrom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("readOnly" Core..=) Core.<$> readOnly,
            ("sourceContainer" Core..=)
              Core.<$> sourceContainer
          ]
      )

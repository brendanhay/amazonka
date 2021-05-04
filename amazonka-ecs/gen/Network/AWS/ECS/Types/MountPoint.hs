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
-- Module      : Network.AWS.ECS.Types.MountPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.MountPoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on a volume mount point that is used in a container definition.
--
-- /See:/ 'newMountPoint' smart constructor.
data MountPoint = MountPoint'
  { -- | If this value is @true@, the container has read-only access to the
    -- volume. If this value is @false@, then the container can write to the
    -- volume. The default value is @false@.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the volume to mount. Must be a volume name referenced in the
    -- @name@ parameter of task definition @volume@.
    sourceVolume :: Prelude.Maybe Prelude.Text,
    -- | The path on the container to mount the host volume at.
    containerPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MountPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOnly', 'mountPoint_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
--
-- 'sourceVolume', 'mountPoint_sourceVolume' - The name of the volume to mount. Must be a volume name referenced in the
-- @name@ parameter of task definition @volume@.
--
-- 'containerPath', 'mountPoint_containerPath' - The path on the container to mount the host volume at.
newMountPoint ::
  MountPoint
newMountPoint =
  MountPoint'
    { readOnly = Prelude.Nothing,
      sourceVolume = Prelude.Nothing,
      containerPath = Prelude.Nothing
    }

-- | If this value is @true@, the container has read-only access to the
-- volume. If this value is @false@, then the container can write to the
-- volume. The default value is @false@.
mountPoint_readOnly :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Bool)
mountPoint_readOnly = Lens.lens (\MountPoint' {readOnly} -> readOnly) (\s@MountPoint' {} a -> s {readOnly = a} :: MountPoint)

-- | The name of the volume to mount. Must be a volume name referenced in the
-- @name@ parameter of task definition @volume@.
mountPoint_sourceVolume :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_sourceVolume = Lens.lens (\MountPoint' {sourceVolume} -> sourceVolume) (\s@MountPoint' {} a -> s {sourceVolume = a} :: MountPoint)

-- | The path on the container to mount the host volume at.
mountPoint_containerPath :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_containerPath = Lens.lens (\MountPoint' {containerPath} -> containerPath) (\s@MountPoint' {} a -> s {containerPath = a} :: MountPoint)

instance Prelude.FromJSON MountPoint where
  parseJSON =
    Prelude.withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            Prelude.<$> (x Prelude..:? "readOnly")
            Prelude.<*> (x Prelude..:? "sourceVolume")
            Prelude.<*> (x Prelude..:? "containerPath")
      )

instance Prelude.Hashable MountPoint

instance Prelude.NFData MountPoint

instance Prelude.ToJSON MountPoint where
  toJSON MountPoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("readOnly" Prelude..=) Prelude.<$> readOnly,
            ("sourceVolume" Prelude..=) Prelude.<$> sourceVolume,
            ("containerPath" Prelude..=)
              Prelude.<$> containerPath
          ]
      )

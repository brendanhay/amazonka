{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Tmpfs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Tmpfs
  ( Tmpfs (..),

    -- * Smart constructor
    mkTmpfs,

    -- * Lenses
    tContainerPath,
    tSize,
    tMountOptions,
  )
where

import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The container path, mount options, and size of the tmpfs mount.
--
-- /See:/ 'mkTmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { -- | The absolute file path in the container where the tmpfs volume is to be mounted.
    containerPath :: Types.String,
    -- | The size (in MiB) of the tmpfs volume.
    size :: Core.Int,
    -- | The list of tmpfs volume mount options.
    --
    -- Valid values: "@defaults@ " | "@ro@ " | "@rw@ " | "@suid@ " | "@nosuid@ " | "@dev@ " | "@nodev@ " | "@exec@ " | "@noexec@ " | "@sync@ " | "@async@ " | "@dirsync@ " | "@remount@ " | "@mand@ " | "@nomand@ " | "@atime@ " | "@noatime@ " | "@diratime@ " | "@nodiratime@ " | "@bind@ " | "@rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime@ " | "@norelatime@ " | "@strictatime@ " | "@nostrictatime@ " | "@mode@ " | "@uid@ " | "@gid@ " | "@nr_inodes@ " | "@nr_blocks@ " | "@mpol@ "
    mountOptions :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tmpfs' value with any optional fields omitted.
mkTmpfs ::
  -- | 'containerPath'
  Types.String ->
  -- | 'size'
  Core.Int ->
  Tmpfs
mkTmpfs containerPath size =
  Tmpfs' {containerPath, size, mountOptions = Core.Nothing}

-- | The absolute file path in the container where the tmpfs volume is to be mounted.
--
-- /Note:/ Consider using 'containerPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainerPath :: Lens.Lens' Tmpfs Types.String
tContainerPath = Lens.field @"containerPath"
{-# DEPRECATED tContainerPath "Use generic-lens or generic-optics with 'containerPath' instead." #-}

-- | The size (in MiB) of the tmpfs volume.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSize :: Lens.Lens' Tmpfs Core.Int
tSize = Lens.field @"size"
{-# DEPRECATED tSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The list of tmpfs volume mount options.
--
-- Valid values: "@defaults@ " | "@ro@ " | "@rw@ " | "@suid@ " | "@nosuid@ " | "@dev@ " | "@nodev@ " | "@exec@ " | "@noexec@ " | "@sync@ " | "@async@ " | "@dirsync@ " | "@remount@ " | "@mand@ " | "@nomand@ " | "@atime@ " | "@noatime@ " | "@diratime@ " | "@nodiratime@ " | "@bind@ " | "@rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime@ " | "@norelatime@ " | "@strictatime@ " | "@nostrictatime@ " | "@mode@ " | "@uid@ " | "@gid@ " | "@nr_inodes@ " | "@nr_blocks@ " | "@mpol@ "
--
-- /Note:/ Consider using 'mountOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMountOptions :: Lens.Lens' Tmpfs (Core.Maybe [Types.String])
tMountOptions = Lens.field @"mountOptions"
{-# DEPRECATED tMountOptions "Use generic-lens or generic-optics with 'mountOptions' instead." #-}

instance Core.FromJSON Tmpfs where
  toJSON Tmpfs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("containerPath" Core..= containerPath),
            Core.Just ("size" Core..= size),
            ("mountOptions" Core..=) Core.<$> mountOptions
          ]
      )

instance Core.FromJSON Tmpfs where
  parseJSON =
    Core.withObject "Tmpfs" Core.$
      \x ->
        Tmpfs'
          Core.<$> (x Core..: "containerPath")
          Core.<*> (x Core..: "size")
          Core.<*> (x Core..:? "mountOptions")

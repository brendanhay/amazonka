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
    tSize,
    tContainerPath,
    tMountOptions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The container path, mount options, and size of the tmpfs mount.
--
-- /See:/ 'mkTmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { -- | The size (in MiB) of the tmpfs volume.
    size :: Lude.Int,
    -- | The absolute file path in the container where the tmpfs volume is to be mounted.
    containerPath :: Lude.Text,
    -- | The list of tmpfs volume mount options.
    --
    -- Valid values: "@defaults@ " | "@ro@ " | "@rw@ " | "@suid@ " | "@nosuid@ " | "@dev@ " | "@nodev@ " | "@exec@ " | "@noexec@ " | "@sync@ " | "@async@ " | "@dirsync@ " | "@remount@ " | "@mand@ " | "@nomand@ " | "@atime@ " | "@noatime@ " | "@diratime@ " | "@nodiratime@ " | "@bind@ " | "@rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime@ " | "@norelatime@ " | "@strictatime@ " | "@nostrictatime@ " | "@mode@ " | "@uid@ " | "@gid@ " | "@nr_inodes@ " | "@nr_blocks@ " | "@mpol@ "
    mountOptions :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tmpfs' with the minimum fields required to make a request.
--
-- * 'size' - The size (in MiB) of the tmpfs volume.
-- * 'containerPath' - The absolute file path in the container where the tmpfs volume is to be mounted.
-- * 'mountOptions' - The list of tmpfs volume mount options.
--
-- Valid values: "@defaults@ " | "@ro@ " | "@rw@ " | "@suid@ " | "@nosuid@ " | "@dev@ " | "@nodev@ " | "@exec@ " | "@noexec@ " | "@sync@ " | "@async@ " | "@dirsync@ " | "@remount@ " | "@mand@ " | "@nomand@ " | "@atime@ " | "@noatime@ " | "@diratime@ " | "@nodiratime@ " | "@bind@ " | "@rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime@ " | "@norelatime@ " | "@strictatime@ " | "@nostrictatime@ " | "@mode@ " | "@uid@ " | "@gid@ " | "@nr_inodes@ " | "@nr_blocks@ " | "@mpol@ "
mkTmpfs ::
  -- | 'size'
  Lude.Int ->
  -- | 'containerPath'
  Lude.Text ->
  Tmpfs
mkTmpfs pSize_ pContainerPath_ =
  Tmpfs'
    { size = pSize_,
      containerPath = pContainerPath_,
      mountOptions = Lude.Nothing
    }

-- | The size (in MiB) of the tmpfs volume.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSize :: Lens.Lens' Tmpfs Lude.Int
tSize = Lens.lens (size :: Tmpfs -> Lude.Int) (\s a -> s {size = a} :: Tmpfs)
{-# DEPRECATED tSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The absolute file path in the container where the tmpfs volume is to be mounted.
--
-- /Note:/ Consider using 'containerPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tContainerPath :: Lens.Lens' Tmpfs Lude.Text
tContainerPath = Lens.lens (containerPath :: Tmpfs -> Lude.Text) (\s a -> s {containerPath = a} :: Tmpfs)
{-# DEPRECATED tContainerPath "Use generic-lens or generic-optics with 'containerPath' instead." #-}

-- | The list of tmpfs volume mount options.
--
-- Valid values: "@defaults@ " | "@ro@ " | "@rw@ " | "@suid@ " | "@nosuid@ " | "@dev@ " | "@nodev@ " | "@exec@ " | "@noexec@ " | "@sync@ " | "@async@ " | "@dirsync@ " | "@remount@ " | "@mand@ " | "@nomand@ " | "@atime@ " | "@noatime@ " | "@diratime@ " | "@nodiratime@ " | "@bind@ " | "@rbind" | "unbindable" | "runbindable" | "private" | "rprivate" | "shared" | "rshared" | "slave" | "rslave" | "relatime@ " | "@norelatime@ " | "@strictatime@ " | "@nostrictatime@ " | "@mode@ " | "@uid@ " | "@gid@ " | "@nr_inodes@ " | "@nr_blocks@ " | "@mpol@ "
--
-- /Note:/ Consider using 'mountOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMountOptions :: Lens.Lens' Tmpfs (Lude.Maybe [Lude.Text])
tMountOptions = Lens.lens (mountOptions :: Tmpfs -> Lude.Maybe [Lude.Text]) (\s a -> s {mountOptions = a} :: Tmpfs)
{-# DEPRECATED tMountOptions "Use generic-lens or generic-optics with 'mountOptions' instead." #-}

instance Lude.FromJSON Tmpfs where
  parseJSON =
    Lude.withObject
      "Tmpfs"
      ( \x ->
          Tmpfs'
            Lude.<$> (x Lude..: "size")
            Lude.<*> (x Lude..: "containerPath")
            Lude.<*> (x Lude..:? "mountOptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Tmpfs where
  toJSON Tmpfs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("size" Lude..= size),
            Lude.Just ("containerPath" Lude..= containerPath),
            ("mountOptions" Lude..=) Lude.<$> mountOptions
          ]
      )

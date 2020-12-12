{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AttachedDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AttachedDisk
  ( AttachedDisk (..),

    -- * Smart constructor
    mkAttachedDisk,

    -- * Lenses
    adPath,
    adSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block storage disk that is attached to an instance, and is included in an automatic snapshot.
--
-- /See:/ 'mkAttachedDisk' smart constructor.
data AttachedDisk = AttachedDisk'
  { path :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachedDisk' with the minimum fields required to make a request.
--
-- * 'path' - The path of the disk (e.g., @/dev/xvdf@ ).
-- * 'sizeInGb' - The size of the disk in GB.
mkAttachedDisk ::
  AttachedDisk
mkAttachedDisk =
  AttachedDisk' {path = Lude.Nothing, sizeInGb = Lude.Nothing}

-- | The path of the disk (e.g., @/dev/xvdf@ ).
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adPath :: Lens.Lens' AttachedDisk (Lude.Maybe Lude.Text)
adPath = Lens.lens (path :: AttachedDisk -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: AttachedDisk)
{-# DEPRECATED adPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSizeInGb :: Lens.Lens' AttachedDisk (Lude.Maybe Lude.Int)
adSizeInGb = Lens.lens (sizeInGb :: AttachedDisk -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: AttachedDisk)
{-# DEPRECATED adSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Lude.FromJSON AttachedDisk where
  parseJSON =
    Lude.withObject
      "AttachedDisk"
      ( \x ->
          AttachedDisk'
            Lude.<$> (x Lude..:? "path") Lude.<*> (x Lude..:? "sizeInGb")
      )

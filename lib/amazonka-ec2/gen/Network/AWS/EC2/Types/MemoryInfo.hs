{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MemoryInfo
  ( MemoryInfo (..),

    -- * Smart constructor
    mkMemoryInfo,

    -- * Lenses
    miSizeInMiB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the memory for the instance type.
--
-- /See:/ 'mkMemoryInfo' smart constructor.
newtype MemoryInfo = MemoryInfo'
  { -- | The size of the memory, in MiB.
    sizeInMiB :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MemoryInfo' with the minimum fields required to make a request.
--
-- * 'sizeInMiB' - The size of the memory, in MiB.
mkMemoryInfo ::
  MemoryInfo
mkMemoryInfo = MemoryInfo' {sizeInMiB = Lude.Nothing}

-- | The size of the memory, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miSizeInMiB :: Lens.Lens' MemoryInfo (Lude.Maybe Lude.Integer)
miSizeInMiB = Lens.lens (sizeInMiB :: MemoryInfo -> Lude.Maybe Lude.Integer) (\s a -> s {sizeInMiB = a} :: MemoryInfo)
{-# DEPRECATED miSizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead." #-}

instance Lude.FromXML MemoryInfo where
  parseXML x = MemoryInfo' Lude.<$> (x Lude..@? "sizeInMiB")

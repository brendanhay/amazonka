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
import qualified Network.AWS.Prelude as Core

-- | Describes the memory for the instance type.
--
-- /See:/ 'mkMemoryInfo' smart constructor.
newtype MemoryInfo = MemoryInfo'
  { -- | The size of the memory, in MiB.
    sizeInMiB :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MemoryInfo' value with any optional fields omitted.
mkMemoryInfo ::
  MemoryInfo
mkMemoryInfo = MemoryInfo' {sizeInMiB = Core.Nothing}

-- | The size of the memory, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miSizeInMiB :: Lens.Lens' MemoryInfo (Core.Maybe Core.Integer)
miSizeInMiB = Lens.field @"sizeInMiB"
{-# DEPRECATED miSizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead." #-}

instance Core.FromXML MemoryInfo where
  parseXML x = MemoryInfo' Core.<$> (x Core..@? "sizeInMiB")

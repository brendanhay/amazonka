{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayProperties
  ( ArrayProperties (..),

    -- * Smart constructor
    mkArrayProperties,

    -- * Lenses
    apSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an AWS Batch array job.
--
-- /See:/ 'mkArrayProperties' smart constructor.
newtype ArrayProperties = ArrayProperties'
  { -- | The size of the array job.
    size :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ArrayProperties' value with any optional fields omitted.
mkArrayProperties ::
  ArrayProperties
mkArrayProperties = ArrayProperties' {size = Core.Nothing}

-- | The size of the array job.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSize :: Lens.Lens' ArrayProperties (Core.Maybe Core.Int)
apSize = Lens.field @"size"
{-# DEPRECATED apSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromJSON ArrayProperties where
  toJSON ArrayProperties {..} =
    Core.object (Core.catMaybes [("size" Core..=) Core.<$> size])

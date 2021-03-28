{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
  ( EstimatedResourceSize (..)
  -- * Smart constructor
  , mkEstimatedResourceSize
  -- * Lenses
  , ersEstimatedOn
  , ersEstimatedSizeInBytes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The estimated size of the resource.
--
-- /See:/ 'mkEstimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { estimatedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the estimate of the size of the resource was made.
  , estimatedSizeInBytes :: Core.Maybe Core.Double
    -- ^ The estimated size of the resource, in bytes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EstimatedResourceSize' value with any optional fields omitted.
mkEstimatedResourceSize
    :: EstimatedResourceSize
mkEstimatedResourceSize
  = EstimatedResourceSize'{estimatedOn = Core.Nothing,
                           estimatedSizeInBytes = Core.Nothing}

-- | The time when the estimate of the size of the resource was made.
--
-- /Note:/ Consider using 'estimatedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersEstimatedOn :: Lens.Lens' EstimatedResourceSize (Core.Maybe Core.NominalDiffTime)
ersEstimatedOn = Lens.field @"estimatedOn"
{-# INLINEABLE ersEstimatedOn #-}
{-# DEPRECATED estimatedOn "Use generic-lens or generic-optics with 'estimatedOn' instead"  #-}

-- | The estimated size of the resource, in bytes.
--
-- /Note:/ Consider using 'estimatedSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersEstimatedSizeInBytes :: Lens.Lens' EstimatedResourceSize (Core.Maybe Core.Double)
ersEstimatedSizeInBytes = Lens.field @"estimatedSizeInBytes"
{-# INLINEABLE ersEstimatedSizeInBytes #-}
{-# DEPRECATED estimatedSizeInBytes "Use generic-lens or generic-optics with 'estimatedSizeInBytes' instead"  #-}

instance Core.FromJSON EstimatedResourceSize where
        parseJSON
          = Core.withObject "EstimatedResourceSize" Core.$
              \ x ->
                EstimatedResourceSize' Core.<$>
                  (x Core..:? "estimatedOn") Core.<*>
                    x Core..:? "estimatedSizeInBytes"

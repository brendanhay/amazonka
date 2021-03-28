{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.TrailInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.TrailInfo
  ( TrailInfo (..)
  -- * Smart constructor
  , mkTrailInfo
  -- * Lenses
  , tiHomeRegion
  , tiName
  , tiTrailARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a CloudTrail trail, including the trail's name, home region, and Amazon Resource Name (ARN).
--
-- /See:/ 'mkTrailInfo' smart constructor.
data TrailInfo = TrailInfo'
  { homeRegion :: Core.Maybe Core.Text
    -- ^ The AWS region in which a trail was created.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a trail.
  , trailARN :: Core.Maybe Core.Text
    -- ^ The ARN of a trail.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrailInfo' value with any optional fields omitted.
mkTrailInfo
    :: TrailInfo
mkTrailInfo
  = TrailInfo'{homeRegion = Core.Nothing, name = Core.Nothing,
               trailARN = Core.Nothing}

-- | The AWS region in which a trail was created.
--
-- /Note:/ Consider using 'homeRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiHomeRegion :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
tiHomeRegion = Lens.field @"homeRegion"
{-# INLINEABLE tiHomeRegion #-}
{-# DEPRECATED homeRegion "Use generic-lens or generic-optics with 'homeRegion' instead"  #-}

-- | The name of a trail.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiName :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
tiName = Lens.field @"name"
{-# INLINEABLE tiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ARN of a trail.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTrailARN :: Lens.Lens' TrailInfo (Core.Maybe Core.Text)
tiTrailARN = Lens.field @"trailARN"
{-# INLINEABLE tiTrailARN #-}
{-# DEPRECATED trailARN "Use generic-lens or generic-optics with 'trailARN' instead"  #-}

instance Core.FromJSON TrailInfo where
        parseJSON
          = Core.withObject "TrailInfo" Core.$
              \ x ->
                TrailInfo' Core.<$>
                  (x Core..:? "HomeRegion") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "TrailARN"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DefaultRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DefaultRetention
  ( DefaultRetention (..),

    -- * Smart constructor
    mkDefaultRetention,

    -- * Lenses
    drDays,
    drMode,
    drYears,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectLockRetentionMode as Types

-- | The container element for specifying the default Object Lock retention settings for new objects placed in the specified bucket.
--
-- /See:/ 'mkDefaultRetention' smart constructor.
data DefaultRetention = DefaultRetention'
  { -- | The number of days that you want to specify for the default retention period.
    days :: Core.Maybe Core.Int,
    -- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
    mode :: Core.Maybe Types.ObjectLockRetentionMode,
    -- | The number of years that you want to specify for the default retention period.
    years :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultRetention' value with any optional fields omitted.
mkDefaultRetention ::
  DefaultRetention
mkDefaultRetention =
  DefaultRetention'
    { days = Core.Nothing,
      mode = Core.Nothing,
      years = Core.Nothing
    }

-- | The number of days that you want to specify for the default retention period.
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDays :: Lens.Lens' DefaultRetention (Core.Maybe Core.Int)
drDays = Lens.field @"days"
{-# DEPRECATED drDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | The default Object Lock retention mode you want to apply to new objects placed in the specified bucket.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMode :: Lens.Lens' DefaultRetention (Core.Maybe Types.ObjectLockRetentionMode)
drMode = Lens.field @"mode"
{-# DEPRECATED drMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The number of years that you want to specify for the default retention period.
--
-- /Note:/ Consider using 'years' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drYears :: Lens.Lens' DefaultRetention (Core.Maybe Core.Int)
drYears = Lens.field @"years"
{-# DEPRECATED drYears "Use generic-lens or generic-optics with 'years' instead." #-}

instance Core.ToXML DefaultRetention where
  toXML DefaultRetention {..} =
    Core.toXMLNode "Days" Core.<$> days
      Core.<> Core.toXMLNode "Mode" Core.<$> mode
      Core.<> Core.toXMLNode "Years" Core.<$> years

instance Core.FromXML DefaultRetention where
  parseXML x =
    DefaultRetention'
      Core.<$> (x Core..@? "Days")
      Core.<*> (x Core..@? "Mode")
      Core.<*> (x Core..@? "Years")

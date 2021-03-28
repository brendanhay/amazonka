{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.NotificationProperty
  ( NotificationProperty (..)
  -- * Smart constructor
  , mkNotificationProperty
  -- * Lenses
  , npNotifyDelayAfter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies configuration properties of a notification.
--
-- /See:/ 'mkNotificationProperty' smart constructor.
newtype NotificationProperty = NotificationProperty'
  { notifyDelayAfter :: Core.Maybe Core.Natural
    -- ^ After a job run starts, the number of minutes to wait before sending a job run delay notification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationProperty' value with any optional fields omitted.
mkNotificationProperty
    :: NotificationProperty
mkNotificationProperty
  = NotificationProperty'{notifyDelayAfter = Core.Nothing}

-- | After a job run starts, the number of minutes to wait before sending a job run delay notification.
--
-- /Note:/ Consider using 'notifyDelayAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNotifyDelayAfter :: Lens.Lens' NotificationProperty (Core.Maybe Core.Natural)
npNotifyDelayAfter = Lens.field @"notifyDelayAfter"
{-# INLINEABLE npNotifyDelayAfter #-}
{-# DEPRECATED notifyDelayAfter "Use generic-lens or generic-optics with 'notifyDelayAfter' instead"  #-}

instance Core.FromJSON NotificationProperty where
        toJSON NotificationProperty{..}
          = Core.object
              (Core.catMaybes
                 [("NotifyDelayAfter" Core..=) Core.<$> notifyDelayAfter])

instance Core.FromJSON NotificationProperty where
        parseJSON
          = Core.withObject "NotificationProperty" Core.$
              \ x ->
                NotificationProperty' Core.<$> (x Core..:? "NotifyDelayAfter")

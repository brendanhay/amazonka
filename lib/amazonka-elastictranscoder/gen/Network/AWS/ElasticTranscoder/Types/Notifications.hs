{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Notifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Notifications
  ( Notifications (..)
  -- * Smart constructor
  , mkNotifications
  -- * Lenses
  , nCompleted
  , nError
  , nProgressing
  , nWarning
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.SnsTopic as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
-- /See:/ 'mkNotifications' smart constructor.
data Notifications = Notifications'
  { completed :: Core.Maybe Types.SnsTopic
    -- ^ The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
  , error :: Core.Maybe Types.SnsTopic
    -- ^ The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
  , progressing :: Core.Maybe Types.SnsTopic
    -- ^ The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
  , warning :: Core.Maybe Types.SnsTopic
    -- ^ The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Notifications' value with any optional fields omitted.
mkNotifications
    :: Notifications
mkNotifications
  = Notifications'{completed = Core.Nothing, error = Core.Nothing,
                   progressing = Core.Nothing, warning = Core.Nothing}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCompleted :: Lens.Lens' Notifications (Core.Maybe Types.SnsTopic)
nCompleted = Lens.field @"completed"
{-# INLINEABLE nCompleted #-}
{-# DEPRECATED completed "Use generic-lens or generic-optics with 'completed' instead"  #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nError :: Lens.Lens' Notifications (Core.Maybe Types.SnsTopic)
nError = Lens.field @"error"
{-# INLINEABLE nError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
-- /Note:/ Consider using 'progressing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nProgressing :: Lens.Lens' Notifications (Core.Maybe Types.SnsTopic)
nProgressing = Lens.field @"progressing"
{-# INLINEABLE nProgressing #-}
{-# DEPRECATED progressing "Use generic-lens or generic-optics with 'progressing' instead"  #-}

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nWarning :: Lens.Lens' Notifications (Core.Maybe Types.SnsTopic)
nWarning = Lens.field @"warning"
{-# INLINEABLE nWarning #-}
{-# DEPRECATED warning "Use generic-lens or generic-optics with 'warning' instead"  #-}

instance Core.FromJSON Notifications where
        toJSON Notifications{..}
          = Core.object
              (Core.catMaybes
                 [("Completed" Core..=) Core.<$> completed,
                  ("Error" Core..=) Core.<$> error,
                  ("Progressing" Core..=) Core.<$> progressing,
                  ("Warning" Core..=) Core.<$> warning])

instance Core.FromJSON Notifications where
        parseJSON
          = Core.withObject "Notifications" Core.$
              \ x ->
                Notifications' Core.<$>
                  (x Core..:? "Completed") Core.<*> x Core..:? "Error" Core.<*>
                    x Core..:? "Progressing"
                    Core.<*> x Core..:? "Warning"

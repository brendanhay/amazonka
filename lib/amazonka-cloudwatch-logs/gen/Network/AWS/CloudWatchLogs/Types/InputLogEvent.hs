{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.InputLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.InputLogEvent
  ( InputLogEvent (..)
  -- * Smart constructor
  , mkInputLogEvent
  -- * Lenses
  , ileTimestamp
  , ileMessage
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.
--
-- /See:/ 'mkInputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { timestamp :: Core.Natural
    -- ^ The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
  , message :: Types.Message
    -- ^ The raw event message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLogEvent' value with any optional fields omitted.
mkInputLogEvent
    :: Core.Natural -- ^ 'timestamp'
    -> Types.Message -- ^ 'message'
    -> InputLogEvent
mkInputLogEvent timestamp message
  = InputLogEvent'{timestamp, message}

-- | The time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileTimestamp :: Lens.Lens' InputLogEvent Core.Natural
ileTimestamp = Lens.field @"timestamp"
{-# INLINEABLE ileTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The raw event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ileMessage :: Lens.Lens' InputLogEvent Types.Message
ileMessage = Lens.field @"message"
{-# INLINEABLE ileMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON InputLogEvent where
        toJSON InputLogEvent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("timestamp" Core..= timestamp),
                  Core.Just ("message" Core..= message)])

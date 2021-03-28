{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LogEvent
  ( LogEvent (..)
  -- * Smart constructor
  , mkLogEvent
  -- * Lenses
  , leCreatedAt
  , leMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a database log event.
--
-- /See:/ 'mkLogEvent' smart constructor.
data LogEvent = LogEvent'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the database log event was created.
  , message :: Core.Maybe Core.Text
    -- ^ The message of the database log event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LogEvent' value with any optional fields omitted.
mkLogEvent
    :: LogEvent
mkLogEvent
  = LogEvent'{createdAt = Core.Nothing, message = Core.Nothing}

-- | The timestamp when the database log event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedAt :: Lens.Lens' LogEvent (Core.Maybe Core.NominalDiffTime)
leCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE leCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The message of the database log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMessage :: Lens.Lens' LogEvent (Core.Maybe Core.Text)
leMessage = Lens.field @"message"
{-# INLINEABLE leMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON LogEvent where
        parseJSON
          = Core.withObject "LogEvent" Core.$
              \ x ->
                LogEvent' Core.<$>
                  (x Core..:? "createdAt") Core.<*> x Core..:? "message"

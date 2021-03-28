{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceLogEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceLogEvent
  ( ContainerServiceLogEvent (..)
  -- * Smart constructor
  , mkContainerServiceLogEvent
  -- * Lenses
  , csleCreatedAt
  , csleMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the log events of a container of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceLogEvent' smart constructor.
data ContainerServiceLogEvent = ContainerServiceLogEvent'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the container service log event was created.
  , message :: Core.Maybe Core.Text
    -- ^ The message of the container service log event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ContainerServiceLogEvent' value with any optional fields omitted.
mkContainerServiceLogEvent
    :: ContainerServiceLogEvent
mkContainerServiceLogEvent
  = ContainerServiceLogEvent'{createdAt = Core.Nothing,
                              message = Core.Nothing}

-- | The timestamp when the container service log event was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csleCreatedAt :: Lens.Lens' ContainerServiceLogEvent (Core.Maybe Core.NominalDiffTime)
csleCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE csleCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The message of the container service log event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csleMessage :: Lens.Lens' ContainerServiceLogEvent (Core.Maybe Core.Text)
csleMessage = Lens.field @"message"
{-# INLINEABLE csleMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON ContainerServiceLogEvent where
        parseJSON
          = Core.withObject "ContainerServiceLogEvent" Core.$
              \ x ->
                ContainerServiceLogEvent' Core.<$>
                  (x Core..:? "createdAt") Core.<*> x Core..:? "message"

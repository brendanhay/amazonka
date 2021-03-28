{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeStatusAction
  ( VolumeStatusAction (..)
  -- * Smart constructor
  , mkVolumeStatusAction
  -- * Lenses
  , vsaCode
  , vsaDescription
  , vsaEventId
  , vsaEventType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a volume status operation code.
--
-- /See:/ 'mkVolumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { code :: Core.Maybe Core.Text
    -- ^ The code identifying the operation, for example, @enable-volume-io@ .
  , description :: Core.Maybe Core.Text
    -- ^ A description of the operation.
  , eventId :: Core.Maybe Core.Text
    -- ^ The ID of the event associated with this operation.
  , eventType :: Core.Maybe Core.Text
    -- ^ The event type associated with this operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeStatusAction' value with any optional fields omitted.
mkVolumeStatusAction
    :: VolumeStatusAction
mkVolumeStatusAction
  = VolumeStatusAction'{code = Core.Nothing,
                        description = Core.Nothing, eventId = Core.Nothing,
                        eventType = Core.Nothing}

-- | The code identifying the operation, for example, @enable-volume-io@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaCode :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
vsaCode = Lens.field @"code"
{-# INLINEABLE vsaCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A description of the operation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaDescription :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
vsaDescription = Lens.field @"description"
{-# INLINEABLE vsaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the event associated with this operation.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaEventId :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
vsaEventId = Lens.field @"eventId"
{-# INLINEABLE vsaEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | The event type associated with this operation.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaEventType :: Lens.Lens' VolumeStatusAction (Core.Maybe Core.Text)
vsaEventType = Lens.field @"eventType"
{-# INLINEABLE vsaEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

instance Core.FromXML VolumeStatusAction where
        parseXML x
          = VolumeStatusAction' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "description" Core.<*>
                x Core..@? "eventId"
                Core.<*> x Core..@? "eventType"

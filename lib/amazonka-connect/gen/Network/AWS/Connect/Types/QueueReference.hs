{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.QueueReference
  ( QueueReference (..)
  -- * Smart constructor
  , mkQueueReference
  -- * Lenses
  , qrArn
  , qrId
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a queue resource for which metrics are returned.
--
-- /See:/ 'mkQueueReference' smart constructor.
data QueueReference = QueueReference'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the queue.
  , id :: Core.Maybe Types.QueueId
    -- ^ The identifier of the queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueueReference' value with any optional fields omitted.
mkQueueReference
    :: QueueReference
mkQueueReference
  = QueueReference'{arn = Core.Nothing, id = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrArn :: Lens.Lens' QueueReference (Core.Maybe Types.ARN)
qrArn = Lens.field @"arn"
{-# INLINEABLE qrArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrId :: Lens.Lens' QueueReference (Core.Maybe Types.QueueId)
qrId = Lens.field @"id"
{-# INLINEABLE qrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON QueueReference where
        parseJSON
          = Core.withObject "QueueReference" Core.$
              \ x ->
                QueueReference' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id"

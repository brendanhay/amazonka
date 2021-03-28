{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.QueueSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.QueueSummary
  ( QueueSummary (..)
  -- * Smart constructor
  , mkQueueSummary
  -- * Lenses
  , qsArn
  , qsId
  , qsName
  , qsQueueType
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Connect.Types.QueueName as Types
import qualified Network.AWS.Connect.Types.QueueType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a queue.
--
-- /See:/ 'mkQueueSummary' smart constructor.
data QueueSummary = QueueSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the queue.
  , id :: Core.Maybe Types.QueueId
    -- ^ The identifier of the queue.
  , name :: Core.Maybe Types.QueueName
    -- ^ The name of the queue.
  , queueType :: Core.Maybe Types.QueueType
    -- ^ The type of queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueueSummary' value with any optional fields omitted.
mkQueueSummary
    :: QueueSummary
mkQueueSummary
  = QueueSummary'{arn = Core.Nothing, id = Core.Nothing,
                  name = Core.Nothing, queueType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsArn :: Lens.Lens' QueueSummary (Core.Maybe Types.ARN)
qsArn = Lens.field @"arn"
{-# INLINEABLE qsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsId :: Lens.Lens' QueueSummary (Core.Maybe Types.QueueId)
qsId = Lens.field @"id"
{-# INLINEABLE qsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the queue.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsName :: Lens.Lens' QueueSummary (Core.Maybe Types.QueueName)
qsName = Lens.field @"name"
{-# INLINEABLE qsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of queue.
--
-- /Note:/ Consider using 'queueType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsQueueType :: Lens.Lens' QueueSummary (Core.Maybe Types.QueueType)
qsQueueType = Lens.field @"queueType"
{-# INLINEABLE qsQueueType #-}
{-# DEPRECATED queueType "Use generic-lens or generic-optics with 'queueType' instead"  #-}

instance Core.FromJSON QueueSummary where
        parseJSON
          = Core.withObject "QueueSummary" Core.$
              \ x ->
                QueueSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
                    Core.<*> x Core..:? "QueueType"

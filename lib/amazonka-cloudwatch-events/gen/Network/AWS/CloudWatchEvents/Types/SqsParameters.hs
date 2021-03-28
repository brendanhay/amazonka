{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.SqsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.SqsParameters
  ( SqsParameters (..)
  -- * Smart constructor
  , mkSqsParameters
  -- * Lenses
  , spMessageGroupId
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.MessageGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure includes the custom parameter to be used when the target is an SQS FIFO queue.
--
-- /See:/ 'mkSqsParameters' smart constructor.
newtype SqsParameters = SqsParameters'
  { messageGroupId :: Core.Maybe Types.MessageGroupId
    -- ^ The FIFO message group ID to use as the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SqsParameters' value with any optional fields omitted.
mkSqsParameters
    :: SqsParameters
mkSqsParameters = SqsParameters'{messageGroupId = Core.Nothing}

-- | The FIFO message group ID to use as the target.
--
-- /Note:/ Consider using 'messageGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMessageGroupId :: Lens.Lens' SqsParameters (Core.Maybe Types.MessageGroupId)
spMessageGroupId = Lens.field @"messageGroupId"
{-# INLINEABLE spMessageGroupId #-}
{-# DEPRECATED messageGroupId "Use generic-lens or generic-optics with 'messageGroupId' instead"  #-}

instance Core.FromJSON SqsParameters where
        toJSON SqsParameters{..}
          = Core.object
              (Core.catMaybes
                 [("MessageGroupId" Core..=) Core.<$> messageGroupId])

instance Core.FromJSON SqsParameters where
        parseJSON
          = Core.withObject "SqsParameters" Core.$
              \ x -> SqsParameters' Core.<$> (x Core..:? "MessageGroupId")

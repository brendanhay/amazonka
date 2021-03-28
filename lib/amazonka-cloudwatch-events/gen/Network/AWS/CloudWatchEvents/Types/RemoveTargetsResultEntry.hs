{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
  ( RemoveTargetsResultEntry (..)
  -- * Smart constructor
  , mkRemoveTargetsResultEntry
  -- * Lenses
  , rtreErrorCode
  , rtreErrorMessage
  , rtreTargetId
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.ErrorCode as Types
import qualified Network.AWS.CloudWatchEvents.Types.ErrorMessage as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a target that failed to be removed from a rule.
--
-- /See:/ 'mkRemoveTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The error message that explains why the target removal failed.
  , targetId :: Core.Maybe Types.TargetId
    -- ^ The ID of the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTargetsResultEntry' value with any optional fields omitted.
mkRemoveTargetsResultEntry
    :: RemoveTargetsResultEntry
mkRemoveTargetsResultEntry
  = RemoveTargetsResultEntry'{errorCode = Core.Nothing,
                              errorMessage = Core.Nothing, targetId = Core.Nothing}

-- | The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreErrorCode :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Types.ErrorCode)
rtreErrorCode = Lens.field @"errorCode"
{-# INLINEABLE rtreErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message that explains why the target removal failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreErrorMessage :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Types.ErrorMessage)
rtreErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE rtreErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The ID of the target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreTargetId :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Types.TargetId)
rtreTargetId = Lens.field @"targetId"
{-# INLINEABLE rtreTargetId #-}
{-# DEPRECATED targetId "Use generic-lens or generic-optics with 'targetId' instead"  #-}

instance Core.FromJSON RemoveTargetsResultEntry where
        parseJSON
          = Core.withObject "RemoveTargetsResultEntry" Core.$
              \ x ->
                RemoveTargetsResultEntry' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "TargetId"

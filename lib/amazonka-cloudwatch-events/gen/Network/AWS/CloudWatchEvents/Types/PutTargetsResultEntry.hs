{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
  ( PutTargetsResultEntry (..),

    -- * Smart constructor
    mkPutTargetsResultEntry,

    -- * Lenses
    ptreErrorCode,
    ptreErrorMessage,
    ptreTargetId,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.ErrorCode as Types
import qualified Network.AWS.CloudWatchEvents.Types.ErrorMessage as Types
import qualified Network.AWS.CloudWatchEvents.Types.TargetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a target that failed to be added to a rule.
--
-- /See:/ 'mkPutTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
  { -- | The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message that explains why the target addition failed.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The ID of the target.
    targetId :: Core.Maybe Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutTargetsResultEntry' value with any optional fields omitted.
mkPutTargetsResultEntry ::
  PutTargetsResultEntry
mkPutTargetsResultEntry =
  PutTargetsResultEntry'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      targetId = Core.Nothing
    }

-- | The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreErrorCode :: Lens.Lens' PutTargetsResultEntry (Core.Maybe Types.ErrorCode)
ptreErrorCode = Lens.field @"errorCode"
{-# DEPRECATED ptreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that explains why the target addition failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreErrorMessage :: Lens.Lens' PutTargetsResultEntry (Core.Maybe Types.ErrorMessage)
ptreErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED ptreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreTargetId :: Lens.Lens' PutTargetsResultEntry (Core.Maybe Types.TargetId)
ptreTargetId = Lens.field @"targetId"
{-# DEPRECATED ptreTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON PutTargetsResultEntry where
  parseJSON =
    Core.withObject "PutTargetsResultEntry" Core.$
      \x ->
        PutTargetsResultEntry'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "TargetId")

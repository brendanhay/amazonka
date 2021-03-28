{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedTraceSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.UnprocessedTraceSegment
  ( UnprocessedTraceSegment (..)
  -- * Smart constructor
  , mkUnprocessedTraceSegment
  -- * Lenses
  , utsErrorCode
  , utsId
  , utsMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a segment that failed processing.
--
-- /See:/ 'mkUnprocessedTraceSegment' smart constructor.
data UnprocessedTraceSegment = UnprocessedTraceSegment'
  { errorCode :: Core.Maybe Core.Text
    -- ^ The error that caused processing to fail.
  , id :: Core.Maybe Core.Text
    -- ^ The segment's ID.
  , message :: Core.Maybe Core.Text
    -- ^ The error message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedTraceSegment' value with any optional fields omitted.
mkUnprocessedTraceSegment
    :: UnprocessedTraceSegment
mkUnprocessedTraceSegment
  = UnprocessedTraceSegment'{errorCode = Core.Nothing,
                             id = Core.Nothing, message = Core.Nothing}

-- | The error that caused processing to fail.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsErrorCode :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
utsErrorCode = Lens.field @"errorCode"
{-# INLINEABLE utsErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The segment's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsId :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
utsId = Lens.field @"id"
{-# INLINEABLE utsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsMessage :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
utsMessage = Lens.field @"message"
{-# INLINEABLE utsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON UnprocessedTraceSegment where
        parseJSON
          = Core.withObject "UnprocessedTraceSegment" Core.$
              \ x ->
                UnprocessedTraceSegment' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Message"

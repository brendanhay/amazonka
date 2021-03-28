{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
  ( PutEventsResultEntry (..)
  -- * Smart constructor
  , mkPutEventsResultEntry
  -- * Lenses
  , pereErrorCode
  , pereErrorMessage
  , pereEventId
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.ErrorCode as Types
import qualified Network.AWS.CloudWatchEvents.Types.ErrorMessage as Types
import qualified Network.AWS.CloudWatchEvents.Types.EventId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an event that failed to be submitted.
--
-- /See:/ 'mkPutEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that indicates why the event submission failed.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The error message that explains why the event submission failed.
  , eventId :: Core.Maybe Types.EventId
    -- ^ The ID of the event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventsResultEntry' value with any optional fields omitted.
mkPutEventsResultEntry
    :: PutEventsResultEntry
mkPutEventsResultEntry
  = PutEventsResultEntry'{errorCode = Core.Nothing,
                          errorMessage = Core.Nothing, eventId = Core.Nothing}

-- | The error code that indicates why the event submission failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereErrorCode :: Lens.Lens' PutEventsResultEntry (Core.Maybe Types.ErrorCode)
pereErrorCode = Lens.field @"errorCode"
{-# INLINEABLE pereErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message that explains why the event submission failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereErrorMessage :: Lens.Lens' PutEventsResultEntry (Core.Maybe Types.ErrorMessage)
pereErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE pereErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereEventId :: Lens.Lens' PutEventsResultEntry (Core.Maybe Types.EventId)
pereEventId = Lens.field @"eventId"
{-# INLINEABLE pereEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

instance Core.FromJSON PutEventsResultEntry where
        parseJSON
          = Core.withObject "PutEventsResultEntry" Core.$
              \ x ->
                PutEventsResultEntry' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "EventId"

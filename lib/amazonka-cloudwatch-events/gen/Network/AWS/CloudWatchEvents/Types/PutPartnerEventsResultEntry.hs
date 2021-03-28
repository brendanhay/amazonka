{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
  ( PutPartnerEventsResultEntry (..)
  -- * Smart constructor
  , mkPutPartnerEventsResultEntry
  -- * Lenses
  , ppereErrorCode
  , ppereErrorMessage
  , ppereEventId
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.ErrorCode as Types
import qualified Network.AWS.CloudWatchEvents.Types.ErrorMessage as Types
import qualified Network.AWS.CloudWatchEvents.Types.EventId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an event that a partner tried to generate, but failed.
--
-- /See:/ 'mkPutPartnerEventsResultEntry' smart constructor.
data PutPartnerEventsResultEntry = PutPartnerEventsResultEntry'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code that indicates why the event submission failed.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The error message that explains why the event submission failed.
  , eventId :: Core.Maybe Types.EventId
    -- ^ The ID of the event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutPartnerEventsResultEntry' value with any optional fields omitted.
mkPutPartnerEventsResultEntry
    :: PutPartnerEventsResultEntry
mkPutPartnerEventsResultEntry
  = PutPartnerEventsResultEntry'{errorCode = Core.Nothing,
                                 errorMessage = Core.Nothing, eventId = Core.Nothing}

-- | The error code that indicates why the event submission failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereErrorCode :: Lens.Lens' PutPartnerEventsResultEntry (Core.Maybe Types.ErrorCode)
ppereErrorCode = Lens.field @"errorCode"
{-# INLINEABLE ppereErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message that explains why the event submission failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereErrorMessage :: Lens.Lens' PutPartnerEventsResultEntry (Core.Maybe Types.ErrorMessage)
ppereErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE ppereErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereEventId :: Lens.Lens' PutPartnerEventsResultEntry (Core.Maybe Types.EventId)
ppereEventId = Lens.field @"eventId"
{-# INLINEABLE ppereEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

instance Core.FromJSON PutPartnerEventsResultEntry where
        parseJSON
          = Core.withObject "PutPartnerEventsResultEntry" Core.$
              \ x ->
                PutPartnerEventsResultEntry' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "EventId"

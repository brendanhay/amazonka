{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
  ( PutPartnerEventsResultEntry (..),

    -- * Smart constructor
    mkPutPartnerEventsResultEntry,

    -- * Lenses
    ppereErrorCode,
    ppereErrorMessage,
    ppereEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an event that a partner tried to generate, but failed.
--
-- /See:/ 'mkPutPartnerEventsResultEntry' smart constructor.
data PutPartnerEventsResultEntry = PutPartnerEventsResultEntry'
  { errorCode ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPartnerEventsResultEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the event submission failed.
-- * 'errorMessage' - The error message that explains why the event submission failed.
-- * 'eventId' - The ID of the event.
mkPutPartnerEventsResultEntry ::
  PutPartnerEventsResultEntry
mkPutPartnerEventsResultEntry =
  PutPartnerEventsResultEntry'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The error code that indicates why the event submission failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereErrorCode :: Lens.Lens' PutPartnerEventsResultEntry (Lude.Maybe Lude.Text)
ppereErrorCode = Lens.lens (errorCode :: PutPartnerEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: PutPartnerEventsResultEntry)
{-# DEPRECATED ppereErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that explains why the event submission failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereErrorMessage :: Lens.Lens' PutPartnerEventsResultEntry (Lude.Maybe Lude.Text)
ppereErrorMessage = Lens.lens (errorMessage :: PutPartnerEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: PutPartnerEventsResultEntry)
{-# DEPRECATED ppereErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereEventId :: Lens.Lens' PutPartnerEventsResultEntry (Lude.Maybe Lude.Text)
ppereEventId = Lens.lens (eventId :: PutPartnerEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: PutPartnerEventsResultEntry)
{-# DEPRECATED ppereEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON PutPartnerEventsResultEntry where
  parseJSON =
    Lude.withObject
      "PutPartnerEventsResultEntry"
      ( \x ->
          PutPartnerEventsResultEntry'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "EventId")
      )

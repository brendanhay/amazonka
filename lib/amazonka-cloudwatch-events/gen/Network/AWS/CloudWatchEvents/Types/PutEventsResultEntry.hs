{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
  ( PutEventsResultEntry (..),

    -- * Smart constructor
    mkPutEventsResultEntry,

    -- * Lenses
    pereErrorCode,
    pereErrorMessage,
    pereEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an event that failed to be submitted.
--
-- /See:/ 'mkPutEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
  { errorCode ::
      Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PutEventsResultEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the event submission failed.
-- * 'errorMessage' - The error message that explains why the event submission failed.
-- * 'eventId' - The ID of the event.
mkPutEventsResultEntry ::
  PutEventsResultEntry
mkPutEventsResultEntry =
  PutEventsResultEntry'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The error code that indicates why the event submission failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereErrorCode :: Lens.Lens' PutEventsResultEntry (Lude.Maybe Lude.Text)
pereErrorCode = Lens.lens (errorCode :: PutEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: PutEventsResultEntry)
{-# DEPRECATED pereErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that explains why the event submission failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereErrorMessage :: Lens.Lens' PutEventsResultEntry (Lude.Maybe Lude.Text)
pereErrorMessage = Lens.lens (errorMessage :: PutEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: PutEventsResultEntry)
{-# DEPRECATED pereErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereEventId :: Lens.Lens' PutEventsResultEntry (Lude.Maybe Lude.Text)
pereEventId = Lens.lens (eventId :: PutEventsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: PutEventsResultEntry)
{-# DEPRECATED pereEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON PutEventsResultEntry where
  parseJSON =
    Lude.withObject
      "PutEventsResultEntry"
      ( \x ->
          PutEventsResultEntry'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "EventId")
      )

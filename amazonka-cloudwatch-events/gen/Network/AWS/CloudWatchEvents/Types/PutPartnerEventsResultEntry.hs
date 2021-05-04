{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an event that a partner tried to generate, but failed.
--
-- /See:/ 'newPutPartnerEventsResultEntry' smart constructor.
data PutPartnerEventsResultEntry = PutPartnerEventsResultEntry'
  { -- | The ID of the event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The error message that explains why the event submission failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the event submission failed.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutPartnerEventsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'putPartnerEventsResultEntry_eventId' - The ID of the event.
--
-- 'errorMessage', 'putPartnerEventsResultEntry_errorMessage' - The error message that explains why the event submission failed.
--
-- 'errorCode', 'putPartnerEventsResultEntry_errorCode' - The error code that indicates why the event submission failed.
newPutPartnerEventsResultEntry ::
  PutPartnerEventsResultEntry
newPutPartnerEventsResultEntry =
  PutPartnerEventsResultEntry'
    { eventId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The ID of the event.
putPartnerEventsResultEntry_eventId :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_eventId = Lens.lens (\PutPartnerEventsResultEntry' {eventId} -> eventId) (\s@PutPartnerEventsResultEntry' {} a -> s {eventId = a} :: PutPartnerEventsResultEntry)

-- | The error message that explains why the event submission failed.
putPartnerEventsResultEntry_errorMessage :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_errorMessage = Lens.lens (\PutPartnerEventsResultEntry' {errorMessage} -> errorMessage) (\s@PutPartnerEventsResultEntry' {} a -> s {errorMessage = a} :: PutPartnerEventsResultEntry)

-- | The error code that indicates why the event submission failed.
putPartnerEventsResultEntry_errorCode :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_errorCode = Lens.lens (\PutPartnerEventsResultEntry' {errorCode} -> errorCode) (\s@PutPartnerEventsResultEntry' {} a -> s {errorCode = a} :: PutPartnerEventsResultEntry)

instance Prelude.FromJSON PutPartnerEventsResultEntry where
  parseJSON =
    Prelude.withObject
      "PutPartnerEventsResultEntry"
      ( \x ->
          PutPartnerEventsResultEntry'
            Prelude.<$> (x Prelude..:? "EventId")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable PutPartnerEventsResultEntry

instance Prelude.NFData PutPartnerEventsResultEntry

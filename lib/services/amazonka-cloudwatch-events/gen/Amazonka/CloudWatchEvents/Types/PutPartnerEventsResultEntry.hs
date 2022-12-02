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
-- Module      : Amazonka.CloudWatchEvents.Types.PutPartnerEventsResultEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.PutPartnerEventsResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an event that a partner tried to generate, but failed.
--
-- /See:/ 'newPutPartnerEventsResultEntry' smart constructor.
data PutPartnerEventsResultEntry = PutPartnerEventsResultEntry'
  { -- | The error message that explains why the event submission failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the event submission failed.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPartnerEventsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'putPartnerEventsResultEntry_errorMessage' - The error message that explains why the event submission failed.
--
-- 'eventId', 'putPartnerEventsResultEntry_eventId' - The ID of the event.
--
-- 'errorCode', 'putPartnerEventsResultEntry_errorCode' - The error code that indicates why the event submission failed.
newPutPartnerEventsResultEntry ::
  PutPartnerEventsResultEntry
newPutPartnerEventsResultEntry =
  PutPartnerEventsResultEntry'
    { errorMessage =
        Prelude.Nothing,
      eventId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message that explains why the event submission failed.
putPartnerEventsResultEntry_errorMessage :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_errorMessage = Lens.lens (\PutPartnerEventsResultEntry' {errorMessage} -> errorMessage) (\s@PutPartnerEventsResultEntry' {} a -> s {errorMessage = a} :: PutPartnerEventsResultEntry)

-- | The ID of the event.
putPartnerEventsResultEntry_eventId :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_eventId = Lens.lens (\PutPartnerEventsResultEntry' {eventId} -> eventId) (\s@PutPartnerEventsResultEntry' {} a -> s {eventId = a} :: PutPartnerEventsResultEntry)

-- | The error code that indicates why the event submission failed.
putPartnerEventsResultEntry_errorCode :: Lens.Lens' PutPartnerEventsResultEntry (Prelude.Maybe Prelude.Text)
putPartnerEventsResultEntry_errorCode = Lens.lens (\PutPartnerEventsResultEntry' {errorCode} -> errorCode) (\s@PutPartnerEventsResultEntry' {} a -> s {errorCode = a} :: PutPartnerEventsResultEntry)

instance Data.FromJSON PutPartnerEventsResultEntry where
  parseJSON =
    Data.withObject
      "PutPartnerEventsResultEntry"
      ( \x ->
          PutPartnerEventsResultEntry'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "EventId")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable PutPartnerEventsResultEntry where
  hashWithSalt _salt PutPartnerEventsResultEntry' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData PutPartnerEventsResultEntry where
  rnf PutPartnerEventsResultEntry' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf errorCode

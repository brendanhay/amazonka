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
-- Module      : Amazonka.Evidently.Types.PutProjectEventsResultEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.PutProjectEventsResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains Evidently\'s response to the sent events,
-- including an event ID and error codes, if any.
--
-- /See:/ 'newPutProjectEventsResultEntry' smart constructor.
data PutProjectEventsResultEntry = PutProjectEventsResultEntry'
  { -- | If the @PutProjectEvents@ operation has an error, the error code is
    -- returned here.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | If the @PutProjectEvents@ operation has an error, the error message is
    -- returned here.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | A unique ID assigned to this @PutProjectEvents@ operation.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProjectEventsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'putProjectEventsResultEntry_errorCode' - If the @PutProjectEvents@ operation has an error, the error code is
-- returned here.
--
-- 'errorMessage', 'putProjectEventsResultEntry_errorMessage' - If the @PutProjectEvents@ operation has an error, the error message is
-- returned here.
--
-- 'eventId', 'putProjectEventsResultEntry_eventId' - A unique ID assigned to this @PutProjectEvents@ operation.
newPutProjectEventsResultEntry ::
  PutProjectEventsResultEntry
newPutProjectEventsResultEntry =
  PutProjectEventsResultEntry'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | If the @PutProjectEvents@ operation has an error, the error code is
-- returned here.
putProjectEventsResultEntry_errorCode :: Lens.Lens' PutProjectEventsResultEntry (Prelude.Maybe Prelude.Text)
putProjectEventsResultEntry_errorCode = Lens.lens (\PutProjectEventsResultEntry' {errorCode} -> errorCode) (\s@PutProjectEventsResultEntry' {} a -> s {errorCode = a} :: PutProjectEventsResultEntry)

-- | If the @PutProjectEvents@ operation has an error, the error message is
-- returned here.
putProjectEventsResultEntry_errorMessage :: Lens.Lens' PutProjectEventsResultEntry (Prelude.Maybe Prelude.Text)
putProjectEventsResultEntry_errorMessage = Lens.lens (\PutProjectEventsResultEntry' {errorMessage} -> errorMessage) (\s@PutProjectEventsResultEntry' {} a -> s {errorMessage = a} :: PutProjectEventsResultEntry)

-- | A unique ID assigned to this @PutProjectEvents@ operation.
putProjectEventsResultEntry_eventId :: Lens.Lens' PutProjectEventsResultEntry (Prelude.Maybe Prelude.Text)
putProjectEventsResultEntry_eventId = Lens.lens (\PutProjectEventsResultEntry' {eventId} -> eventId) (\s@PutProjectEventsResultEntry' {} a -> s {eventId = a} :: PutProjectEventsResultEntry)

instance Data.FromJSON PutProjectEventsResultEntry where
  parseJSON =
    Data.withObject
      "PutProjectEventsResultEntry"
      ( \x ->
          PutProjectEventsResultEntry'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "eventId")
      )

instance Prelude.Hashable PutProjectEventsResultEntry where
  hashWithSalt _salt PutProjectEventsResultEntry' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` eventId

instance Prelude.NFData PutProjectEventsResultEntry where
  rnf PutProjectEventsResultEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf eventId

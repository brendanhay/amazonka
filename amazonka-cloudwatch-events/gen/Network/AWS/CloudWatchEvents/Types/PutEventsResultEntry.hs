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
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an event that failed to be submitted.
--
-- /See:/ 'newPutEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
  { -- | The ID of the event.
    eventId :: Core.Maybe Core.Text,
    -- | The error message that explains why the event submission failed.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code that indicates why the event submission failed.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEventsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'putEventsResultEntry_eventId' - The ID of the event.
--
-- 'errorMessage', 'putEventsResultEntry_errorMessage' - The error message that explains why the event submission failed.
--
-- 'errorCode', 'putEventsResultEntry_errorCode' - The error code that indicates why the event submission failed.
newPutEventsResultEntry ::
  PutEventsResultEntry
newPutEventsResultEntry =
  PutEventsResultEntry'
    { eventId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The ID of the event.
putEventsResultEntry_eventId :: Lens.Lens' PutEventsResultEntry (Core.Maybe Core.Text)
putEventsResultEntry_eventId = Lens.lens (\PutEventsResultEntry' {eventId} -> eventId) (\s@PutEventsResultEntry' {} a -> s {eventId = a} :: PutEventsResultEntry)

-- | The error message that explains why the event submission failed.
putEventsResultEntry_errorMessage :: Lens.Lens' PutEventsResultEntry (Core.Maybe Core.Text)
putEventsResultEntry_errorMessage = Lens.lens (\PutEventsResultEntry' {errorMessage} -> errorMessage) (\s@PutEventsResultEntry' {} a -> s {errorMessage = a} :: PutEventsResultEntry)

-- | The error code that indicates why the event submission failed.
putEventsResultEntry_errorCode :: Lens.Lens' PutEventsResultEntry (Core.Maybe Core.Text)
putEventsResultEntry_errorCode = Lens.lens (\PutEventsResultEntry' {errorCode} -> errorCode) (\s@PutEventsResultEntry' {} a -> s {errorCode = a} :: PutEventsResultEntry)

instance Core.FromJSON PutEventsResultEntry where
  parseJSON =
    Core.withObject
      "PutEventsResultEntry"
      ( \x ->
          PutEventsResultEntry'
            Core.<$> (x Core..:? "EventId")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable PutEventsResultEntry

instance Core.NFData PutEventsResultEntry

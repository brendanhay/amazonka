{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutPartnerEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is used by SaaS partners to write events to a customer\'s partner
-- event bus. AWS customers do not use this operation.
module Network.AWS.CloudWatchEvents.PutPartnerEvents
  ( -- * Creating a Request
    PutPartnerEvents (..),
    newPutPartnerEvents,

    -- * Request Lenses
    putPartnerEvents_entries,

    -- * Destructuring the Response
    PutPartnerEventsResponse (..),
    newPutPartnerEventsResponse,

    -- * Response Lenses
    putPartnerEventsResponse_failedEntryCount,
    putPartnerEventsResponse_entries,
    putPartnerEventsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutPartnerEvents' smart constructor.
data PutPartnerEvents = PutPartnerEvents'
  { -- | The list of events to write to the event bus.
    entries :: Core.NonEmpty PutPartnerEventsRequestEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutPartnerEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'putPartnerEvents_entries' - The list of events to write to the event bus.
newPutPartnerEvents ::
  -- | 'entries'
  Core.NonEmpty PutPartnerEventsRequestEntry ->
  PutPartnerEvents
newPutPartnerEvents pEntries_ =
  PutPartnerEvents'
    { entries =
        Lens._Coerce Lens.# pEntries_
    }

-- | The list of events to write to the event bus.
putPartnerEvents_entries :: Lens.Lens' PutPartnerEvents (Core.NonEmpty PutPartnerEventsRequestEntry)
putPartnerEvents_entries = Lens.lens (\PutPartnerEvents' {entries} -> entries) (\s@PutPartnerEvents' {} a -> s {entries = a} :: PutPartnerEvents) Core.. Lens._Coerce

instance Core.AWSRequest PutPartnerEvents where
  type
    AWSResponse PutPartnerEvents =
      PutPartnerEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPartnerEventsResponse'
            Core.<$> (x Core..?> "FailedEntryCount")
            Core.<*> (x Core..?> "Entries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutPartnerEvents

instance Core.NFData PutPartnerEvents

instance Core.ToHeaders PutPartnerEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.PutPartnerEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutPartnerEvents where
  toJSON PutPartnerEvents' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Entries" Core..= entries)]
      )

instance Core.ToPath PutPartnerEvents where
  toPath = Core.const "/"

instance Core.ToQuery PutPartnerEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutPartnerEventsResponse' smart constructor.
data PutPartnerEventsResponse = PutPartnerEventsResponse'
  { -- | The number of events from this operation that could not be written to
    -- the partner event bus.
    failedEntryCount :: Core.Maybe Core.Int,
    -- | The list of events from this operation that were successfully written to
    -- the partner event bus.
    entries :: Core.Maybe [PutPartnerEventsResultEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutPartnerEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntryCount', 'putPartnerEventsResponse_failedEntryCount' - The number of events from this operation that could not be written to
-- the partner event bus.
--
-- 'entries', 'putPartnerEventsResponse_entries' - The list of events from this operation that were successfully written to
-- the partner event bus.
--
-- 'httpStatus', 'putPartnerEventsResponse_httpStatus' - The response's http status code.
newPutPartnerEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutPartnerEventsResponse
newPutPartnerEventsResponse pHttpStatus_ =
  PutPartnerEventsResponse'
    { failedEntryCount =
        Core.Nothing,
      entries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of events from this operation that could not be written to
-- the partner event bus.
putPartnerEventsResponse_failedEntryCount :: Lens.Lens' PutPartnerEventsResponse (Core.Maybe Core.Int)
putPartnerEventsResponse_failedEntryCount = Lens.lens (\PutPartnerEventsResponse' {failedEntryCount} -> failedEntryCount) (\s@PutPartnerEventsResponse' {} a -> s {failedEntryCount = a} :: PutPartnerEventsResponse)

-- | The list of events from this operation that were successfully written to
-- the partner event bus.
putPartnerEventsResponse_entries :: Lens.Lens' PutPartnerEventsResponse (Core.Maybe [PutPartnerEventsResultEntry])
putPartnerEventsResponse_entries = Lens.lens (\PutPartnerEventsResponse' {entries} -> entries) (\s@PutPartnerEventsResponse' {} a -> s {entries = a} :: PutPartnerEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putPartnerEventsResponse_httpStatus :: Lens.Lens' PutPartnerEventsResponse Core.Int
putPartnerEventsResponse_httpStatus = Lens.lens (\PutPartnerEventsResponse' {httpStatus} -> httpStatus) (\s@PutPartnerEventsResponse' {} a -> s {httpStatus = a} :: PutPartnerEventsResponse)

instance Core.NFData PutPartnerEventsResponse

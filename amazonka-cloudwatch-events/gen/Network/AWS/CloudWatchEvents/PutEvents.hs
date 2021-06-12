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
-- Module      : Network.AWS.CloudWatchEvents.PutEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends custom events to Amazon EventBridge so that they can be matched to
-- rules.
module Network.AWS.CloudWatchEvents.PutEvents
  ( -- * Creating a Request
    PutEvents (..),
    newPutEvents,

    -- * Request Lenses
    putEvents_entries,

    -- * Destructuring the Response
    PutEventsResponse (..),
    newPutEventsResponse,

    -- * Response Lenses
    putEventsResponse_failedEntryCount,
    putEventsResponse_entries,
    putEventsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutEvents' smart constructor.
data PutEvents = PutEvents'
  { -- | The entry that defines an event in your system. You can specify several
    -- parameters for the entry such as the source and type of the event,
    -- resources associated with the event, and so on.
    entries :: Core.NonEmpty PutEventsRequestEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'putEvents_entries' - The entry that defines an event in your system. You can specify several
-- parameters for the entry such as the source and type of the event,
-- resources associated with the event, and so on.
newPutEvents ::
  -- | 'entries'
  Core.NonEmpty PutEventsRequestEntry ->
  PutEvents
newPutEvents pEntries_ =
  PutEvents' {entries = Lens._Coerce Lens.# pEntries_}

-- | The entry that defines an event in your system. You can specify several
-- parameters for the entry such as the source and type of the event,
-- resources associated with the event, and so on.
putEvents_entries :: Lens.Lens' PutEvents (Core.NonEmpty PutEventsRequestEntry)
putEvents_entries = Lens.lens (\PutEvents' {entries} -> entries) (\s@PutEvents' {} a -> s {entries = a} :: PutEvents) Core.. Lens._Coerce

instance Core.AWSRequest PutEvents where
  type AWSResponse PutEvents = PutEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Core.<$> (x Core..?> "FailedEntryCount")
            Core.<*> (x Core..?> "Entries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutEvents

instance Core.NFData PutEvents

instance Core.ToHeaders PutEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.PutEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutEvents where
  toJSON PutEvents' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Entries" Core..= entries)]
      )

instance Core.ToPath PutEvents where
  toPath = Core.const "/"

instance Core.ToQuery PutEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Core.Maybe Core.Int,
    -- | The successfully and unsuccessfully ingested events results. If the
    -- ingestion was successful, the entry has the event ID in it. Otherwise,
    -- you can use the error code and error message to identify the problem
    -- with the entry.
    entries :: Core.Maybe [PutEventsResultEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntryCount', 'putEventsResponse_failedEntryCount' - The number of failed entries.
--
-- 'entries', 'putEventsResponse_entries' - The successfully and unsuccessfully ingested events results. If the
-- ingestion was successful, the entry has the event ID in it. Otherwise,
-- you can use the error code and error message to identify the problem
-- with the entry.
--
-- 'httpStatus', 'putEventsResponse_httpStatus' - The response's http status code.
newPutEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutEventsResponse
newPutEventsResponse pHttpStatus_ =
  PutEventsResponse'
    { failedEntryCount = Core.Nothing,
      entries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of failed entries.
putEventsResponse_failedEntryCount :: Lens.Lens' PutEventsResponse (Core.Maybe Core.Int)
putEventsResponse_failedEntryCount = Lens.lens (\PutEventsResponse' {failedEntryCount} -> failedEntryCount) (\s@PutEventsResponse' {} a -> s {failedEntryCount = a} :: PutEventsResponse)

-- | The successfully and unsuccessfully ingested events results. If the
-- ingestion was successful, the entry has the event ID in it. Otherwise,
-- you can use the error code and error message to identify the problem
-- with the entry.
putEventsResponse_entries :: Lens.Lens' PutEventsResponse (Core.Maybe [PutEventsResultEntry])
putEventsResponse_entries = Lens.lens (\PutEventsResponse' {entries} -> entries) (\s@PutEventsResponse' {} a -> s {entries = a} :: PutEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putEventsResponse_httpStatus :: Lens.Lens' PutEventsResponse Core.Int
putEventsResponse_httpStatus = Lens.lens (\PutEventsResponse' {httpStatus} -> httpStatus) (\s@PutEventsResponse' {} a -> s {httpStatus = a} :: PutEventsResponse)

instance Core.NFData PutEventsResponse

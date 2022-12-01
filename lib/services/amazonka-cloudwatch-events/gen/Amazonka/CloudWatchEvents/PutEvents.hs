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
-- Module      : Amazonka.CloudWatchEvents.PutEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends custom events to Amazon EventBridge so that they can be matched to
-- rules.
--
-- PutEvents will only process nested JSON up to 1100 levels deep.
module Amazonka.CloudWatchEvents.PutEvents
  ( -- * Creating a Request
    PutEvents (..),
    newPutEvents,

    -- * Request Lenses
    putEvents_endpointId,
    putEvents_entries,

    -- * Destructuring the Response
    PutEventsResponse (..),
    newPutEventsResponse,

    -- * Response Lenses
    putEventsResponse_entries,
    putEventsResponse_failedEntryCount,
    putEventsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutEvents' smart constructor.
data PutEvents = PutEvents'
  { -- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
    -- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
    -- @abcde.veo@.
    --
    -- When using Java, you must include @auth-crt@ on the class path.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The entry that defines an event in your system. You can specify several
    -- parameters for the entry such as the source and type of the event,
    -- resources associated with the event, and so on.
    entries :: Prelude.NonEmpty PutEventsRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointId', 'putEvents_endpointId' - The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
-- @abcde.veo@.
--
-- When using Java, you must include @auth-crt@ on the class path.
--
-- 'entries', 'putEvents_entries' - The entry that defines an event in your system. You can specify several
-- parameters for the entry such as the source and type of the event,
-- resources associated with the event, and so on.
newPutEvents ::
  -- | 'entries'
  Prelude.NonEmpty PutEventsRequestEntry ->
  PutEvents
newPutEvents pEntries_ =
  PutEvents'
    { endpointId = Prelude.Nothing,
      entries = Lens.coerced Lens.# pEntries_
    }

-- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
-- @abcde.veo@.
--
-- When using Java, you must include @auth-crt@ on the class path.
putEvents_endpointId :: Lens.Lens' PutEvents (Prelude.Maybe Prelude.Text)
putEvents_endpointId = Lens.lens (\PutEvents' {endpointId} -> endpointId) (\s@PutEvents' {} a -> s {endpointId = a} :: PutEvents)

-- | The entry that defines an event in your system. You can specify several
-- parameters for the entry such as the source and type of the event,
-- resources associated with the event, and so on.
putEvents_entries :: Lens.Lens' PutEvents (Prelude.NonEmpty PutEventsRequestEntry)
putEvents_entries = Lens.lens (\PutEvents' {entries} -> entries) (\s@PutEvents' {} a -> s {entries = a} :: PutEvents) Prelude.. Lens.coerced

instance Core.AWSRequest PutEvents where
  type AWSResponse PutEvents = PutEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Prelude.<$> (x Core..?> "Entries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "FailedEntryCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEvents where
  hashWithSalt _salt PutEvents' {..} =
    _salt `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData PutEvents where
  rnf PutEvents' {..} =
    Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf entries

instance Core.ToHeaders PutEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.PutEvents" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEvents where
  toJSON PutEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndpointId" Core..=) Prelude.<$> endpointId,
            Prelude.Just ("Entries" Core..= entries)
          ]
      )

instance Core.ToPath PutEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery PutEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { -- | The successfully and unsuccessfully ingested events results. If the
    -- ingestion was successful, the entry has the event ID in it. Otherwise,
    -- you can use the error code and error message to identify the problem
    -- with the entry.
    entries :: Prelude.Maybe [PutEventsResultEntry],
    -- | The number of failed entries.
    failedEntryCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'putEventsResponse_entries' - The successfully and unsuccessfully ingested events results. If the
-- ingestion was successful, the entry has the event ID in it. Otherwise,
-- you can use the error code and error message to identify the problem
-- with the entry.
--
-- 'failedEntryCount', 'putEventsResponse_failedEntryCount' - The number of failed entries.
--
-- 'httpStatus', 'putEventsResponse_httpStatus' - The response's http status code.
newPutEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEventsResponse
newPutEventsResponse pHttpStatus_ =
  PutEventsResponse'
    { entries = Prelude.Nothing,
      failedEntryCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The successfully and unsuccessfully ingested events results. If the
-- ingestion was successful, the entry has the event ID in it. Otherwise,
-- you can use the error code and error message to identify the problem
-- with the entry.
putEventsResponse_entries :: Lens.Lens' PutEventsResponse (Prelude.Maybe [PutEventsResultEntry])
putEventsResponse_entries = Lens.lens (\PutEventsResponse' {entries} -> entries) (\s@PutEventsResponse' {} a -> s {entries = a} :: PutEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of failed entries.
putEventsResponse_failedEntryCount :: Lens.Lens' PutEventsResponse (Prelude.Maybe Prelude.Int)
putEventsResponse_failedEntryCount = Lens.lens (\PutEventsResponse' {failedEntryCount} -> failedEntryCount) (\s@PutEventsResponse' {} a -> s {failedEntryCount = a} :: PutEventsResponse)

-- | The response's http status code.
putEventsResponse_httpStatus :: Lens.Lens' PutEventsResponse Prelude.Int
putEventsResponse_httpStatus = Lens.lens (\PutEventsResponse' {httpStatus} -> httpStatus) (\s@PutEventsResponse' {} a -> s {httpStatus = a} :: PutEventsResponse)

instance Prelude.NFData PutEventsResponse where
  rnf PutEventsResponse' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf failedEntryCount
      `Prelude.seq` Prelude.rnf httpStatus

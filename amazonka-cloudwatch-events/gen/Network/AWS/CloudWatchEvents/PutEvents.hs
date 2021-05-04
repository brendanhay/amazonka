{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutEvents' smart constructor.
data PutEvents = PutEvents'
  { -- | The entry that defines an event in your system. You can specify several
    -- parameters for the entry such as the source and type of the event,
    -- resources associated with the event, and so on.
    entries :: Prelude.NonEmpty PutEventsRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.NonEmpty PutEventsRequestEntry ->
  PutEvents
newPutEvents pEntries_ =
  PutEvents'
    { entries =
        Prelude._Coerce Lens.# pEntries_
    }

-- | The entry that defines an event in your system. You can specify several
-- parameters for the entry such as the source and type of the event,
-- resources associated with the event, and so on.
putEvents_entries :: Lens.Lens' PutEvents (Prelude.NonEmpty PutEventsRequestEntry)
putEvents_entries = Lens.lens (\PutEvents' {entries} -> entries) (\s@PutEvents' {} a -> s {entries = a} :: PutEvents) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutEvents where
  type Rs PutEvents = PutEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Prelude.<$> (x Prelude..?> "FailedEntryCount")
            Prelude.<*> (x Prelude..?> "Entries" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEvents

instance Prelude.NFData PutEvents

instance Prelude.ToHeaders PutEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.PutEvents" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutEvents where
  toJSON PutEvents' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Entries" Prelude..= entries)]
      )

instance Prelude.ToPath PutEvents where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Prelude.Maybe Prelude.Int,
    -- | The successfully and unsuccessfully ingested events results. If the
    -- ingestion was successful, the entry has the event ID in it. Otherwise,
    -- you can use the error code and error message to identify the problem
    -- with the entry.
    entries :: Prelude.Maybe [PutEventsResultEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutEventsResponse
newPutEventsResponse pHttpStatus_ =
  PutEventsResponse'
    { failedEntryCount =
        Prelude.Nothing,
      entries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of failed entries.
putEventsResponse_failedEntryCount :: Lens.Lens' PutEventsResponse (Prelude.Maybe Prelude.Int)
putEventsResponse_failedEntryCount = Lens.lens (\PutEventsResponse' {failedEntryCount} -> failedEntryCount) (\s@PutEventsResponse' {} a -> s {failedEntryCount = a} :: PutEventsResponse)

-- | The successfully and unsuccessfully ingested events results. If the
-- ingestion was successful, the entry has the event ID in it. Otherwise,
-- you can use the error code and error message to identify the problem
-- with the entry.
putEventsResponse_entries :: Lens.Lens' PutEventsResponse (Prelude.Maybe [PutEventsResultEntry])
putEventsResponse_entries = Lens.lens (\PutEventsResponse' {entries} -> entries) (\s@PutEventsResponse' {} a -> s {entries = a} :: PutEventsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
putEventsResponse_httpStatus :: Lens.Lens' PutEventsResponse Prelude.Int
putEventsResponse_httpStatus = Lens.lens (\PutEventsResponse' {httpStatus} -> httpStatus) (\s@PutEventsResponse' {} a -> s {httpStatus = a} :: PutEventsResponse)

instance Prelude.NFData PutEventsResponse

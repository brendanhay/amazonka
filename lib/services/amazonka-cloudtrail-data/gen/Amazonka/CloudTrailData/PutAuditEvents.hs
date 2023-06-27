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
-- Module      : Amazonka.CloudTrailData.PutAuditEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ingests your application events into CloudTrail Lake. A required
-- parameter, @auditEvents@, accepts the JSON records (also called
-- /payload/) of events that you want CloudTrail to ingest. You can add up
-- to 100 of these events (or up to 1 MB) per @PutAuditEvents@ request.
module Amazonka.CloudTrailData.PutAuditEvents
  ( -- * Creating a Request
    PutAuditEvents (..),
    newPutAuditEvents,

    -- * Request Lenses
    putAuditEvents_externalId,
    putAuditEvents_auditEvents,
    putAuditEvents_channelArn,

    -- * Destructuring the Response
    PutAuditEventsResponse (..),
    newPutAuditEventsResponse,

    -- * Response Lenses
    putAuditEventsResponse_httpStatus,
    putAuditEventsResponse_failed,
    putAuditEventsResponse_successful,
  )
where

import Amazonka.CloudTrailData.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAuditEvents' smart constructor.
data PutAuditEvents = PutAuditEvents'
  { -- | A unique identifier that is conditionally required when the channel\'s
    -- resource policy includes an external ID. This value can be any string,
    -- such as a passphrase or account number.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The JSON payload of events that you want to ingest. You can also point
    -- to the JSON event payload in a file.
    auditEvents :: Prelude.NonEmpty AuditEvent,
    -- | The ARN or ID (the ARN suffix) of a channel.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAuditEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'putAuditEvents_externalId' - A unique identifier that is conditionally required when the channel\'s
-- resource policy includes an external ID. This value can be any string,
-- such as a passphrase or account number.
--
-- 'auditEvents', 'putAuditEvents_auditEvents' - The JSON payload of events that you want to ingest. You can also point
-- to the JSON event payload in a file.
--
-- 'channelArn', 'putAuditEvents_channelArn' - The ARN or ID (the ARN suffix) of a channel.
newPutAuditEvents ::
  -- | 'auditEvents'
  Prelude.NonEmpty AuditEvent ->
  -- | 'channelArn'
  Prelude.Text ->
  PutAuditEvents
newPutAuditEvents pAuditEvents_ pChannelArn_ =
  PutAuditEvents'
    { externalId = Prelude.Nothing,
      auditEvents = Lens.coerced Lens.# pAuditEvents_,
      channelArn = pChannelArn_
    }

-- | A unique identifier that is conditionally required when the channel\'s
-- resource policy includes an external ID. This value can be any string,
-- such as a passphrase or account number.
putAuditEvents_externalId :: Lens.Lens' PutAuditEvents (Prelude.Maybe Prelude.Text)
putAuditEvents_externalId = Lens.lens (\PutAuditEvents' {externalId} -> externalId) (\s@PutAuditEvents' {} a -> s {externalId = a} :: PutAuditEvents)

-- | The JSON payload of events that you want to ingest. You can also point
-- to the JSON event payload in a file.
putAuditEvents_auditEvents :: Lens.Lens' PutAuditEvents (Prelude.NonEmpty AuditEvent)
putAuditEvents_auditEvents = Lens.lens (\PutAuditEvents' {auditEvents} -> auditEvents) (\s@PutAuditEvents' {} a -> s {auditEvents = a} :: PutAuditEvents) Prelude.. Lens.coerced

-- | The ARN or ID (the ARN suffix) of a channel.
putAuditEvents_channelArn :: Lens.Lens' PutAuditEvents Prelude.Text
putAuditEvents_channelArn = Lens.lens (\PutAuditEvents' {channelArn} -> channelArn) (\s@PutAuditEvents' {} a -> s {channelArn = a} :: PutAuditEvents)

instance Core.AWSRequest PutAuditEvents where
  type
    AWSResponse PutAuditEvents =
      PutAuditEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAuditEventsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "successful" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable PutAuditEvents where
  hashWithSalt _salt PutAuditEvents' {..} =
    _salt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` auditEvents
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData PutAuditEvents where
  rnf PutAuditEvents' {..} =
    Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf auditEvents
      `Prelude.seq` Prelude.rnf channelArn

instance Data.ToHeaders PutAuditEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAuditEvents where
  toJSON PutAuditEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("auditEvents" Data..= auditEvents)]
      )

instance Data.ToPath PutAuditEvents where
  toPath = Prelude.const "/PutAuditEvents"

instance Data.ToQuery PutAuditEvents where
  toQuery PutAuditEvents' {..} =
    Prelude.mconcat
      [ "externalId" Data.=: externalId,
        "channelArn" Data.=: channelArn
      ]

-- | /See:/ 'newPutAuditEventsResponse' smart constructor.
data PutAuditEventsResponse = PutAuditEventsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists events in the provided event payload that could not be ingested
    -- into CloudTrail, and includes the error code and error message returned
    -- for events that could not be ingested.
    failed :: [ResultErrorEntry],
    -- | Lists events in the provided event payload that were successfully
    -- ingested into CloudTrail.
    successful :: [AuditEventResultEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAuditEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAuditEventsResponse_httpStatus' - The response's http status code.
--
-- 'failed', 'putAuditEventsResponse_failed' - Lists events in the provided event payload that could not be ingested
-- into CloudTrail, and includes the error code and error message returned
-- for events that could not be ingested.
--
-- 'successful', 'putAuditEventsResponse_successful' - Lists events in the provided event payload that were successfully
-- ingested into CloudTrail.
newPutAuditEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAuditEventsResponse
newPutAuditEventsResponse pHttpStatus_ =
  PutAuditEventsResponse'
    { httpStatus = pHttpStatus_,
      failed = Prelude.mempty,
      successful = Prelude.mempty
    }

-- | The response's http status code.
putAuditEventsResponse_httpStatus :: Lens.Lens' PutAuditEventsResponse Prelude.Int
putAuditEventsResponse_httpStatus = Lens.lens (\PutAuditEventsResponse' {httpStatus} -> httpStatus) (\s@PutAuditEventsResponse' {} a -> s {httpStatus = a} :: PutAuditEventsResponse)

-- | Lists events in the provided event payload that could not be ingested
-- into CloudTrail, and includes the error code and error message returned
-- for events that could not be ingested.
putAuditEventsResponse_failed :: Lens.Lens' PutAuditEventsResponse [ResultErrorEntry]
putAuditEventsResponse_failed = Lens.lens (\PutAuditEventsResponse' {failed} -> failed) (\s@PutAuditEventsResponse' {} a -> s {failed = a} :: PutAuditEventsResponse) Prelude.. Lens.coerced

-- | Lists events in the provided event payload that were successfully
-- ingested into CloudTrail.
putAuditEventsResponse_successful :: Lens.Lens' PutAuditEventsResponse [AuditEventResultEntry]
putAuditEventsResponse_successful = Lens.lens (\PutAuditEventsResponse' {successful} -> successful) (\s@PutAuditEventsResponse' {} a -> s {successful = a} :: PutAuditEventsResponse) Prelude.. Lens.coerced

instance Prelude.NFData PutAuditEventsResponse where
  rnf PutAuditEventsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf successful

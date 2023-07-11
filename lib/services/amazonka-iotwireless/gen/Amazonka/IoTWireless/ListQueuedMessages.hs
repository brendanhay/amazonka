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
-- Module      : Amazonka.IoTWireless.ListQueuedMessages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List queued messages in the downlink queue.
module Amazonka.IoTWireless.ListQueuedMessages
  ( -- * Creating a Request
    ListQueuedMessages (..),
    newListQueuedMessages,

    -- * Request Lenses
    listQueuedMessages_maxResults,
    listQueuedMessages_nextToken,
    listQueuedMessages_wirelessDeviceType,
    listQueuedMessages_id,

    -- * Destructuring the Response
    ListQueuedMessagesResponse (..),
    newListQueuedMessagesResponse,

    -- * Response Lenses
    listQueuedMessagesResponse_downlinkQueueMessagesList,
    listQueuedMessagesResponse_nextToken,
    listQueuedMessagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListQueuedMessages' smart constructor.
data ListQueuedMessages = ListQueuedMessages'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The wireless device type, whic can be either Sidewalk or LoRaWAN.
    wirelessDeviceType :: Prelude.Maybe WirelessDeviceType,
    -- | The ID of a given wireless device which the downlink message packets are
    -- being sent.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueuedMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listQueuedMessages_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listQueuedMessages_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'wirelessDeviceType', 'listQueuedMessages_wirelessDeviceType' - The wireless device type, whic can be either Sidewalk or LoRaWAN.
--
-- 'id', 'listQueuedMessages_id' - The ID of a given wireless device which the downlink message packets are
-- being sent.
newListQueuedMessages ::
  -- | 'id'
  Prelude.Text ->
  ListQueuedMessages
newListQueuedMessages pId_ =
  ListQueuedMessages'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      wirelessDeviceType = Prelude.Nothing,
      id = pId_
    }

-- | The maximum number of results to return in this operation.
listQueuedMessages_maxResults :: Lens.Lens' ListQueuedMessages (Prelude.Maybe Prelude.Natural)
listQueuedMessages_maxResults = Lens.lens (\ListQueuedMessages' {maxResults} -> maxResults) (\s@ListQueuedMessages' {} a -> s {maxResults = a} :: ListQueuedMessages)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listQueuedMessages_nextToken :: Lens.Lens' ListQueuedMessages (Prelude.Maybe Prelude.Text)
listQueuedMessages_nextToken = Lens.lens (\ListQueuedMessages' {nextToken} -> nextToken) (\s@ListQueuedMessages' {} a -> s {nextToken = a} :: ListQueuedMessages)

-- | The wireless device type, whic can be either Sidewalk or LoRaWAN.
listQueuedMessages_wirelessDeviceType :: Lens.Lens' ListQueuedMessages (Prelude.Maybe WirelessDeviceType)
listQueuedMessages_wirelessDeviceType = Lens.lens (\ListQueuedMessages' {wirelessDeviceType} -> wirelessDeviceType) (\s@ListQueuedMessages' {} a -> s {wirelessDeviceType = a} :: ListQueuedMessages)

-- | The ID of a given wireless device which the downlink message packets are
-- being sent.
listQueuedMessages_id :: Lens.Lens' ListQueuedMessages Prelude.Text
listQueuedMessages_id = Lens.lens (\ListQueuedMessages' {id} -> id) (\s@ListQueuedMessages' {} a -> s {id = a} :: ListQueuedMessages)

instance Core.AWSRequest ListQueuedMessages where
  type
    AWSResponse ListQueuedMessages =
      ListQueuedMessagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueuedMessagesResponse'
            Prelude.<$> ( x
                            Data..?> "DownlinkQueueMessagesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueuedMessages where
  hashWithSalt _salt ListQueuedMessages' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` wirelessDeviceType
      `Prelude.hashWithSalt` id

instance Prelude.NFData ListQueuedMessages where
  rnf ListQueuedMessages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf wirelessDeviceType
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders ListQueuedMessages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListQueuedMessages where
  toPath ListQueuedMessages' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id, "/data"]

instance Data.ToQuery ListQueuedMessages where
  toQuery ListQueuedMessages' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "WirelessDeviceType" Data.=: wirelessDeviceType
      ]

-- | /See:/ 'newListQueuedMessagesResponse' smart constructor.
data ListQueuedMessagesResponse = ListQueuedMessagesResponse'
  { -- | The messages in the downlink queue.
    downlinkQueueMessagesList :: Prelude.Maybe [DownlinkQueueMessage],
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueuedMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downlinkQueueMessagesList', 'listQueuedMessagesResponse_downlinkQueueMessagesList' - The messages in the downlink queue.
--
-- 'nextToken', 'listQueuedMessagesResponse_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'httpStatus', 'listQueuedMessagesResponse_httpStatus' - The response's http status code.
newListQueuedMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueuedMessagesResponse
newListQueuedMessagesResponse pHttpStatus_ =
  ListQueuedMessagesResponse'
    { downlinkQueueMessagesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The messages in the downlink queue.
listQueuedMessagesResponse_downlinkQueueMessagesList :: Lens.Lens' ListQueuedMessagesResponse (Prelude.Maybe [DownlinkQueueMessage])
listQueuedMessagesResponse_downlinkQueueMessagesList = Lens.lens (\ListQueuedMessagesResponse' {downlinkQueueMessagesList} -> downlinkQueueMessagesList) (\s@ListQueuedMessagesResponse' {} a -> s {downlinkQueueMessagesList = a} :: ListQueuedMessagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listQueuedMessagesResponse_nextToken :: Lens.Lens' ListQueuedMessagesResponse (Prelude.Maybe Prelude.Text)
listQueuedMessagesResponse_nextToken = Lens.lens (\ListQueuedMessagesResponse' {nextToken} -> nextToken) (\s@ListQueuedMessagesResponse' {} a -> s {nextToken = a} :: ListQueuedMessagesResponse)

-- | The response's http status code.
listQueuedMessagesResponse_httpStatus :: Lens.Lens' ListQueuedMessagesResponse Prelude.Int
listQueuedMessagesResponse_httpStatus = Lens.lens (\ListQueuedMessagesResponse' {httpStatus} -> httpStatus) (\s@ListQueuedMessagesResponse' {} a -> s {httpStatus = a} :: ListQueuedMessagesResponse)

instance Prelude.NFData ListQueuedMessagesResponse where
  rnf ListQueuedMessagesResponse' {..} =
    Prelude.rnf downlinkQueueMessagesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

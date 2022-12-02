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
-- Module      : Amazonka.IoTWireless.DeleteQueuedMessages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove queued messages from the downlink queue.
module Amazonka.IoTWireless.DeleteQueuedMessages
  ( -- * Creating a Request
    DeleteQueuedMessages (..),
    newDeleteQueuedMessages,

    -- * Request Lenses
    deleteQueuedMessages_wirelessDeviceType,
    deleteQueuedMessages_id,
    deleteQueuedMessages_messageId,

    -- * Destructuring the Response
    DeleteQueuedMessagesResponse (..),
    newDeleteQueuedMessagesResponse,

    -- * Response Lenses
    deleteQueuedMessagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteQueuedMessages' smart constructor.
data DeleteQueuedMessages = DeleteQueuedMessages'
  { -- | The wireless device type, which can be either Sidewalk or LoRaWAN.
    wirelessDeviceType :: Prelude.Maybe WirelessDeviceType,
    -- | The ID of a given wireless device for which downlink messages will be
    -- deleted.
    id :: Prelude.Text,
    -- | If message ID is @\"*\"@, it cleares the entire downlink queue for a
    -- given device, specified by the wireless device ID. Otherwise, the
    -- downlink message with the specified message ID will be deleted.
    messageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueuedMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessDeviceType', 'deleteQueuedMessages_wirelessDeviceType' - The wireless device type, which can be either Sidewalk or LoRaWAN.
--
-- 'id', 'deleteQueuedMessages_id' - The ID of a given wireless device for which downlink messages will be
-- deleted.
--
-- 'messageId', 'deleteQueuedMessages_messageId' - If message ID is @\"*\"@, it cleares the entire downlink queue for a
-- given device, specified by the wireless device ID. Otherwise, the
-- downlink message with the specified message ID will be deleted.
newDeleteQueuedMessages ::
  -- | 'id'
  Prelude.Text ->
  -- | 'messageId'
  Prelude.Text ->
  DeleteQueuedMessages
newDeleteQueuedMessages pId_ pMessageId_ =
  DeleteQueuedMessages'
    { wirelessDeviceType =
        Prelude.Nothing,
      id = pId_,
      messageId = pMessageId_
    }

-- | The wireless device type, which can be either Sidewalk or LoRaWAN.
deleteQueuedMessages_wirelessDeviceType :: Lens.Lens' DeleteQueuedMessages (Prelude.Maybe WirelessDeviceType)
deleteQueuedMessages_wirelessDeviceType = Lens.lens (\DeleteQueuedMessages' {wirelessDeviceType} -> wirelessDeviceType) (\s@DeleteQueuedMessages' {} a -> s {wirelessDeviceType = a} :: DeleteQueuedMessages)

-- | The ID of a given wireless device for which downlink messages will be
-- deleted.
deleteQueuedMessages_id :: Lens.Lens' DeleteQueuedMessages Prelude.Text
deleteQueuedMessages_id = Lens.lens (\DeleteQueuedMessages' {id} -> id) (\s@DeleteQueuedMessages' {} a -> s {id = a} :: DeleteQueuedMessages)

-- | If message ID is @\"*\"@, it cleares the entire downlink queue for a
-- given device, specified by the wireless device ID. Otherwise, the
-- downlink message with the specified message ID will be deleted.
deleteQueuedMessages_messageId :: Lens.Lens' DeleteQueuedMessages Prelude.Text
deleteQueuedMessages_messageId = Lens.lens (\DeleteQueuedMessages' {messageId} -> messageId) (\s@DeleteQueuedMessages' {} a -> s {messageId = a} :: DeleteQueuedMessages)

instance Core.AWSRequest DeleteQueuedMessages where
  type
    AWSResponse DeleteQueuedMessages =
      DeleteQueuedMessagesResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueuedMessagesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQueuedMessages where
  hashWithSalt _salt DeleteQueuedMessages' {..} =
    _salt `Prelude.hashWithSalt` wirelessDeviceType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` messageId

instance Prelude.NFData DeleteQueuedMessages where
  rnf DeleteQueuedMessages' {..} =
    Prelude.rnf wirelessDeviceType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf messageId

instance Data.ToHeaders DeleteQueuedMessages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteQueuedMessages where
  toPath DeleteQueuedMessages' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id, "/data"]

instance Data.ToQuery DeleteQueuedMessages where
  toQuery DeleteQueuedMessages' {..} =
    Prelude.mconcat
      [ "WirelessDeviceType" Data.=: wirelessDeviceType,
        "messageId" Data.=: messageId
      ]

-- | /See:/ 'newDeleteQueuedMessagesResponse' smart constructor.
data DeleteQueuedMessagesResponse = DeleteQueuedMessagesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueuedMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteQueuedMessagesResponse_httpStatus' - The response's http status code.
newDeleteQueuedMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteQueuedMessagesResponse
newDeleteQueuedMessagesResponse pHttpStatus_ =
  DeleteQueuedMessagesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteQueuedMessagesResponse_httpStatus :: Lens.Lens' DeleteQueuedMessagesResponse Prelude.Int
deleteQueuedMessagesResponse_httpStatus = Lens.lens (\DeleteQueuedMessagesResponse' {httpStatus} -> httpStatus) (\s@DeleteQueuedMessagesResponse' {} a -> s {httpStatus = a} :: DeleteQueuedMessagesResponse)

instance Prelude.NFData DeleteQueuedMessagesResponse where
  rnf DeleteQueuedMessagesResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.IoTWireless.SendDataToMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends the specified data to a multicast group.
module Amazonka.IoTWireless.SendDataToMulticastGroup
  ( -- * Creating a Request
    SendDataToMulticastGroup (..),
    newSendDataToMulticastGroup,

    -- * Request Lenses
    sendDataToMulticastGroup_id,
    sendDataToMulticastGroup_payloadData,
    sendDataToMulticastGroup_wirelessMetadata,

    -- * Destructuring the Response
    SendDataToMulticastGroupResponse (..),
    newSendDataToMulticastGroupResponse,

    -- * Response Lenses
    sendDataToMulticastGroupResponse_messageId,
    sendDataToMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendDataToMulticastGroup' smart constructor.
data SendDataToMulticastGroup = SendDataToMulticastGroup'
  { id :: Prelude.Text,
    payloadData :: Prelude.Text,
    wirelessMetadata :: MulticastWirelessMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDataToMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'sendDataToMulticastGroup_id' - Undocumented member.
--
-- 'payloadData', 'sendDataToMulticastGroup_payloadData' - Undocumented member.
--
-- 'wirelessMetadata', 'sendDataToMulticastGroup_wirelessMetadata' - Undocumented member.
newSendDataToMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'payloadData'
  Prelude.Text ->
  -- | 'wirelessMetadata'
  MulticastWirelessMetadata ->
  SendDataToMulticastGroup
newSendDataToMulticastGroup
  pId_
  pPayloadData_
  pWirelessMetadata_ =
    SendDataToMulticastGroup'
      { id = pId_,
        payloadData = pPayloadData_,
        wirelessMetadata = pWirelessMetadata_
      }

-- | Undocumented member.
sendDataToMulticastGroup_id :: Lens.Lens' SendDataToMulticastGroup Prelude.Text
sendDataToMulticastGroup_id = Lens.lens (\SendDataToMulticastGroup' {id} -> id) (\s@SendDataToMulticastGroup' {} a -> s {id = a} :: SendDataToMulticastGroup)

-- | Undocumented member.
sendDataToMulticastGroup_payloadData :: Lens.Lens' SendDataToMulticastGroup Prelude.Text
sendDataToMulticastGroup_payloadData = Lens.lens (\SendDataToMulticastGroup' {payloadData} -> payloadData) (\s@SendDataToMulticastGroup' {} a -> s {payloadData = a} :: SendDataToMulticastGroup)

-- | Undocumented member.
sendDataToMulticastGroup_wirelessMetadata :: Lens.Lens' SendDataToMulticastGroup MulticastWirelessMetadata
sendDataToMulticastGroup_wirelessMetadata = Lens.lens (\SendDataToMulticastGroup' {wirelessMetadata} -> wirelessMetadata) (\s@SendDataToMulticastGroup' {} a -> s {wirelessMetadata = a} :: SendDataToMulticastGroup)

instance Core.AWSRequest SendDataToMulticastGroup where
  type
    AWSResponse SendDataToMulticastGroup =
      SendDataToMulticastGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendDataToMulticastGroupResponse'
            Prelude.<$> (x Data..?> "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendDataToMulticastGroup where
  hashWithSalt _salt SendDataToMulticastGroup' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` payloadData
      `Prelude.hashWithSalt` wirelessMetadata

instance Prelude.NFData SendDataToMulticastGroup where
  rnf SendDataToMulticastGroup' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf payloadData
      `Prelude.seq` Prelude.rnf wirelessMetadata

instance Data.ToHeaders SendDataToMulticastGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SendDataToMulticastGroup where
  toJSON SendDataToMulticastGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PayloadData" Data..= payloadData),
            Prelude.Just
              ("WirelessMetadata" Data..= wirelessMetadata)
          ]
      )

instance Data.ToPath SendDataToMulticastGroup where
  toPath SendDataToMulticastGroup' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id, "/data"]

instance Data.ToQuery SendDataToMulticastGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendDataToMulticastGroupResponse' smart constructor.
data SendDataToMulticastGroupResponse = SendDataToMulticastGroupResponse'
  { messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDataToMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendDataToMulticastGroupResponse_messageId' - Undocumented member.
--
-- 'httpStatus', 'sendDataToMulticastGroupResponse_httpStatus' - The response's http status code.
newSendDataToMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendDataToMulticastGroupResponse
newSendDataToMulticastGroupResponse pHttpStatus_ =
  SendDataToMulticastGroupResponse'
    { messageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
sendDataToMulticastGroupResponse_messageId :: Lens.Lens' SendDataToMulticastGroupResponse (Prelude.Maybe Prelude.Text)
sendDataToMulticastGroupResponse_messageId = Lens.lens (\SendDataToMulticastGroupResponse' {messageId} -> messageId) (\s@SendDataToMulticastGroupResponse' {} a -> s {messageId = a} :: SendDataToMulticastGroupResponse)

-- | The response's http status code.
sendDataToMulticastGroupResponse_httpStatus :: Lens.Lens' SendDataToMulticastGroupResponse Prelude.Int
sendDataToMulticastGroupResponse_httpStatus = Lens.lens (\SendDataToMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@SendDataToMulticastGroupResponse' {} a -> s {httpStatus = a} :: SendDataToMulticastGroupResponse)

instance
  Prelude.NFData
    SendDataToMulticastGroupResponse
  where
  rnf SendDataToMulticastGroupResponse' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf httpStatus

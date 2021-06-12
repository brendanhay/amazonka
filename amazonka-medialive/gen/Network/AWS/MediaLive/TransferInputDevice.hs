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
-- Module      : Network.AWS.MediaLive.TransferInputDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start an input device transfer to another AWS account. After you make
-- the request, the other account must accept or reject the transfer.
module Network.AWS.MediaLive.TransferInputDevice
  ( -- * Creating a Request
    TransferInputDevice' (..),
    newTransferInputDevice',

    -- * Request Lenses
    transferInputDevice'_transferMessage,
    transferInputDevice'_targetCustomerId,
    transferInputDevice'_inputDeviceId,

    -- * Destructuring the Response
    TransferInputDeviceResponse (..),
    newTransferInputDeviceResponse,

    -- * Response Lenses
    transferInputDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to transfer an input device.
--
-- /See:/ 'newTransferInputDevice'' smart constructor.
data TransferInputDevice' = TransferInputDevice''
  { -- | An optional message for the recipient. Maximum 280 characters.
    transferMessage :: Core.Maybe Core.Text,
    -- | The AWS account ID (12 digits) for the recipient of the device transfer.
    targetCustomerId :: Core.Maybe Core.Text,
    -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransferInputDevice'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferMessage', 'transferInputDevice'_transferMessage' - An optional message for the recipient. Maximum 280 characters.
--
-- 'targetCustomerId', 'transferInputDevice'_targetCustomerId' - The AWS account ID (12 digits) for the recipient of the device transfer.
--
-- 'inputDeviceId', 'transferInputDevice'_inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
newTransferInputDevice' ::
  -- | 'inputDeviceId'
  Core.Text ->
  TransferInputDevice'
newTransferInputDevice' pInputDeviceId_ =
  TransferInputDevice''
    { transferMessage =
        Core.Nothing,
      targetCustomerId = Core.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | An optional message for the recipient. Maximum 280 characters.
transferInputDevice'_transferMessage :: Lens.Lens' TransferInputDevice' (Core.Maybe Core.Text)
transferInputDevice'_transferMessage = Lens.lens (\TransferInputDevice'' {transferMessage} -> transferMessage) (\s@TransferInputDevice'' {} a -> s {transferMessage = a} :: TransferInputDevice')

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
transferInputDevice'_targetCustomerId :: Lens.Lens' TransferInputDevice' (Core.Maybe Core.Text)
transferInputDevice'_targetCustomerId = Lens.lens (\TransferInputDevice'' {targetCustomerId} -> targetCustomerId) (\s@TransferInputDevice'' {} a -> s {targetCustomerId = a} :: TransferInputDevice')

-- | The unique ID of this input device. For example, hd-123456789abcdef.
transferInputDevice'_inputDeviceId :: Lens.Lens' TransferInputDevice' Core.Text
transferInputDevice'_inputDeviceId = Lens.lens (\TransferInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@TransferInputDevice'' {} a -> s {inputDeviceId = a} :: TransferInputDevice')

instance Core.AWSRequest TransferInputDevice' where
  type
    AWSResponse TransferInputDevice' =
      TransferInputDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TransferInputDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TransferInputDevice'

instance Core.NFData TransferInputDevice'

instance Core.ToHeaders TransferInputDevice' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TransferInputDevice' where
  toJSON TransferInputDevice'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("transferMessage" Core..=)
              Core.<$> transferMessage,
            ("targetCustomerId" Core..=)
              Core.<$> targetCustomerId
          ]
      )

instance Core.ToPath TransferInputDevice' where
  toPath TransferInputDevice'' {..} =
    Core.mconcat
      [ "/prod/inputDevices/",
        Core.toBS inputDeviceId,
        "/transfer"
      ]

instance Core.ToQuery TransferInputDevice' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'newTransferInputDeviceResponse' smart constructor.
data TransferInputDeviceResponse = TransferInputDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransferInputDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'transferInputDeviceResponse_httpStatus' - The response's http status code.
newTransferInputDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TransferInputDeviceResponse
newTransferInputDeviceResponse pHttpStatus_ =
  TransferInputDeviceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
transferInputDeviceResponse_httpStatus :: Lens.Lens' TransferInputDeviceResponse Core.Int
transferInputDeviceResponse_httpStatus = Lens.lens (\TransferInputDeviceResponse' {httpStatus} -> httpStatus) (\s@TransferInputDeviceResponse' {} a -> s {httpStatus = a} :: TransferInputDeviceResponse)

instance Core.NFData TransferInputDeviceResponse

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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to transfer an input device.
--
-- /See:/ 'newTransferInputDevice'' smart constructor.
data TransferInputDevice' = TransferInputDevice''
  { -- | An optional message for the recipient. Maximum 280 characters.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID (12 digits) for the recipient of the device transfer.
    targetCustomerId :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  TransferInputDevice'
newTransferInputDevice' pInputDeviceId_ =
  TransferInputDevice''
    { transferMessage =
        Prelude.Nothing,
      targetCustomerId = Prelude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | An optional message for the recipient. Maximum 280 characters.
transferInputDevice'_transferMessage :: Lens.Lens' TransferInputDevice' (Prelude.Maybe Prelude.Text)
transferInputDevice'_transferMessage = Lens.lens (\TransferInputDevice'' {transferMessage} -> transferMessage) (\s@TransferInputDevice'' {} a -> s {transferMessage = a} :: TransferInputDevice')

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
transferInputDevice'_targetCustomerId :: Lens.Lens' TransferInputDevice' (Prelude.Maybe Prelude.Text)
transferInputDevice'_targetCustomerId = Lens.lens (\TransferInputDevice'' {targetCustomerId} -> targetCustomerId) (\s@TransferInputDevice'' {} a -> s {targetCustomerId = a} :: TransferInputDevice')

-- | The unique ID of this input device. For example, hd-123456789abcdef.
transferInputDevice'_inputDeviceId :: Lens.Lens' TransferInputDevice' Prelude.Text
transferInputDevice'_inputDeviceId = Lens.lens (\TransferInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@TransferInputDevice'' {} a -> s {inputDeviceId = a} :: TransferInputDevice')

instance Prelude.AWSRequest TransferInputDevice' where
  type
    Rs TransferInputDevice' =
      TransferInputDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TransferInputDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransferInputDevice'

instance Prelude.NFData TransferInputDevice'

instance Prelude.ToHeaders TransferInputDevice' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TransferInputDevice' where
  toJSON TransferInputDevice'' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("transferMessage" Prelude..=)
              Prelude.<$> transferMessage,
            ("targetCustomerId" Prelude..=)
              Prelude.<$> targetCustomerId
          ]
      )

instance Prelude.ToPath TransferInputDevice' where
  toPath TransferInputDevice'' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Prelude.toBS inputDeviceId,
        "/transfer"
      ]

instance Prelude.ToQuery TransferInputDevice' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'newTransferInputDeviceResponse' smart constructor.
data TransferInputDeviceResponse = TransferInputDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  TransferInputDeviceResponse
newTransferInputDeviceResponse pHttpStatus_ =
  TransferInputDeviceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
transferInputDeviceResponse_httpStatus :: Lens.Lens' TransferInputDeviceResponse Prelude.Int
transferInputDeviceResponse_httpStatus = Lens.lens (\TransferInputDeviceResponse' {httpStatus} -> httpStatus) (\s@TransferInputDeviceResponse' {} a -> s {httpStatus = a} :: TransferInputDeviceResponse)

instance Prelude.NFData TransferInputDeviceResponse

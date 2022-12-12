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
-- Module      : Amazonka.MediaLive.TransferInputDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start an input device transfer to another AWS account. After you make
-- the request, the other account must accept or reject the transfer.
module Amazonka.MediaLive.TransferInputDevice
  ( -- * Creating a Request
    TransferInputDevice' (..),
    newTransferInputDevice',

    -- * Request Lenses
    transferInputDevice'_targetCustomerId,
    transferInputDevice'_targetRegion,
    transferInputDevice'_transferMessage,
    transferInputDevice'_inputDeviceId,

    -- * Destructuring the Response
    TransferInputDeviceResponse (..),
    newTransferInputDeviceResponse,

    -- * Response Lenses
    transferInputDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to transfer an input device.
--
-- /See:/ 'newTransferInputDevice'' smart constructor.
data TransferInputDevice' = TransferInputDevice''
  { -- | The AWS account ID (12 digits) for the recipient of the device transfer.
    targetCustomerId :: Prelude.Maybe Prelude.Text,
    -- | The target AWS region to transfer the device.
    targetRegion :: Prelude.Maybe Prelude.Text,
    -- | An optional message for the recipient. Maximum 280 characters.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferInputDevice'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetCustomerId', 'transferInputDevice'_targetCustomerId' - The AWS account ID (12 digits) for the recipient of the device transfer.
--
-- 'targetRegion', 'transferInputDevice'_targetRegion' - The target AWS region to transfer the device.
--
-- 'transferMessage', 'transferInputDevice'_transferMessage' - An optional message for the recipient. Maximum 280 characters.
--
-- 'inputDeviceId', 'transferInputDevice'_inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
newTransferInputDevice' ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  TransferInputDevice'
newTransferInputDevice' pInputDeviceId_ =
  TransferInputDevice''
    { targetCustomerId =
        Prelude.Nothing,
      targetRegion = Prelude.Nothing,
      transferMessage = Prelude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | The AWS account ID (12 digits) for the recipient of the device transfer.
transferInputDevice'_targetCustomerId :: Lens.Lens' TransferInputDevice' (Prelude.Maybe Prelude.Text)
transferInputDevice'_targetCustomerId = Lens.lens (\TransferInputDevice'' {targetCustomerId} -> targetCustomerId) (\s@TransferInputDevice'' {} a -> s {targetCustomerId = a} :: TransferInputDevice')

-- | The target AWS region to transfer the device.
transferInputDevice'_targetRegion :: Lens.Lens' TransferInputDevice' (Prelude.Maybe Prelude.Text)
transferInputDevice'_targetRegion = Lens.lens (\TransferInputDevice'' {targetRegion} -> targetRegion) (\s@TransferInputDevice'' {} a -> s {targetRegion = a} :: TransferInputDevice')

-- | An optional message for the recipient. Maximum 280 characters.
transferInputDevice'_transferMessage :: Lens.Lens' TransferInputDevice' (Prelude.Maybe Prelude.Text)
transferInputDevice'_transferMessage = Lens.lens (\TransferInputDevice'' {transferMessage} -> transferMessage) (\s@TransferInputDevice'' {} a -> s {transferMessage = a} :: TransferInputDevice')

-- | The unique ID of this input device. For example, hd-123456789abcdef.
transferInputDevice'_inputDeviceId :: Lens.Lens' TransferInputDevice' Prelude.Text
transferInputDevice'_inputDeviceId = Lens.lens (\TransferInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@TransferInputDevice'' {} a -> s {inputDeviceId = a} :: TransferInputDevice')

instance Core.AWSRequest TransferInputDevice' where
  type
    AWSResponse TransferInputDevice' =
      TransferInputDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TransferInputDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransferInputDevice' where
  hashWithSalt _salt TransferInputDevice'' {..} =
    _salt `Prelude.hashWithSalt` targetCustomerId
      `Prelude.hashWithSalt` targetRegion
      `Prelude.hashWithSalt` transferMessage
      `Prelude.hashWithSalt` inputDeviceId

instance Prelude.NFData TransferInputDevice' where
  rnf TransferInputDevice'' {..} =
    Prelude.rnf targetCustomerId
      `Prelude.seq` Prelude.rnf targetRegion
      `Prelude.seq` Prelude.rnf transferMessage
      `Prelude.seq` Prelude.rnf inputDeviceId

instance Data.ToHeaders TransferInputDevice' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TransferInputDevice' where
  toJSON TransferInputDevice'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetCustomerId" Data..=)
              Prelude.<$> targetCustomerId,
            ("targetRegion" Data..=) Prelude.<$> targetRegion,
            ("transferMessage" Data..=)
              Prelude.<$> transferMessage
          ]
      )

instance Data.ToPath TransferInputDevice' where
  toPath TransferInputDevice'' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Data.toBS inputDeviceId,
        "/transfer"
      ]

instance Data.ToQuery TransferInputDevice' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for TransferInputDeviceResponse
--
-- /See:/ 'newTransferInputDeviceResponse' smart constructor.
data TransferInputDeviceResponse = TransferInputDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData TransferInputDeviceResponse where
  rnf TransferInputDeviceResponse' {..} =
    Prelude.rnf httpStatus

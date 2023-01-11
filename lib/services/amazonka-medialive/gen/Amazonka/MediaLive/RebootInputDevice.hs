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
-- Module      : Amazonka.MediaLive.RebootInputDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send a reboot command to the specified input device. The device will
-- begin rebooting within a few seconds of sending the command. When the
-- reboot is complete, the device’s connection status will change to
-- connected.
module Amazonka.MediaLive.RebootInputDevice
  ( -- * Creating a Request
    RebootInputDevice' (..),
    newRebootInputDevice',

    -- * Request Lenses
    rebootInputDevice'_force,
    rebootInputDevice'_inputDeviceId,

    -- * Destructuring the Response
    RebootInputDeviceResponse (..),
    newRebootInputDeviceResponse,

    -- * Response Lenses
    rebootInputDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to reboot an AWS Elemental device.
--
-- /See:/ 'newRebootInputDevice'' smart constructor.
data RebootInputDevice' = RebootInputDevice''
  { -- | Force a reboot of an input device. If the device is streaming, it will
    -- stop streaming and begin rebooting within a few seconds of sending the
    -- command. If the device was streaming prior to the reboot, the device
    -- will resume streaming when the reboot completes.
    force :: Prelude.Maybe RebootInputDeviceForce,
    -- | The unique ID of the input device to reboot. For example,
    -- hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInputDevice'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'rebootInputDevice'_force' - Force a reboot of an input device. If the device is streaming, it will
-- stop streaming and begin rebooting within a few seconds of sending the
-- command. If the device was streaming prior to the reboot, the device
-- will resume streaming when the reboot completes.
--
-- 'inputDeviceId', 'rebootInputDevice'_inputDeviceId' - The unique ID of the input device to reboot. For example,
-- hd-123456789abcdef.
newRebootInputDevice' ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  RebootInputDevice'
newRebootInputDevice' pInputDeviceId_ =
  RebootInputDevice''
    { force = Prelude.Nothing,
      inputDeviceId = pInputDeviceId_
    }

-- | Force a reboot of an input device. If the device is streaming, it will
-- stop streaming and begin rebooting within a few seconds of sending the
-- command. If the device was streaming prior to the reboot, the device
-- will resume streaming when the reboot completes.
rebootInputDevice'_force :: Lens.Lens' RebootInputDevice' (Prelude.Maybe RebootInputDeviceForce)
rebootInputDevice'_force = Lens.lens (\RebootInputDevice'' {force} -> force) (\s@RebootInputDevice'' {} a -> s {force = a} :: RebootInputDevice')

-- | The unique ID of the input device to reboot. For example,
-- hd-123456789abcdef.
rebootInputDevice'_inputDeviceId :: Lens.Lens' RebootInputDevice' Prelude.Text
rebootInputDevice'_inputDeviceId = Lens.lens (\RebootInputDevice'' {inputDeviceId} -> inputDeviceId) (\s@RebootInputDevice'' {} a -> s {inputDeviceId = a} :: RebootInputDevice')

instance Core.AWSRequest RebootInputDevice' where
  type
    AWSResponse RebootInputDevice' =
      RebootInputDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RebootInputDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootInputDevice' where
  hashWithSalt _salt RebootInputDevice'' {..} =
    _salt `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` inputDeviceId

instance Prelude.NFData RebootInputDevice' where
  rnf RebootInputDevice'' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf inputDeviceId

instance Data.ToHeaders RebootInputDevice' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebootInputDevice' where
  toJSON RebootInputDevice'' {..} =
    Data.object
      ( Prelude.catMaybes
          [("force" Data..=) Prelude.<$> force]
      )

instance Data.ToPath RebootInputDevice' where
  toPath RebootInputDevice'' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Data.toBS inputDeviceId,
        "/reboot"
      ]

instance Data.ToQuery RebootInputDevice' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for RebootInputDeviceResponse
--
-- /See:/ 'newRebootInputDeviceResponse' smart constructor.
data RebootInputDeviceResponse = RebootInputDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootInputDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rebootInputDeviceResponse_httpStatus' - The response's http status code.
newRebootInputDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootInputDeviceResponse
newRebootInputDeviceResponse pHttpStatus_ =
  RebootInputDeviceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rebootInputDeviceResponse_httpStatus :: Lens.Lens' RebootInputDeviceResponse Prelude.Int
rebootInputDeviceResponse_httpStatus = Lens.lens (\RebootInputDeviceResponse' {httpStatus} -> httpStatus) (\s@RebootInputDeviceResponse' {} a -> s {httpStatus = a} :: RebootInputDeviceResponse)

instance Prelude.NFData RebootInputDeviceResponse where
  rnf RebootInputDeviceResponse' {..} =
    Prelude.rnf httpStatus

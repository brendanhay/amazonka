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
-- Module      : Amazonka.MediaLive.StartInputDeviceMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a maintenance window for the specified input device. Starting a
-- maintenance window will give the device up to two hours to install
-- software. If the device was streaming prior to the maintenance, it will
-- resume streaming when the software is fully installed. Devices
-- automatically install updates while they are powered on and their
-- MediaLive channels are stopped. A maintenance window allows you to
-- update a device without having to stop MediaLive channels that use the
-- device. The device must remain powered on and connected to the internet
-- for the duration of the maintenance.
module Amazonka.MediaLive.StartInputDeviceMaintenanceWindow
  ( -- * Creating a Request
    StartInputDeviceMaintenanceWindow (..),
    newStartInputDeviceMaintenanceWindow,

    -- * Request Lenses
    startInputDeviceMaintenanceWindow_inputDeviceId,

    -- * Destructuring the Response
    StartInputDeviceMaintenanceWindowResponse (..),
    newStartInputDeviceMaintenanceWindowResponse,

    -- * Response Lenses
    startInputDeviceMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for StartInputDeviceMaintenanceWindowRequest
--
-- /See:/ 'newStartInputDeviceMaintenanceWindow' smart constructor.
data StartInputDeviceMaintenanceWindow = StartInputDeviceMaintenanceWindow'
  { -- | The unique ID of the input device to start a maintenance window for. For
    -- example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInputDeviceMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'startInputDeviceMaintenanceWindow_inputDeviceId' - The unique ID of the input device to start a maintenance window for. For
-- example, hd-123456789abcdef.
newStartInputDeviceMaintenanceWindow ::
  -- | 'inputDeviceId'
  Prelude.Text ->
  StartInputDeviceMaintenanceWindow
newStartInputDeviceMaintenanceWindow pInputDeviceId_ =
  StartInputDeviceMaintenanceWindow'
    { inputDeviceId =
        pInputDeviceId_
    }

-- | The unique ID of the input device to start a maintenance window for. For
-- example, hd-123456789abcdef.
startInputDeviceMaintenanceWindow_inputDeviceId :: Lens.Lens' StartInputDeviceMaintenanceWindow Prelude.Text
startInputDeviceMaintenanceWindow_inputDeviceId = Lens.lens (\StartInputDeviceMaintenanceWindow' {inputDeviceId} -> inputDeviceId) (\s@StartInputDeviceMaintenanceWindow' {} a -> s {inputDeviceId = a} :: StartInputDeviceMaintenanceWindow)

instance
  Core.AWSRequest
    StartInputDeviceMaintenanceWindow
  where
  type
    AWSResponse StartInputDeviceMaintenanceWindow =
      StartInputDeviceMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartInputDeviceMaintenanceWindowResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartInputDeviceMaintenanceWindow
  where
  hashWithSalt
    _salt
    StartInputDeviceMaintenanceWindow' {..} =
      _salt `Prelude.hashWithSalt` inputDeviceId

instance
  Prelude.NFData
    StartInputDeviceMaintenanceWindow
  where
  rnf StartInputDeviceMaintenanceWindow' {..} =
    Prelude.rnf inputDeviceId

instance
  Data.ToHeaders
    StartInputDeviceMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    StartInputDeviceMaintenanceWindow
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    StartInputDeviceMaintenanceWindow
  where
  toPath StartInputDeviceMaintenanceWindow' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Data.toBS inputDeviceId,
        "/startInputDeviceMaintenanceWindow"
      ]

instance
  Data.ToQuery
    StartInputDeviceMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for StartInputDeviceMaintenanceWindowResponse
--
-- /See:/ 'newStartInputDeviceMaintenanceWindowResponse' smart constructor.
data StartInputDeviceMaintenanceWindowResponse = StartInputDeviceMaintenanceWindowResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInputDeviceMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startInputDeviceMaintenanceWindowResponse_httpStatus' - The response's http status code.
newStartInputDeviceMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInputDeviceMaintenanceWindowResponse
newStartInputDeviceMaintenanceWindowResponse
  pHttpStatus_ =
    StartInputDeviceMaintenanceWindowResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
startInputDeviceMaintenanceWindowResponse_httpStatus :: Lens.Lens' StartInputDeviceMaintenanceWindowResponse Prelude.Int
startInputDeviceMaintenanceWindowResponse_httpStatus = Lens.lens (\StartInputDeviceMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@StartInputDeviceMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: StartInputDeviceMaintenanceWindowResponse)

instance
  Prelude.NFData
    StartInputDeviceMaintenanceWindowResponse
  where
  rnf StartInputDeviceMaintenanceWindowResponse' {..} =
    Prelude.rnf httpStatus

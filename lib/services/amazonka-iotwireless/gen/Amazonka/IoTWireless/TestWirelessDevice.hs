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
-- Module      : Amazonka.IoTWireless.TestWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates a provisioned device by sending an uplink data payload of
-- @Hello@.
module Amazonka.IoTWireless.TestWirelessDevice
  ( -- * Creating a Request
    TestWirelessDevice (..),
    newTestWirelessDevice,

    -- * Request Lenses
    testWirelessDevice_id,

    -- * Destructuring the Response
    TestWirelessDeviceResponse (..),
    newTestWirelessDeviceResponse,

    -- * Response Lenses
    testWirelessDeviceResponse_result,
    testWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestWirelessDevice' smart constructor.
data TestWirelessDevice = TestWirelessDevice'
  { -- | The ID of the wireless device to test.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'testWirelessDevice_id' - The ID of the wireless device to test.
newTestWirelessDevice ::
  -- | 'id'
  Prelude.Text ->
  TestWirelessDevice
newTestWirelessDevice pId_ =
  TestWirelessDevice' {id = pId_}

-- | The ID of the wireless device to test.
testWirelessDevice_id :: Lens.Lens' TestWirelessDevice Prelude.Text
testWirelessDevice_id = Lens.lens (\TestWirelessDevice' {id} -> id) (\s@TestWirelessDevice' {} a -> s {id = a} :: TestWirelessDevice)

instance Core.AWSRequest TestWirelessDevice where
  type
    AWSResponse TestWirelessDevice =
      TestWirelessDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestWirelessDeviceResponse'
            Prelude.<$> (x Data..?> "Result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestWirelessDevice where
  hashWithSalt _salt TestWirelessDevice' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData TestWirelessDevice where
  rnf TestWirelessDevice' {..} = Prelude.rnf id

instance Data.ToHeaders TestWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON TestWirelessDevice where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath TestWirelessDevice where
  toPath TestWirelessDevice' {..} =
    Prelude.mconcat
      ["/wireless-devices/", Data.toBS id, "/test"]

instance Data.ToQuery TestWirelessDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestWirelessDeviceResponse' smart constructor.
data TestWirelessDeviceResponse = TestWirelessDeviceResponse'
  { -- | The result returned by the test.
    result :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'testWirelessDeviceResponse_result' - The result returned by the test.
--
-- 'httpStatus', 'testWirelessDeviceResponse_httpStatus' - The response's http status code.
newTestWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestWirelessDeviceResponse
newTestWirelessDeviceResponse pHttpStatus_ =
  TestWirelessDeviceResponse'
    { result =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The result returned by the test.
testWirelessDeviceResponse_result :: Lens.Lens' TestWirelessDeviceResponse (Prelude.Maybe Prelude.Text)
testWirelessDeviceResponse_result = Lens.lens (\TestWirelessDeviceResponse' {result} -> result) (\s@TestWirelessDeviceResponse' {} a -> s {result = a} :: TestWirelessDeviceResponse)

-- | The response's http status code.
testWirelessDeviceResponse_httpStatus :: Lens.Lens' TestWirelessDeviceResponse Prelude.Int
testWirelessDeviceResponse_httpStatus = Lens.lens (\TestWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@TestWirelessDeviceResponse' {} a -> s {httpStatus = a} :: TestWirelessDeviceResponse)

instance Prelude.NFData TestWirelessDeviceResponse where
  rnf TestWirelessDeviceResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus

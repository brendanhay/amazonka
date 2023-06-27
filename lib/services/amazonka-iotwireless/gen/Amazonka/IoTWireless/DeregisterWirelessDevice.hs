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
-- Module      : Amazonka.IoTWireless.DeregisterWirelessDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregister a wireless device from AWS IoT Wireless.
module Amazonka.IoTWireless.DeregisterWirelessDevice
  ( -- * Creating a Request
    DeregisterWirelessDevice (..),
    newDeregisterWirelessDevice,

    -- * Request Lenses
    deregisterWirelessDevice_wirelessDeviceType,
    deregisterWirelessDevice_identifier,

    -- * Destructuring the Response
    DeregisterWirelessDeviceResponse (..),
    newDeregisterWirelessDeviceResponse,

    -- * Response Lenses
    deregisterWirelessDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterWirelessDevice' smart constructor.
data DeregisterWirelessDevice = DeregisterWirelessDevice'
  { -- | The type of wireless device to deregister from AWS IoT Wireless, which
    -- can be @LoRaWAN@ or @Sidewalk@.
    wirelessDeviceType :: Prelude.Maybe WirelessDeviceType,
    -- | The identifier of the wireless device to deregister from AWS IoT
    -- Wireless.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterWirelessDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessDeviceType', 'deregisterWirelessDevice_wirelessDeviceType' - The type of wireless device to deregister from AWS IoT Wireless, which
-- can be @LoRaWAN@ or @Sidewalk@.
--
-- 'identifier', 'deregisterWirelessDevice_identifier' - The identifier of the wireless device to deregister from AWS IoT
-- Wireless.
newDeregisterWirelessDevice ::
  -- | 'identifier'
  Prelude.Text ->
  DeregisterWirelessDevice
newDeregisterWirelessDevice pIdentifier_ =
  DeregisterWirelessDevice'
    { wirelessDeviceType =
        Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The type of wireless device to deregister from AWS IoT Wireless, which
-- can be @LoRaWAN@ or @Sidewalk@.
deregisterWirelessDevice_wirelessDeviceType :: Lens.Lens' DeregisterWirelessDevice (Prelude.Maybe WirelessDeviceType)
deregisterWirelessDevice_wirelessDeviceType = Lens.lens (\DeregisterWirelessDevice' {wirelessDeviceType} -> wirelessDeviceType) (\s@DeregisterWirelessDevice' {} a -> s {wirelessDeviceType = a} :: DeregisterWirelessDevice)

-- | The identifier of the wireless device to deregister from AWS IoT
-- Wireless.
deregisterWirelessDevice_identifier :: Lens.Lens' DeregisterWirelessDevice Prelude.Text
deregisterWirelessDevice_identifier = Lens.lens (\DeregisterWirelessDevice' {identifier} -> identifier) (\s@DeregisterWirelessDevice' {} a -> s {identifier = a} :: DeregisterWirelessDevice)

instance Core.AWSRequest DeregisterWirelessDevice where
  type
    AWSResponse DeregisterWirelessDevice =
      DeregisterWirelessDeviceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWirelessDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterWirelessDevice where
  hashWithSalt _salt DeregisterWirelessDevice' {..} =
    _salt
      `Prelude.hashWithSalt` wirelessDeviceType
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeregisterWirelessDevice where
  rnf DeregisterWirelessDevice' {..} =
    Prelude.rnf wirelessDeviceType
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders DeregisterWirelessDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DeregisterWirelessDevice where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeregisterWirelessDevice where
  toPath DeregisterWirelessDevice' {..} =
    Prelude.mconcat
      [ "/wireless-devices/",
        Data.toBS identifier,
        "/deregister"
      ]

instance Data.ToQuery DeregisterWirelessDevice where
  toQuery DeregisterWirelessDevice' {..} =
    Prelude.mconcat
      ["WirelessDeviceType" Data.=: wirelessDeviceType]

-- | /See:/ 'newDeregisterWirelessDeviceResponse' smart constructor.
data DeregisterWirelessDeviceResponse = DeregisterWirelessDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterWirelessDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterWirelessDeviceResponse_httpStatus' - The response's http status code.
newDeregisterWirelessDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterWirelessDeviceResponse
newDeregisterWirelessDeviceResponse pHttpStatus_ =
  DeregisterWirelessDeviceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterWirelessDeviceResponse_httpStatus :: Lens.Lens' DeregisterWirelessDeviceResponse Prelude.Int
deregisterWirelessDeviceResponse_httpStatus = Lens.lens (\DeregisterWirelessDeviceResponse' {httpStatus} -> httpStatus) (\s@DeregisterWirelessDeviceResponse' {} a -> s {httpStatus = a} :: DeregisterWirelessDeviceResponse)

instance
  Prelude.NFData
    DeregisterWirelessDeviceResponse
  where
  rnf DeregisterWirelessDeviceResponse' {..} =
    Prelude.rnf httpStatus

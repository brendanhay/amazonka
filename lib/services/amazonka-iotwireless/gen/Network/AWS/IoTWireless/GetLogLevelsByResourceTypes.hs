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
-- Module      : Network.AWS.IoTWireless.GetLogLevelsByResourceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current default log levels or log levels by resource types.
-- Based on resource types, log levels can be for wireless device log
-- options or wireless gateway log options.
module Network.AWS.IoTWireless.GetLogLevelsByResourceTypes
  ( -- * Creating a Request
    GetLogLevelsByResourceTypes (..),
    newGetLogLevelsByResourceTypes,

    -- * Destructuring the Response
    GetLogLevelsByResourceTypesResponse (..),
    newGetLogLevelsByResourceTypesResponse,

    -- * Response Lenses
    getLogLevelsByResourceTypesResponse_defaultLogLevel,
    getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions,
    getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions,
    getLogLevelsByResourceTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLogLevelsByResourceTypes' smart constructor.
data GetLogLevelsByResourceTypes = GetLogLevelsByResourceTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLogLevelsByResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetLogLevelsByResourceTypes ::
  GetLogLevelsByResourceTypes
newGetLogLevelsByResourceTypes =
  GetLogLevelsByResourceTypes'

instance Core.AWSRequest GetLogLevelsByResourceTypes where
  type
    AWSResponse GetLogLevelsByResourceTypes =
      GetLogLevelsByResourceTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLogLevelsByResourceTypesResponse'
            Prelude.<$> (x Core..?> "DefaultLogLevel")
            Prelude.<*> ( x Core..?> "WirelessGatewayLogOptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "WirelessDeviceLogOptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLogLevelsByResourceTypes

instance Prelude.NFData GetLogLevelsByResourceTypes

instance Core.ToHeaders GetLogLevelsByResourceTypes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetLogLevelsByResourceTypes where
  toPath = Prelude.const "/log-levels"

instance Core.ToQuery GetLogLevelsByResourceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLogLevelsByResourceTypesResponse' smart constructor.
data GetLogLevelsByResourceTypesResponse = GetLogLevelsByResourceTypesResponse'
  { defaultLogLevel :: Prelude.Maybe LogLevel,
    wirelessGatewayLogOptions :: Prelude.Maybe [WirelessGatewayLogOption],
    wirelessDeviceLogOptions :: Prelude.Maybe [WirelessDeviceLogOption],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLogLevelsByResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultLogLevel', 'getLogLevelsByResourceTypesResponse_defaultLogLevel' - Undocumented member.
--
-- 'wirelessGatewayLogOptions', 'getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions' - Undocumented member.
--
-- 'wirelessDeviceLogOptions', 'getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions' - Undocumented member.
--
-- 'httpStatus', 'getLogLevelsByResourceTypesResponse_httpStatus' - The response's http status code.
newGetLogLevelsByResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLogLevelsByResourceTypesResponse
newGetLogLevelsByResourceTypesResponse pHttpStatus_ =
  GetLogLevelsByResourceTypesResponse'
    { defaultLogLevel =
        Prelude.Nothing,
      wirelessGatewayLogOptions =
        Prelude.Nothing,
      wirelessDeviceLogOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getLogLevelsByResourceTypesResponse_defaultLogLevel :: Lens.Lens' GetLogLevelsByResourceTypesResponse (Prelude.Maybe LogLevel)
getLogLevelsByResourceTypesResponse_defaultLogLevel = Lens.lens (\GetLogLevelsByResourceTypesResponse' {defaultLogLevel} -> defaultLogLevel) (\s@GetLogLevelsByResourceTypesResponse' {} a -> s {defaultLogLevel = a} :: GetLogLevelsByResourceTypesResponse)

-- | Undocumented member.
getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions :: Lens.Lens' GetLogLevelsByResourceTypesResponse (Prelude.Maybe [WirelessGatewayLogOption])
getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions = Lens.lens (\GetLogLevelsByResourceTypesResponse' {wirelessGatewayLogOptions} -> wirelessGatewayLogOptions) (\s@GetLogLevelsByResourceTypesResponse' {} a -> s {wirelessGatewayLogOptions = a} :: GetLogLevelsByResourceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions :: Lens.Lens' GetLogLevelsByResourceTypesResponse (Prelude.Maybe [WirelessDeviceLogOption])
getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions = Lens.lens (\GetLogLevelsByResourceTypesResponse' {wirelessDeviceLogOptions} -> wirelessDeviceLogOptions) (\s@GetLogLevelsByResourceTypesResponse' {} a -> s {wirelessDeviceLogOptions = a} :: GetLogLevelsByResourceTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLogLevelsByResourceTypesResponse_httpStatus :: Lens.Lens' GetLogLevelsByResourceTypesResponse Prelude.Int
getLogLevelsByResourceTypesResponse_httpStatus = Lens.lens (\GetLogLevelsByResourceTypesResponse' {httpStatus} -> httpStatus) (\s@GetLogLevelsByResourceTypesResponse' {} a -> s {httpStatus = a} :: GetLogLevelsByResourceTypesResponse)

instance
  Prelude.NFData
    GetLogLevelsByResourceTypesResponse

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
-- Module      : Amazonka.IoTWireless.UpdateLogLevelsByResourceTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set default log level, or log levels by resource types. This can be for
-- wireless device log options or wireless gateways log options and is used
-- to control the log messages that\'ll be displayed in CloudWatch.
module Amazonka.IoTWireless.UpdateLogLevelsByResourceTypes
  ( -- * Creating a Request
    UpdateLogLevelsByResourceTypes (..),
    newUpdateLogLevelsByResourceTypes,

    -- * Request Lenses
    updateLogLevelsByResourceTypes_defaultLogLevel,
    updateLogLevelsByResourceTypes_wirelessGatewayLogOptions,
    updateLogLevelsByResourceTypes_wirelessDeviceLogOptions,

    -- * Destructuring the Response
    UpdateLogLevelsByResourceTypesResponse (..),
    newUpdateLogLevelsByResourceTypesResponse,

    -- * Response Lenses
    updateLogLevelsByResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLogLevelsByResourceTypes' smart constructor.
data UpdateLogLevelsByResourceTypes = UpdateLogLevelsByResourceTypes'
  { defaultLogLevel :: Prelude.Maybe LogLevel,
    wirelessGatewayLogOptions :: Prelude.Maybe [WirelessGatewayLogOption],
    wirelessDeviceLogOptions :: Prelude.Maybe [WirelessDeviceLogOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLogLevelsByResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultLogLevel', 'updateLogLevelsByResourceTypes_defaultLogLevel' - Undocumented member.
--
-- 'wirelessGatewayLogOptions', 'updateLogLevelsByResourceTypes_wirelessGatewayLogOptions' - Undocumented member.
--
-- 'wirelessDeviceLogOptions', 'updateLogLevelsByResourceTypes_wirelessDeviceLogOptions' - Undocumented member.
newUpdateLogLevelsByResourceTypes ::
  UpdateLogLevelsByResourceTypes
newUpdateLogLevelsByResourceTypes =
  UpdateLogLevelsByResourceTypes'
    { defaultLogLevel =
        Prelude.Nothing,
      wirelessGatewayLogOptions = Prelude.Nothing,
      wirelessDeviceLogOptions = Prelude.Nothing
    }

-- | Undocumented member.
updateLogLevelsByResourceTypes_defaultLogLevel :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe LogLevel)
updateLogLevelsByResourceTypes_defaultLogLevel = Lens.lens (\UpdateLogLevelsByResourceTypes' {defaultLogLevel} -> defaultLogLevel) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {defaultLogLevel = a} :: UpdateLogLevelsByResourceTypes)

-- | Undocumented member.
updateLogLevelsByResourceTypes_wirelessGatewayLogOptions :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe [WirelessGatewayLogOption])
updateLogLevelsByResourceTypes_wirelessGatewayLogOptions = Lens.lens (\UpdateLogLevelsByResourceTypes' {wirelessGatewayLogOptions} -> wirelessGatewayLogOptions) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {wirelessGatewayLogOptions = a} :: UpdateLogLevelsByResourceTypes) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateLogLevelsByResourceTypes_wirelessDeviceLogOptions :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe [WirelessDeviceLogOption])
updateLogLevelsByResourceTypes_wirelessDeviceLogOptions = Lens.lens (\UpdateLogLevelsByResourceTypes' {wirelessDeviceLogOptions} -> wirelessDeviceLogOptions) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {wirelessDeviceLogOptions = a} :: UpdateLogLevelsByResourceTypes) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateLogLevelsByResourceTypes
  where
  type
    AWSResponse UpdateLogLevelsByResourceTypes =
      UpdateLogLevelsByResourceTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLogLevelsByResourceTypesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateLogLevelsByResourceTypes

instance
  Prelude.NFData
    UpdateLogLevelsByResourceTypes

instance
  Core.ToHeaders
    UpdateLogLevelsByResourceTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateLogLevelsByResourceTypes where
  toJSON UpdateLogLevelsByResourceTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultLogLevel" Core..=)
              Prelude.<$> defaultLogLevel,
            ("WirelessGatewayLogOptions" Core..=)
              Prelude.<$> wirelessGatewayLogOptions,
            ("WirelessDeviceLogOptions" Core..=)
              Prelude.<$> wirelessDeviceLogOptions
          ]
      )

instance Core.ToPath UpdateLogLevelsByResourceTypes where
  toPath = Prelude.const "/log-levels"

instance Core.ToQuery UpdateLogLevelsByResourceTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLogLevelsByResourceTypesResponse' smart constructor.
data UpdateLogLevelsByResourceTypesResponse = UpdateLogLevelsByResourceTypesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLogLevelsByResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLogLevelsByResourceTypesResponse_httpStatus' - The response's http status code.
newUpdateLogLevelsByResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLogLevelsByResourceTypesResponse
newUpdateLogLevelsByResourceTypesResponse
  pHttpStatus_ =
    UpdateLogLevelsByResourceTypesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateLogLevelsByResourceTypesResponse_httpStatus :: Lens.Lens' UpdateLogLevelsByResourceTypesResponse Prelude.Int
updateLogLevelsByResourceTypesResponse_httpStatus = Lens.lens (\UpdateLogLevelsByResourceTypesResponse' {httpStatus} -> httpStatus) (\s@UpdateLogLevelsByResourceTypesResponse' {} a -> s {httpStatus = a} :: UpdateLogLevelsByResourceTypesResponse)

instance
  Prelude.NFData
    UpdateLogLevelsByResourceTypesResponse

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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateLogLevelsByResourceTypes_wirelessDeviceLogOptions,
    updateLogLevelsByResourceTypes_wirelessGatewayLogOptions,

    -- * Destructuring the Response
    UpdateLogLevelsByResourceTypesResponse (..),
    newUpdateLogLevelsByResourceTypesResponse,

    -- * Response Lenses
    updateLogLevelsByResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLogLevelsByResourceTypes' smart constructor.
data UpdateLogLevelsByResourceTypes = UpdateLogLevelsByResourceTypes'
  { defaultLogLevel :: Prelude.Maybe LogLevel,
    wirelessDeviceLogOptions :: Prelude.Maybe [WirelessDeviceLogOption],
    wirelessGatewayLogOptions :: Prelude.Maybe [WirelessGatewayLogOption]
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
-- 'wirelessDeviceLogOptions', 'updateLogLevelsByResourceTypes_wirelessDeviceLogOptions' - Undocumented member.
--
-- 'wirelessGatewayLogOptions', 'updateLogLevelsByResourceTypes_wirelessGatewayLogOptions' - Undocumented member.
newUpdateLogLevelsByResourceTypes ::
  UpdateLogLevelsByResourceTypes
newUpdateLogLevelsByResourceTypes =
  UpdateLogLevelsByResourceTypes'
    { defaultLogLevel =
        Prelude.Nothing,
      wirelessDeviceLogOptions = Prelude.Nothing,
      wirelessGatewayLogOptions = Prelude.Nothing
    }

-- | Undocumented member.
updateLogLevelsByResourceTypes_defaultLogLevel :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe LogLevel)
updateLogLevelsByResourceTypes_defaultLogLevel = Lens.lens (\UpdateLogLevelsByResourceTypes' {defaultLogLevel} -> defaultLogLevel) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {defaultLogLevel = a} :: UpdateLogLevelsByResourceTypes)

-- | Undocumented member.
updateLogLevelsByResourceTypes_wirelessDeviceLogOptions :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe [WirelessDeviceLogOption])
updateLogLevelsByResourceTypes_wirelessDeviceLogOptions = Lens.lens (\UpdateLogLevelsByResourceTypes' {wirelessDeviceLogOptions} -> wirelessDeviceLogOptions) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {wirelessDeviceLogOptions = a} :: UpdateLogLevelsByResourceTypes) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateLogLevelsByResourceTypes_wirelessGatewayLogOptions :: Lens.Lens' UpdateLogLevelsByResourceTypes (Prelude.Maybe [WirelessGatewayLogOption])
updateLogLevelsByResourceTypes_wirelessGatewayLogOptions = Lens.lens (\UpdateLogLevelsByResourceTypes' {wirelessGatewayLogOptions} -> wirelessGatewayLogOptions) (\s@UpdateLogLevelsByResourceTypes' {} a -> s {wirelessGatewayLogOptions = a} :: UpdateLogLevelsByResourceTypes) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateLogLevelsByResourceTypes
  where
  type
    AWSResponse UpdateLogLevelsByResourceTypes =
      UpdateLogLevelsByResourceTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLogLevelsByResourceTypesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateLogLevelsByResourceTypes
  where
  hashWithSalt
    _salt
    UpdateLogLevelsByResourceTypes' {..} =
      _salt `Prelude.hashWithSalt` defaultLogLevel
        `Prelude.hashWithSalt` wirelessDeviceLogOptions
        `Prelude.hashWithSalt` wirelessGatewayLogOptions

instance
  Prelude.NFData
    UpdateLogLevelsByResourceTypes
  where
  rnf UpdateLogLevelsByResourceTypes' {..} =
    Prelude.rnf defaultLogLevel
      `Prelude.seq` Prelude.rnf wirelessDeviceLogOptions
      `Prelude.seq` Prelude.rnf wirelessGatewayLogOptions

instance
  Data.ToHeaders
    UpdateLogLevelsByResourceTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateLogLevelsByResourceTypes where
  toJSON UpdateLogLevelsByResourceTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultLogLevel" Data..=)
              Prelude.<$> defaultLogLevel,
            ("WirelessDeviceLogOptions" Data..=)
              Prelude.<$> wirelessDeviceLogOptions,
            ("WirelessGatewayLogOptions" Data..=)
              Prelude.<$> wirelessGatewayLogOptions
          ]
      )

instance Data.ToPath UpdateLogLevelsByResourceTypes where
  toPath = Prelude.const "/log-levels"

instance Data.ToQuery UpdateLogLevelsByResourceTypes where
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
  where
  rnf UpdateLogLevelsByResourceTypesResponse' {..} =
    Prelude.rnf httpStatus

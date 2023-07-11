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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.GetServiceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Linux subscriptions service settings.
module Amazonka.LicenseManagerLinuxSubscriptions.GetServiceSettings
  ( -- * Creating a Request
    GetServiceSettings (..),
    newGetServiceSettings,

    -- * Destructuring the Response
    GetServiceSettingsResponse (..),
    newGetServiceSettingsResponse,

    -- * Response Lenses
    getServiceSettingsResponse_homeRegions,
    getServiceSettingsResponse_linuxSubscriptionsDiscovery,
    getServiceSettingsResponse_linuxSubscriptionsDiscoverySettings,
    getServiceSettingsResponse_status,
    getServiceSettingsResponse_statusMessage,
    getServiceSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceSettings' smart constructor.
data GetServiceSettings = GetServiceSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetServiceSettings ::
  GetServiceSettings
newGetServiceSettings = GetServiceSettings'

instance Core.AWSRequest GetServiceSettings where
  type
    AWSResponse GetServiceSettings =
      GetServiceSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSettingsResponse'
            Prelude.<$> (x Data..?> "HomeRegions")
            Prelude.<*> (x Data..?> "LinuxSubscriptionsDiscovery")
            Prelude.<*> (x Data..?> "LinuxSubscriptionsDiscoverySettings")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetServiceSettings where
  rnf _ = ()

instance Data.ToHeaders GetServiceSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetServiceSettings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetServiceSettings where
  toPath =
    Prelude.const "/subscription/GetServiceSettings"

instance Data.ToQuery GetServiceSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceSettingsResponse' smart constructor.
data GetServiceSettingsResponse = GetServiceSettingsResponse'
  { -- | The Region in which License Manager displays the aggregated data for
    -- Linux subscriptions.
    homeRegions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Lists if discovery has been enabled for Linux subscriptions.
    linuxSubscriptionsDiscovery :: Prelude.Maybe LinuxSubscriptionsDiscovery,
    -- | Lists the settings defined for Linux subscriptions discovery. The
    -- settings include if Organizations integration has been enabled, and
    -- which Regions data will be aggregated from.
    linuxSubscriptionsDiscoverySettings :: Prelude.Maybe LinuxSubscriptionsDiscoverySettings,
    -- | Indicates the status of Linux subscriptions settings being applied.
    status :: Prelude.Maybe Status,
    -- | A message which details the Linux subscriptions service settings current
    -- status.
    statusMessage :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeRegions', 'getServiceSettingsResponse_homeRegions' - The Region in which License Manager displays the aggregated data for
-- Linux subscriptions.
--
-- 'linuxSubscriptionsDiscovery', 'getServiceSettingsResponse_linuxSubscriptionsDiscovery' - Lists if discovery has been enabled for Linux subscriptions.
--
-- 'linuxSubscriptionsDiscoverySettings', 'getServiceSettingsResponse_linuxSubscriptionsDiscoverySettings' - Lists the settings defined for Linux subscriptions discovery. The
-- settings include if Organizations integration has been enabled, and
-- which Regions data will be aggregated from.
--
-- 'status', 'getServiceSettingsResponse_status' - Indicates the status of Linux subscriptions settings being applied.
--
-- 'statusMessage', 'getServiceSettingsResponse_statusMessage' - A message which details the Linux subscriptions service settings current
-- status.
--
-- 'httpStatus', 'getServiceSettingsResponse_httpStatus' - The response's http status code.
newGetServiceSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceSettingsResponse
newGetServiceSettingsResponse pHttpStatus_ =
  GetServiceSettingsResponse'
    { homeRegions =
        Prelude.Nothing,
      linuxSubscriptionsDiscovery = Prelude.Nothing,
      linuxSubscriptionsDiscoverySettings =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Region in which License Manager displays the aggregated data for
-- Linux subscriptions.
getServiceSettingsResponse_homeRegions :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getServiceSettingsResponse_homeRegions = Lens.lens (\GetServiceSettingsResponse' {homeRegions} -> homeRegions) (\s@GetServiceSettingsResponse' {} a -> s {homeRegions = a} :: GetServiceSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Lists if discovery has been enabled for Linux subscriptions.
getServiceSettingsResponse_linuxSubscriptionsDiscovery :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe LinuxSubscriptionsDiscovery)
getServiceSettingsResponse_linuxSubscriptionsDiscovery = Lens.lens (\GetServiceSettingsResponse' {linuxSubscriptionsDiscovery} -> linuxSubscriptionsDiscovery) (\s@GetServiceSettingsResponse' {} a -> s {linuxSubscriptionsDiscovery = a} :: GetServiceSettingsResponse)

-- | Lists the settings defined for Linux subscriptions discovery. The
-- settings include if Organizations integration has been enabled, and
-- which Regions data will be aggregated from.
getServiceSettingsResponse_linuxSubscriptionsDiscoverySettings :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe LinuxSubscriptionsDiscoverySettings)
getServiceSettingsResponse_linuxSubscriptionsDiscoverySettings = Lens.lens (\GetServiceSettingsResponse' {linuxSubscriptionsDiscoverySettings} -> linuxSubscriptionsDiscoverySettings) (\s@GetServiceSettingsResponse' {} a -> s {linuxSubscriptionsDiscoverySettings = a} :: GetServiceSettingsResponse)

-- | Indicates the status of Linux subscriptions settings being applied.
getServiceSettingsResponse_status :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe Status)
getServiceSettingsResponse_status = Lens.lens (\GetServiceSettingsResponse' {status} -> status) (\s@GetServiceSettingsResponse' {} a -> s {status = a} :: GetServiceSettingsResponse)

-- | A message which details the Linux subscriptions service settings current
-- status.
getServiceSettingsResponse_statusMessage :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getServiceSettingsResponse_statusMessage = Lens.lens (\GetServiceSettingsResponse' {statusMessage} -> statusMessage) (\s@GetServiceSettingsResponse' {} a -> s {statusMessage = a} :: GetServiceSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getServiceSettingsResponse_httpStatus :: Lens.Lens' GetServiceSettingsResponse Prelude.Int
getServiceSettingsResponse_httpStatus = Lens.lens (\GetServiceSettingsResponse' {httpStatus} -> httpStatus) (\s@GetServiceSettingsResponse' {} a -> s {httpStatus = a} :: GetServiceSettingsResponse)

instance Prelude.NFData GetServiceSettingsResponse where
  rnf GetServiceSettingsResponse' {..} =
    Prelude.rnf homeRegions
      `Prelude.seq` Prelude.rnf linuxSubscriptionsDiscovery
      `Prelude.seq` Prelude.rnf linuxSubscriptionsDiscoverySettings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus

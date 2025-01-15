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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.UpdateServiceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the service settings for Linux subscriptions.
module Amazonka.LicenseManagerLinuxSubscriptions.UpdateServiceSettings
  ( -- * Creating a Request
    UpdateServiceSettings (..),
    newUpdateServiceSettings,

    -- * Request Lenses
    updateServiceSettings_allowUpdate,
    updateServiceSettings_linuxSubscriptionsDiscovery,
    updateServiceSettings_linuxSubscriptionsDiscoverySettings,

    -- * Destructuring the Response
    UpdateServiceSettingsResponse (..),
    newUpdateServiceSettingsResponse,

    -- * Response Lenses
    updateServiceSettingsResponse_homeRegions,
    updateServiceSettingsResponse_linuxSubscriptionsDiscovery,
    updateServiceSettingsResponse_linuxSubscriptionsDiscoverySettings,
    updateServiceSettingsResponse_status,
    updateServiceSettingsResponse_statusMessage,
    updateServiceSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceSettings' smart constructor.
data UpdateServiceSettings = UpdateServiceSettings'
  { -- | Describes if updates are allowed to the service settings for Linux
    -- subscriptions. If you allow updates, you can aggregate Linux
    -- subscription data in more than one home Region.
    allowUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Describes if the discovery of Linux subscriptions is enabled.
    linuxSubscriptionsDiscovery :: LinuxSubscriptionsDiscovery,
    -- | The settings defined for Linux subscriptions discovery. The settings
    -- include if Organizations integration has been enabled, and which Regions
    -- data will be aggregated from.
    linuxSubscriptionsDiscoverySettings :: LinuxSubscriptionsDiscoverySettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowUpdate', 'updateServiceSettings_allowUpdate' - Describes if updates are allowed to the service settings for Linux
-- subscriptions. If you allow updates, you can aggregate Linux
-- subscription data in more than one home Region.
--
-- 'linuxSubscriptionsDiscovery', 'updateServiceSettings_linuxSubscriptionsDiscovery' - Describes if the discovery of Linux subscriptions is enabled.
--
-- 'linuxSubscriptionsDiscoverySettings', 'updateServiceSettings_linuxSubscriptionsDiscoverySettings' - The settings defined for Linux subscriptions discovery. The settings
-- include if Organizations integration has been enabled, and which Regions
-- data will be aggregated from.
newUpdateServiceSettings ::
  -- | 'linuxSubscriptionsDiscovery'
  LinuxSubscriptionsDiscovery ->
  -- | 'linuxSubscriptionsDiscoverySettings'
  LinuxSubscriptionsDiscoverySettings ->
  UpdateServiceSettings
newUpdateServiceSettings
  pLinuxSubscriptionsDiscovery_
  pLinuxSubscriptionsDiscoverySettings_ =
    UpdateServiceSettings'
      { allowUpdate =
          Prelude.Nothing,
        linuxSubscriptionsDiscovery =
          pLinuxSubscriptionsDiscovery_,
        linuxSubscriptionsDiscoverySettings =
          pLinuxSubscriptionsDiscoverySettings_
      }

-- | Describes if updates are allowed to the service settings for Linux
-- subscriptions. If you allow updates, you can aggregate Linux
-- subscription data in more than one home Region.
updateServiceSettings_allowUpdate :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Bool)
updateServiceSettings_allowUpdate = Lens.lens (\UpdateServiceSettings' {allowUpdate} -> allowUpdate) (\s@UpdateServiceSettings' {} a -> s {allowUpdate = a} :: UpdateServiceSettings)

-- | Describes if the discovery of Linux subscriptions is enabled.
updateServiceSettings_linuxSubscriptionsDiscovery :: Lens.Lens' UpdateServiceSettings LinuxSubscriptionsDiscovery
updateServiceSettings_linuxSubscriptionsDiscovery = Lens.lens (\UpdateServiceSettings' {linuxSubscriptionsDiscovery} -> linuxSubscriptionsDiscovery) (\s@UpdateServiceSettings' {} a -> s {linuxSubscriptionsDiscovery = a} :: UpdateServiceSettings)

-- | The settings defined for Linux subscriptions discovery. The settings
-- include if Organizations integration has been enabled, and which Regions
-- data will be aggregated from.
updateServiceSettings_linuxSubscriptionsDiscoverySettings :: Lens.Lens' UpdateServiceSettings LinuxSubscriptionsDiscoverySettings
updateServiceSettings_linuxSubscriptionsDiscoverySettings = Lens.lens (\UpdateServiceSettings' {linuxSubscriptionsDiscoverySettings} -> linuxSubscriptionsDiscoverySettings) (\s@UpdateServiceSettings' {} a -> s {linuxSubscriptionsDiscoverySettings = a} :: UpdateServiceSettings)

instance Core.AWSRequest UpdateServiceSettings where
  type
    AWSResponse UpdateServiceSettings =
      UpdateServiceSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceSettingsResponse'
            Prelude.<$> (x Data..?> "HomeRegions")
            Prelude.<*> (x Data..?> "LinuxSubscriptionsDiscovery")
            Prelude.<*> (x Data..?> "LinuxSubscriptionsDiscoverySettings")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSettings where
  hashWithSalt _salt UpdateServiceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` allowUpdate
      `Prelude.hashWithSalt` linuxSubscriptionsDiscovery
      `Prelude.hashWithSalt` linuxSubscriptionsDiscoverySettings

instance Prelude.NFData UpdateServiceSettings where
  rnf UpdateServiceSettings' {..} =
    Prelude.rnf allowUpdate `Prelude.seq`
      Prelude.rnf linuxSubscriptionsDiscovery `Prelude.seq`
        Prelude.rnf linuxSubscriptionsDiscoverySettings

instance Data.ToHeaders UpdateServiceSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceSettings where
  toJSON UpdateServiceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowUpdate" Data..=) Prelude.<$> allowUpdate,
            Prelude.Just
              ( "LinuxSubscriptionsDiscovery"
                  Data..= linuxSubscriptionsDiscovery
              ),
            Prelude.Just
              ( "LinuxSubscriptionsDiscoverySettings"
                  Data..= linuxSubscriptionsDiscoverySettings
              )
          ]
      )

instance Data.ToPath UpdateServiceSettings where
  toPath =
    Prelude.const "/subscription/UpdateServiceSettings"

instance Data.ToQuery UpdateServiceSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceSettingsResponse' smart constructor.
data UpdateServiceSettingsResponse = UpdateServiceSettingsResponse'
  { -- | The Region in which License Manager displays the aggregated data for
    -- Linux subscriptions.
    homeRegions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Lists if discovery has been enabled for Linux subscriptions.
    linuxSubscriptionsDiscovery :: Prelude.Maybe LinuxSubscriptionsDiscovery,
    -- | The settings defined for Linux subscriptions discovery. The settings
    -- include if Organizations integration has been enabled, and which Regions
    -- data will be aggregated from.
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
-- Create a value of 'UpdateServiceSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeRegions', 'updateServiceSettingsResponse_homeRegions' - The Region in which License Manager displays the aggregated data for
-- Linux subscriptions.
--
-- 'linuxSubscriptionsDiscovery', 'updateServiceSettingsResponse_linuxSubscriptionsDiscovery' - Lists if discovery has been enabled for Linux subscriptions.
--
-- 'linuxSubscriptionsDiscoverySettings', 'updateServiceSettingsResponse_linuxSubscriptionsDiscoverySettings' - The settings defined for Linux subscriptions discovery. The settings
-- include if Organizations integration has been enabled, and which Regions
-- data will be aggregated from.
--
-- 'status', 'updateServiceSettingsResponse_status' - Indicates the status of Linux subscriptions settings being applied.
--
-- 'statusMessage', 'updateServiceSettingsResponse_statusMessage' - A message which details the Linux subscriptions service settings current
-- status.
--
-- 'httpStatus', 'updateServiceSettingsResponse_httpStatus' - The response's http status code.
newUpdateServiceSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceSettingsResponse
newUpdateServiceSettingsResponse pHttpStatus_ =
  UpdateServiceSettingsResponse'
    { homeRegions =
        Prelude.Nothing,
      linuxSubscriptionsDiscovery =
        Prelude.Nothing,
      linuxSubscriptionsDiscoverySettings =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Region in which License Manager displays the aggregated data for
-- Linux subscriptions.
updateServiceSettingsResponse_homeRegions :: Lens.Lens' UpdateServiceSettingsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateServiceSettingsResponse_homeRegions = Lens.lens (\UpdateServiceSettingsResponse' {homeRegions} -> homeRegions) (\s@UpdateServiceSettingsResponse' {} a -> s {homeRegions = a} :: UpdateServiceSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Lists if discovery has been enabled for Linux subscriptions.
updateServiceSettingsResponse_linuxSubscriptionsDiscovery :: Lens.Lens' UpdateServiceSettingsResponse (Prelude.Maybe LinuxSubscriptionsDiscovery)
updateServiceSettingsResponse_linuxSubscriptionsDiscovery = Lens.lens (\UpdateServiceSettingsResponse' {linuxSubscriptionsDiscovery} -> linuxSubscriptionsDiscovery) (\s@UpdateServiceSettingsResponse' {} a -> s {linuxSubscriptionsDiscovery = a} :: UpdateServiceSettingsResponse)

-- | The settings defined for Linux subscriptions discovery. The settings
-- include if Organizations integration has been enabled, and which Regions
-- data will be aggregated from.
updateServiceSettingsResponse_linuxSubscriptionsDiscoverySettings :: Lens.Lens' UpdateServiceSettingsResponse (Prelude.Maybe LinuxSubscriptionsDiscoverySettings)
updateServiceSettingsResponse_linuxSubscriptionsDiscoverySettings = Lens.lens (\UpdateServiceSettingsResponse' {linuxSubscriptionsDiscoverySettings} -> linuxSubscriptionsDiscoverySettings) (\s@UpdateServiceSettingsResponse' {} a -> s {linuxSubscriptionsDiscoverySettings = a} :: UpdateServiceSettingsResponse)

-- | Indicates the status of Linux subscriptions settings being applied.
updateServiceSettingsResponse_status :: Lens.Lens' UpdateServiceSettingsResponse (Prelude.Maybe Status)
updateServiceSettingsResponse_status = Lens.lens (\UpdateServiceSettingsResponse' {status} -> status) (\s@UpdateServiceSettingsResponse' {} a -> s {status = a} :: UpdateServiceSettingsResponse)

-- | A message which details the Linux subscriptions service settings current
-- status.
updateServiceSettingsResponse_statusMessage :: Lens.Lens' UpdateServiceSettingsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateServiceSettingsResponse_statusMessage = Lens.lens (\UpdateServiceSettingsResponse' {statusMessage} -> statusMessage) (\s@UpdateServiceSettingsResponse' {} a -> s {statusMessage = a} :: UpdateServiceSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateServiceSettingsResponse_httpStatus :: Lens.Lens' UpdateServiceSettingsResponse Prelude.Int
updateServiceSettingsResponse_httpStatus = Lens.lens (\UpdateServiceSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceSettingsResponse' {} a -> s {httpStatus = a} :: UpdateServiceSettingsResponse)

instance Prelude.NFData UpdateServiceSettingsResponse where
  rnf UpdateServiceSettingsResponse' {..} =
    Prelude.rnf homeRegions `Prelude.seq`
      Prelude.rnf linuxSubscriptionsDiscovery `Prelude.seq`
        Prelude.rnf linuxSubscriptionsDiscoverySettings `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf statusMessage `Prelude.seq`
              Prelude.rnf httpStatus

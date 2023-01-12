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
-- Module      : Amazonka.LookoutMetrics.UpdateAlert
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Make changes to an existing alert.
module Amazonka.LookoutMetrics.UpdateAlert
  ( -- * Creating a Request
    UpdateAlert (..),
    newUpdateAlert,

    -- * Request Lenses
    updateAlert_action,
    updateAlert_alertDescription,
    updateAlert_alertFilters,
    updateAlert_alertSensitivityThreshold,
    updateAlert_alertArn,

    -- * Destructuring the Response
    UpdateAlertResponse (..),
    newUpdateAlertResponse,

    -- * Response Lenses
    updateAlertResponse_alertArn,
    updateAlertResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAlert' smart constructor.
data UpdateAlert = UpdateAlert'
  { -- | Action that will be triggered when there is an alert.
    action :: Prelude.Maybe Action,
    -- | A description of the alert.
    alertDescription :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the alert filters, containing MetricList and
    -- DimensionFilterList.
    alertFilters :: Prelude.Maybe AlertFilters,
    -- | An integer from 0 to 100 specifying the alert sensitivity threshold.
    alertSensitivityThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the alert to update.
    alertArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateAlert_action' - Action that will be triggered when there is an alert.
--
-- 'alertDescription', 'updateAlert_alertDescription' - A description of the alert.
--
-- 'alertFilters', 'updateAlert_alertFilters' - The configuration of the alert filters, containing MetricList and
-- DimensionFilterList.
--
-- 'alertSensitivityThreshold', 'updateAlert_alertSensitivityThreshold' - An integer from 0 to 100 specifying the alert sensitivity threshold.
--
-- 'alertArn', 'updateAlert_alertArn' - The ARN of the alert to update.
newUpdateAlert ::
  -- | 'alertArn'
  Prelude.Text ->
  UpdateAlert
newUpdateAlert pAlertArn_ =
  UpdateAlert'
    { action = Prelude.Nothing,
      alertDescription = Prelude.Nothing,
      alertFilters = Prelude.Nothing,
      alertSensitivityThreshold = Prelude.Nothing,
      alertArn = pAlertArn_
    }

-- | Action that will be triggered when there is an alert.
updateAlert_action :: Lens.Lens' UpdateAlert (Prelude.Maybe Action)
updateAlert_action = Lens.lens (\UpdateAlert' {action} -> action) (\s@UpdateAlert' {} a -> s {action = a} :: UpdateAlert)

-- | A description of the alert.
updateAlert_alertDescription :: Lens.Lens' UpdateAlert (Prelude.Maybe Prelude.Text)
updateAlert_alertDescription = Lens.lens (\UpdateAlert' {alertDescription} -> alertDescription) (\s@UpdateAlert' {} a -> s {alertDescription = a} :: UpdateAlert)

-- | The configuration of the alert filters, containing MetricList and
-- DimensionFilterList.
updateAlert_alertFilters :: Lens.Lens' UpdateAlert (Prelude.Maybe AlertFilters)
updateAlert_alertFilters = Lens.lens (\UpdateAlert' {alertFilters} -> alertFilters) (\s@UpdateAlert' {} a -> s {alertFilters = a} :: UpdateAlert)

-- | An integer from 0 to 100 specifying the alert sensitivity threshold.
updateAlert_alertSensitivityThreshold :: Lens.Lens' UpdateAlert (Prelude.Maybe Prelude.Natural)
updateAlert_alertSensitivityThreshold = Lens.lens (\UpdateAlert' {alertSensitivityThreshold} -> alertSensitivityThreshold) (\s@UpdateAlert' {} a -> s {alertSensitivityThreshold = a} :: UpdateAlert)

-- | The ARN of the alert to update.
updateAlert_alertArn :: Lens.Lens' UpdateAlert Prelude.Text
updateAlert_alertArn = Lens.lens (\UpdateAlert' {alertArn} -> alertArn) (\s@UpdateAlert' {} a -> s {alertArn = a} :: UpdateAlert)

instance Core.AWSRequest UpdateAlert where
  type AWSResponse UpdateAlert = UpdateAlertResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAlertResponse'
            Prelude.<$> (x Data..?> "AlertArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAlert where
  hashWithSalt _salt UpdateAlert' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` alertDescription
      `Prelude.hashWithSalt` alertFilters
      `Prelude.hashWithSalt` alertSensitivityThreshold
      `Prelude.hashWithSalt` alertArn

instance Prelude.NFData UpdateAlert where
  rnf UpdateAlert' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf alertDescription
      `Prelude.seq` Prelude.rnf alertFilters
      `Prelude.seq` Prelude.rnf alertSensitivityThreshold
      `Prelude.seq` Prelude.rnf alertArn

instance Data.ToHeaders UpdateAlert where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAlert where
  toJSON UpdateAlert' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("AlertDescription" Data..=)
              Prelude.<$> alertDescription,
            ("AlertFilters" Data..=) Prelude.<$> alertFilters,
            ("AlertSensitivityThreshold" Data..=)
              Prelude.<$> alertSensitivityThreshold,
            Prelude.Just ("AlertArn" Data..= alertArn)
          ]
      )

instance Data.ToPath UpdateAlert where
  toPath = Prelude.const "/UpdateAlert"

instance Data.ToQuery UpdateAlert where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAlertResponse' smart constructor.
data UpdateAlertResponse = UpdateAlertResponse'
  { -- | The ARN of the updated alert.
    alertArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlertResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertArn', 'updateAlertResponse_alertArn' - The ARN of the updated alert.
--
-- 'httpStatus', 'updateAlertResponse_httpStatus' - The response's http status code.
newUpdateAlertResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAlertResponse
newUpdateAlertResponse pHttpStatus_ =
  UpdateAlertResponse'
    { alertArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the updated alert.
updateAlertResponse_alertArn :: Lens.Lens' UpdateAlertResponse (Prelude.Maybe Prelude.Text)
updateAlertResponse_alertArn = Lens.lens (\UpdateAlertResponse' {alertArn} -> alertArn) (\s@UpdateAlertResponse' {} a -> s {alertArn = a} :: UpdateAlertResponse)

-- | The response's http status code.
updateAlertResponse_httpStatus :: Lens.Lens' UpdateAlertResponse Prelude.Int
updateAlertResponse_httpStatus = Lens.lens (\UpdateAlertResponse' {httpStatus} -> httpStatus) (\s@UpdateAlertResponse' {} a -> s {httpStatus = a} :: UpdateAlertResponse)

instance Prelude.NFData UpdateAlertResponse where
  rnf UpdateAlertResponse' {..} =
    Prelude.rnf alertArn
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.LookoutMetrics.CreateAlert
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alert for an anomaly detector.
module Amazonka.LookoutMetrics.CreateAlert
  ( -- * Creating a Request
    CreateAlert (..),
    newCreateAlert,

    -- * Request Lenses
    createAlert_tags,
    createAlert_alertDescription,
    createAlert_alertSensitivityThreshold,
    createAlert_alertFilters,
    createAlert_alertName,
    createAlert_anomalyDetectorArn,
    createAlert_action,

    -- * Destructuring the Response
    CreateAlertResponse (..),
    newCreateAlertResponse,

    -- * Response Lenses
    createAlertResponse_alertArn,
    createAlertResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAlert' smart constructor.
data CreateAlert = CreateAlert'
  { -- | A list of
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
    -- to apply to the alert.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the alert.
    alertDescription :: Prelude.Maybe Prelude.Text,
    -- | An integer from 0 to 100 specifying the alert sensitivity threshold.
    alertSensitivityThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The configuration of the alert filters, containing MetricList and
    -- DimensionFilterList.
    alertFilters :: Prelude.Maybe AlertFilters,
    -- | The name of the alert.
    alertName :: Prelude.Text,
    -- | The ARN of the detector to which the alert is attached.
    anomalyDetectorArn :: Prelude.Text,
    -- | Action that will be triggered when there is an alert.
    action :: Action
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAlert_tags' - A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the alert.
--
-- 'alertDescription', 'createAlert_alertDescription' - A description of the alert.
--
-- 'alertSensitivityThreshold', 'createAlert_alertSensitivityThreshold' - An integer from 0 to 100 specifying the alert sensitivity threshold.
--
-- 'alertFilters', 'createAlert_alertFilters' - The configuration of the alert filters, containing MetricList and
-- DimensionFilterList.
--
-- 'alertName', 'createAlert_alertName' - The name of the alert.
--
-- 'anomalyDetectorArn', 'createAlert_anomalyDetectorArn' - The ARN of the detector to which the alert is attached.
--
-- 'action', 'createAlert_action' - Action that will be triggered when there is an alert.
newCreateAlert ::
  -- | 'alertName'
  Prelude.Text ->
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'action'
  Action ->
  CreateAlert
newCreateAlert
  pAlertName_
  pAnomalyDetectorArn_
  pAction_ =
    CreateAlert'
      { tags = Prelude.Nothing,
        alertDescription = Prelude.Nothing,
        alertSensitivityThreshold = Prelude.Nothing,
        alertFilters = Prelude.Nothing,
        alertName = pAlertName_,
        anomalyDetectorArn = pAnomalyDetectorArn_,
        action = pAction_
      }

-- | A list of
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>
-- to apply to the alert.
createAlert_tags :: Lens.Lens' CreateAlert (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAlert_tags = Lens.lens (\CreateAlert' {tags} -> tags) (\s@CreateAlert' {} a -> s {tags = a} :: CreateAlert) Prelude.. Lens.mapping Lens.coerced

-- | A description of the alert.
createAlert_alertDescription :: Lens.Lens' CreateAlert (Prelude.Maybe Prelude.Text)
createAlert_alertDescription = Lens.lens (\CreateAlert' {alertDescription} -> alertDescription) (\s@CreateAlert' {} a -> s {alertDescription = a} :: CreateAlert)

-- | An integer from 0 to 100 specifying the alert sensitivity threshold.
createAlert_alertSensitivityThreshold :: Lens.Lens' CreateAlert (Prelude.Maybe Prelude.Natural)
createAlert_alertSensitivityThreshold = Lens.lens (\CreateAlert' {alertSensitivityThreshold} -> alertSensitivityThreshold) (\s@CreateAlert' {} a -> s {alertSensitivityThreshold = a} :: CreateAlert)

-- | The configuration of the alert filters, containing MetricList and
-- DimensionFilterList.
createAlert_alertFilters :: Lens.Lens' CreateAlert (Prelude.Maybe AlertFilters)
createAlert_alertFilters = Lens.lens (\CreateAlert' {alertFilters} -> alertFilters) (\s@CreateAlert' {} a -> s {alertFilters = a} :: CreateAlert)

-- | The name of the alert.
createAlert_alertName :: Lens.Lens' CreateAlert Prelude.Text
createAlert_alertName = Lens.lens (\CreateAlert' {alertName} -> alertName) (\s@CreateAlert' {} a -> s {alertName = a} :: CreateAlert)

-- | The ARN of the detector to which the alert is attached.
createAlert_anomalyDetectorArn :: Lens.Lens' CreateAlert Prelude.Text
createAlert_anomalyDetectorArn = Lens.lens (\CreateAlert' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@CreateAlert' {} a -> s {anomalyDetectorArn = a} :: CreateAlert)

-- | Action that will be triggered when there is an alert.
createAlert_action :: Lens.Lens' CreateAlert Action
createAlert_action = Lens.lens (\CreateAlert' {action} -> action) (\s@CreateAlert' {} a -> s {action = a} :: CreateAlert)

instance Core.AWSRequest CreateAlert where
  type AWSResponse CreateAlert = CreateAlertResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAlertResponse'
            Prelude.<$> (x Data..?> "AlertArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAlert where
  hashWithSalt _salt CreateAlert' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` alertDescription
      `Prelude.hashWithSalt` alertSensitivityThreshold
      `Prelude.hashWithSalt` alertFilters
      `Prelude.hashWithSalt` alertName
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` action

instance Prelude.NFData CreateAlert where
  rnf CreateAlert' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf alertDescription
      `Prelude.seq` Prelude.rnf alertSensitivityThreshold
      `Prelude.seq` Prelude.rnf alertFilters
      `Prelude.seq` Prelude.rnf alertName
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf action

instance Data.ToHeaders CreateAlert where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAlert where
  toJSON CreateAlert' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("AlertDescription" Data..=)
              Prelude.<$> alertDescription,
            ("AlertSensitivityThreshold" Data..=)
              Prelude.<$> alertSensitivityThreshold,
            ("AlertFilters" Data..=) Prelude.<$> alertFilters,
            Prelude.Just ("AlertName" Data..= alertName),
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn),
            Prelude.Just ("Action" Data..= action)
          ]
      )

instance Data.ToPath CreateAlert where
  toPath = Prelude.const "/CreateAlert"

instance Data.ToQuery CreateAlert where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAlertResponse' smart constructor.
data CreateAlertResponse = CreateAlertResponse'
  { -- | The ARN of the alert.
    alertArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlertResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertArn', 'createAlertResponse_alertArn' - The ARN of the alert.
--
-- 'httpStatus', 'createAlertResponse_httpStatus' - The response's http status code.
newCreateAlertResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAlertResponse
newCreateAlertResponse pHttpStatus_ =
  CreateAlertResponse'
    { alertArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the alert.
createAlertResponse_alertArn :: Lens.Lens' CreateAlertResponse (Prelude.Maybe Prelude.Text)
createAlertResponse_alertArn = Lens.lens (\CreateAlertResponse' {alertArn} -> alertArn) (\s@CreateAlertResponse' {} a -> s {alertArn = a} :: CreateAlertResponse)

-- | The response's http status code.
createAlertResponse_httpStatus :: Lens.Lens' CreateAlertResponse Prelude.Int
createAlertResponse_httpStatus = Lens.lens (\CreateAlertResponse' {httpStatus} -> httpStatus) (\s@CreateAlertResponse' {} a -> s {httpStatus = a} :: CreateAlertResponse)

instance Prelude.NFData CreateAlertResponse where
  rnf CreateAlertResponse' {..} =
    Prelude.rnf alertArn
      `Prelude.seq` Prelude.rnf httpStatus

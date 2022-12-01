{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutMetrics.Types.AlertSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AlertSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.AlertStatus
import Amazonka.LookoutMetrics.Types.AlertType
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of an alert\'s configuration.
--
-- /See:/ 'newAlertSummary' smart constructor.
data AlertSummary = AlertSummary'
  { -- | The time at which the alert was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The alert\'s
    -- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the detector to which the alert is attached.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The minimum severity for an anomaly to trigger the alert.
    alertSensitivityThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The time at which the alert was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the alert.
    alertName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the alert.
    alertArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the alert.
    alertStatus :: Prelude.Maybe AlertStatus,
    -- | The type of the alert.
    alertType :: Prelude.Maybe AlertType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlertSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'alertSummary_lastModificationTime' - The time at which the alert was last modified.
--
-- 'tags', 'alertSummary_tags' - The alert\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
--
-- 'anomalyDetectorArn', 'alertSummary_anomalyDetectorArn' - The ARN of the detector to which the alert is attached.
--
-- 'alertSensitivityThreshold', 'alertSummary_alertSensitivityThreshold' - The minimum severity for an anomaly to trigger the alert.
--
-- 'creationTime', 'alertSummary_creationTime' - The time at which the alert was created.
--
-- 'alertName', 'alertSummary_alertName' - The name of the alert.
--
-- 'alertArn', 'alertSummary_alertArn' - The ARN of the alert.
--
-- 'alertStatus', 'alertSummary_alertStatus' - The status of the alert.
--
-- 'alertType', 'alertSummary_alertType' - The type of the alert.
newAlertSummary ::
  AlertSummary
newAlertSummary =
  AlertSummary'
    { lastModificationTime =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      alertSensitivityThreshold = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      alertName = Prelude.Nothing,
      alertArn = Prelude.Nothing,
      alertStatus = Prelude.Nothing,
      alertType = Prelude.Nothing
    }

-- | The time at which the alert was last modified.
alertSummary_lastModificationTime :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.UTCTime)
alertSummary_lastModificationTime = Lens.lens (\AlertSummary' {lastModificationTime} -> lastModificationTime) (\s@AlertSummary' {} a -> s {lastModificationTime = a} :: AlertSummary) Prelude.. Lens.mapping Core._Time

-- | The alert\'s
-- <https://docs.aws.amazon.com/lookoutmetrics/latest/dev/detectors-tags.html tags>.
alertSummary_tags :: Lens.Lens' AlertSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
alertSummary_tags = Lens.lens (\AlertSummary' {tags} -> tags) (\s@AlertSummary' {} a -> s {tags = a} :: AlertSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the detector to which the alert is attached.
alertSummary_anomalyDetectorArn :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.Text)
alertSummary_anomalyDetectorArn = Lens.lens (\AlertSummary' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@AlertSummary' {} a -> s {anomalyDetectorArn = a} :: AlertSummary)

-- | The minimum severity for an anomaly to trigger the alert.
alertSummary_alertSensitivityThreshold :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.Natural)
alertSummary_alertSensitivityThreshold = Lens.lens (\AlertSummary' {alertSensitivityThreshold} -> alertSensitivityThreshold) (\s@AlertSummary' {} a -> s {alertSensitivityThreshold = a} :: AlertSummary)

-- | The time at which the alert was created.
alertSummary_creationTime :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.UTCTime)
alertSummary_creationTime = Lens.lens (\AlertSummary' {creationTime} -> creationTime) (\s@AlertSummary' {} a -> s {creationTime = a} :: AlertSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the alert.
alertSummary_alertName :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.Text)
alertSummary_alertName = Lens.lens (\AlertSummary' {alertName} -> alertName) (\s@AlertSummary' {} a -> s {alertName = a} :: AlertSummary)

-- | The ARN of the alert.
alertSummary_alertArn :: Lens.Lens' AlertSummary (Prelude.Maybe Prelude.Text)
alertSummary_alertArn = Lens.lens (\AlertSummary' {alertArn} -> alertArn) (\s@AlertSummary' {} a -> s {alertArn = a} :: AlertSummary)

-- | The status of the alert.
alertSummary_alertStatus :: Lens.Lens' AlertSummary (Prelude.Maybe AlertStatus)
alertSummary_alertStatus = Lens.lens (\AlertSummary' {alertStatus} -> alertStatus) (\s@AlertSummary' {} a -> s {alertStatus = a} :: AlertSummary)

-- | The type of the alert.
alertSummary_alertType :: Lens.Lens' AlertSummary (Prelude.Maybe AlertType)
alertSummary_alertType = Lens.lens (\AlertSummary' {alertType} -> alertType) (\s@AlertSummary' {} a -> s {alertType = a} :: AlertSummary)

instance Core.FromJSON AlertSummary where
  parseJSON =
    Core.withObject
      "AlertSummary"
      ( \x ->
          AlertSummary'
            Prelude.<$> (x Core..:? "LastModificationTime")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AnomalyDetectorArn")
            Prelude.<*> (x Core..:? "AlertSensitivityThreshold")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "AlertName")
            Prelude.<*> (x Core..:? "AlertArn")
            Prelude.<*> (x Core..:? "AlertStatus")
            Prelude.<*> (x Core..:? "AlertType")
      )

instance Prelude.Hashable AlertSummary where
  hashWithSalt _salt AlertSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` alertSensitivityThreshold
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` alertName
      `Prelude.hashWithSalt` alertArn
      `Prelude.hashWithSalt` alertStatus
      `Prelude.hashWithSalt` alertType

instance Prelude.NFData AlertSummary where
  rnf AlertSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf alertSensitivityThreshold
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf alertName
      `Prelude.seq` Prelude.rnf alertArn
      `Prelude.seq` Prelude.rnf alertStatus
      `Prelude.seq` Prelude.rnf alertType

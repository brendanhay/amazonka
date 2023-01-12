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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ApplicationMode
import Amazonka.KinesisAnalyticsV2.Types.ApplicationStatus
import Amazonka.KinesisAnalyticsV2.Types.RuntimeEnvironment
import qualified Amazonka.Prelude as Prelude

-- | Provides application summary information, including the application
-- Amazon Resource Name (ARN), name, and status.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | For a Kinesis Data Analytics for Apache Flink application, the mode is
    -- @STREAMING@. For a Kinesis Data Analytics Studio notebook, it is
    -- @INTERACTIVE@.
    applicationMode :: Prelude.Maybe ApplicationMode,
    -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The ARN of the application.
    applicationARN :: Prelude.Text,
    -- | The status of the application.
    applicationStatus :: ApplicationStatus,
    -- | Provides the current application version.
    applicationVersionId :: Prelude.Natural,
    -- | The runtime environment for the application.
    runtimeEnvironment :: RuntimeEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationMode', 'applicationSummary_applicationMode' - For a Kinesis Data Analytics for Apache Flink application, the mode is
-- @STREAMING@. For a Kinesis Data Analytics Studio notebook, it is
-- @INTERACTIVE@.
--
-- 'applicationName', 'applicationSummary_applicationName' - The name of the application.
--
-- 'applicationARN', 'applicationSummary_applicationARN' - The ARN of the application.
--
-- 'applicationStatus', 'applicationSummary_applicationStatus' - The status of the application.
--
-- 'applicationVersionId', 'applicationSummary_applicationVersionId' - Provides the current application version.
--
-- 'runtimeEnvironment', 'applicationSummary_runtimeEnvironment' - The runtime environment for the application.
newApplicationSummary ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'applicationARN'
  Prelude.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  -- | 'applicationVersionId'
  Prelude.Natural ->
  -- | 'runtimeEnvironment'
  RuntimeEnvironment ->
  ApplicationSummary
newApplicationSummary
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_
  pApplicationVersionId_
  pRuntimeEnvironment_ =
    ApplicationSummary'
      { applicationMode =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_,
        applicationVersionId = pApplicationVersionId_,
        runtimeEnvironment = pRuntimeEnvironment_
      }

-- | For a Kinesis Data Analytics for Apache Flink application, the mode is
-- @STREAMING@. For a Kinesis Data Analytics Studio notebook, it is
-- @INTERACTIVE@.
applicationSummary_applicationMode :: Lens.Lens' ApplicationSummary (Prelude.Maybe ApplicationMode)
applicationSummary_applicationMode = Lens.lens (\ApplicationSummary' {applicationMode} -> applicationMode) (\s@ApplicationSummary' {} a -> s {applicationMode = a} :: ApplicationSummary)

-- | The name of the application.
applicationSummary_applicationName :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationName = Lens.lens (\ApplicationSummary' {applicationName} -> applicationName) (\s@ApplicationSummary' {} a -> s {applicationName = a} :: ApplicationSummary)

-- | The ARN of the application.
applicationSummary_applicationARN :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationARN = Lens.lens (\ApplicationSummary' {applicationARN} -> applicationARN) (\s@ApplicationSummary' {} a -> s {applicationARN = a} :: ApplicationSummary)

-- | The status of the application.
applicationSummary_applicationStatus :: Lens.Lens' ApplicationSummary ApplicationStatus
applicationSummary_applicationStatus = Lens.lens (\ApplicationSummary' {applicationStatus} -> applicationStatus) (\s@ApplicationSummary' {} a -> s {applicationStatus = a} :: ApplicationSummary)

-- | Provides the current application version.
applicationSummary_applicationVersionId :: Lens.Lens' ApplicationSummary Prelude.Natural
applicationSummary_applicationVersionId = Lens.lens (\ApplicationSummary' {applicationVersionId} -> applicationVersionId) (\s@ApplicationSummary' {} a -> s {applicationVersionId = a} :: ApplicationSummary)

-- | The runtime environment for the application.
applicationSummary_runtimeEnvironment :: Lens.Lens' ApplicationSummary RuntimeEnvironment
applicationSummary_runtimeEnvironment = Lens.lens (\ApplicationSummary' {runtimeEnvironment} -> runtimeEnvironment) (\s@ApplicationSummary' {} a -> s {runtimeEnvironment = a} :: ApplicationSummary)

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "ApplicationMode")
            Prelude.<*> (x Data..: "ApplicationName")
            Prelude.<*> (x Data..: "ApplicationARN")
            Prelude.<*> (x Data..: "ApplicationStatus")
            Prelude.<*> (x Data..: "ApplicationVersionId")
            Prelude.<*> (x Data..: "RuntimeEnvironment")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt `Prelude.hashWithSalt` applicationMode
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` applicationARN
      `Prelude.hashWithSalt` applicationStatus
      `Prelude.hashWithSalt` applicationVersionId
      `Prelude.hashWithSalt` runtimeEnvironment

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf applicationMode
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf applicationARN
      `Prelude.seq` Prelude.rnf applicationStatus
      `Prelude.seq` Prelude.rnf applicationVersionId
      `Prelude.seq` Prelude.rnf runtimeEnvironment

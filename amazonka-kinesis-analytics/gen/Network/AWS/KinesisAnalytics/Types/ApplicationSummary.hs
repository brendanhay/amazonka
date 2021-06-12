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
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import qualified Network.AWS.Lens as Lens

-- | This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Provides application summary information, including the application
-- Amazon Resource Name (ARN), name, and status.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | Name of the application.
    applicationName :: Core.Text,
    -- | ARN of the application.
    applicationARN :: Core.Text,
    -- | Status of the application.
    applicationStatus :: ApplicationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'applicationSummary_applicationName' - Name of the application.
--
-- 'applicationARN', 'applicationSummary_applicationARN' - ARN of the application.
--
-- 'applicationStatus', 'applicationSummary_applicationStatus' - Status of the application.
newApplicationSummary ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'applicationARN'
  Core.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  ApplicationSummary
newApplicationSummary
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_ =
    ApplicationSummary'
      { applicationName =
          pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_
      }

-- | Name of the application.
applicationSummary_applicationName :: Lens.Lens' ApplicationSummary Core.Text
applicationSummary_applicationName = Lens.lens (\ApplicationSummary' {applicationName} -> applicationName) (\s@ApplicationSummary' {} a -> s {applicationName = a} :: ApplicationSummary)

-- | ARN of the application.
applicationSummary_applicationARN :: Lens.Lens' ApplicationSummary Core.Text
applicationSummary_applicationARN = Lens.lens (\ApplicationSummary' {applicationARN} -> applicationARN) (\s@ApplicationSummary' {} a -> s {applicationARN = a} :: ApplicationSummary)

-- | Status of the application.
applicationSummary_applicationStatus :: Lens.Lens' ApplicationSummary ApplicationStatus
applicationSummary_applicationStatus = Lens.lens (\ApplicationSummary' {applicationStatus} -> applicationStatus) (\s@ApplicationSummary' {} a -> s {applicationStatus = a} :: ApplicationSummary)

instance Core.FromJSON ApplicationSummary where
  parseJSON =
    Core.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Core.<$> (x Core..: "ApplicationName")
            Core.<*> (x Core..: "ApplicationARN")
            Core.<*> (x Core..: "ApplicationStatus")
      )

instance Core.Hashable ApplicationSummary

instance Core.NFData ApplicationSummary

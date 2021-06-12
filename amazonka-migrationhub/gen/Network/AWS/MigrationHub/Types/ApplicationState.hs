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
-- Module      : Network.AWS.MigrationHub.Types.ApplicationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ApplicationState where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ApplicationStatus

-- | The state of an application discovered through Migration Hub import, the
-- AWS Agentless Discovery Connector, or the AWS Application Discovery
-- Agent.
--
-- /See:/ 'newApplicationState' smart constructor.
data ApplicationState = ApplicationState'
  { -- | The configurationId from the Application Discovery Service that uniquely
    -- identifies an application.
    applicationId :: Core.Maybe Core.Text,
    -- | The current status of an application.
    applicationStatus :: Core.Maybe ApplicationStatus,
    -- | The timestamp when the application status was last updated.
    lastUpdatedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'applicationState_applicationId' - The configurationId from the Application Discovery Service that uniquely
-- identifies an application.
--
-- 'applicationStatus', 'applicationState_applicationStatus' - The current status of an application.
--
-- 'lastUpdatedTime', 'applicationState_lastUpdatedTime' - The timestamp when the application status was last updated.
newApplicationState ::
  ApplicationState
newApplicationState =
  ApplicationState'
    { applicationId = Core.Nothing,
      applicationStatus = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | The configurationId from the Application Discovery Service that uniquely
-- identifies an application.
applicationState_applicationId :: Lens.Lens' ApplicationState (Core.Maybe Core.Text)
applicationState_applicationId = Lens.lens (\ApplicationState' {applicationId} -> applicationId) (\s@ApplicationState' {} a -> s {applicationId = a} :: ApplicationState)

-- | The current status of an application.
applicationState_applicationStatus :: Lens.Lens' ApplicationState (Core.Maybe ApplicationStatus)
applicationState_applicationStatus = Lens.lens (\ApplicationState' {applicationStatus} -> applicationStatus) (\s@ApplicationState' {} a -> s {applicationStatus = a} :: ApplicationState)

-- | The timestamp when the application status was last updated.
applicationState_lastUpdatedTime :: Lens.Lens' ApplicationState (Core.Maybe Core.UTCTime)
applicationState_lastUpdatedTime = Lens.lens (\ApplicationState' {lastUpdatedTime} -> lastUpdatedTime) (\s@ApplicationState' {} a -> s {lastUpdatedTime = a} :: ApplicationState) Core.. Lens.mapping Core._Time

instance Core.FromJSON ApplicationState where
  parseJSON =
    Core.withObject
      "ApplicationState"
      ( \x ->
          ApplicationState'
            Core.<$> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "ApplicationStatus")
            Core.<*> (x Core..:? "LastUpdatedTime")
      )

instance Core.Hashable ApplicationState

instance Core.NFData ApplicationState

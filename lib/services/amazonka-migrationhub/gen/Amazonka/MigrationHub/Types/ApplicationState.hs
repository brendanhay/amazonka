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
-- Module      : Amazonka.MigrationHub.Types.ApplicationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.ApplicationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHub.Types.ApplicationStatus
import qualified Amazonka.Prelude as Prelude

-- | The state of an application discovered through Migration Hub import, the
-- AWS Agentless Discovery Connector, or the AWS Application Discovery
-- Agent.
--
-- /See:/ 'newApplicationState' smart constructor.
data ApplicationState = ApplicationState'
  { -- | The timestamp when the application status was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of an application.
    applicationStatus :: Prelude.Maybe ApplicationStatus,
    -- | The configurationId from the Application Discovery Service that uniquely
    -- identifies an application.
    applicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'applicationState_lastUpdatedTime' - The timestamp when the application status was last updated.
--
-- 'applicationStatus', 'applicationState_applicationStatus' - The current status of an application.
--
-- 'applicationId', 'applicationState_applicationId' - The configurationId from the Application Discovery Service that uniquely
-- identifies an application.
newApplicationState ::
  ApplicationState
newApplicationState =
  ApplicationState'
    { lastUpdatedTime =
        Prelude.Nothing,
      applicationStatus = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- | The timestamp when the application status was last updated.
applicationState_lastUpdatedTime :: Lens.Lens' ApplicationState (Prelude.Maybe Prelude.UTCTime)
applicationState_lastUpdatedTime = Lens.lens (\ApplicationState' {lastUpdatedTime} -> lastUpdatedTime) (\s@ApplicationState' {} a -> s {lastUpdatedTime = a} :: ApplicationState) Prelude.. Lens.mapping Core._Time

-- | The current status of an application.
applicationState_applicationStatus :: Lens.Lens' ApplicationState (Prelude.Maybe ApplicationStatus)
applicationState_applicationStatus = Lens.lens (\ApplicationState' {applicationStatus} -> applicationStatus) (\s@ApplicationState' {} a -> s {applicationStatus = a} :: ApplicationState)

-- | The configurationId from the Application Discovery Service that uniquely
-- identifies an application.
applicationState_applicationId :: Lens.Lens' ApplicationState (Prelude.Maybe Prelude.Text)
applicationState_applicationId = Lens.lens (\ApplicationState' {applicationId} -> applicationId) (\s@ApplicationState' {} a -> s {applicationId = a} :: ApplicationState)

instance Core.FromJSON ApplicationState where
  parseJSON =
    Core.withObject
      "ApplicationState"
      ( \x ->
          ApplicationState'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "ApplicationStatus")
            Prelude.<*> (x Core..:? "ApplicationId")
      )

instance Prelude.Hashable ApplicationState where
  hashWithSalt _salt ApplicationState' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` applicationStatus
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ApplicationState where
  rnf ApplicationState' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf applicationStatus
      `Prelude.seq` Prelude.rnf applicationId

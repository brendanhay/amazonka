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
-- Module      : Amazonka.M2.Types.ApplicationVersionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.ApplicationVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types.ApplicationVersionLifecycle
import qualified Amazonka.Prelude as Prelude

-- | Defines an application version summary.
--
-- /See:/ 'newApplicationVersionSummary' smart constructor.
data ApplicationVersionSummary = ApplicationVersionSummary'
  { -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The application version.
    applicationVersion :: Prelude.Natural,
    -- | The timestamp when the application version was created.
    creationTime :: Data.POSIX,
    -- | The status of the application.
    status :: ApplicationVersionLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'applicationVersionSummary_statusReason' - The reason for the reported status.
--
-- 'applicationVersion', 'applicationVersionSummary_applicationVersion' - The application version.
--
-- 'creationTime', 'applicationVersionSummary_creationTime' - The timestamp when the application version was created.
--
-- 'status', 'applicationVersionSummary_status' - The status of the application.
newApplicationVersionSummary ::
  -- | 'applicationVersion'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'status'
  ApplicationVersionLifecycle ->
  ApplicationVersionSummary
newApplicationVersionSummary
  pApplicationVersion_
  pCreationTime_
  pStatus_ =
    ApplicationVersionSummary'
      { statusReason =
          Prelude.Nothing,
        applicationVersion = pApplicationVersion_,
        creationTime = Data._Time Lens.# pCreationTime_,
        status = pStatus_
      }

-- | The reason for the reported status.
applicationVersionSummary_statusReason :: Lens.Lens' ApplicationVersionSummary (Prelude.Maybe Prelude.Text)
applicationVersionSummary_statusReason = Lens.lens (\ApplicationVersionSummary' {statusReason} -> statusReason) (\s@ApplicationVersionSummary' {} a -> s {statusReason = a} :: ApplicationVersionSummary)

-- | The application version.
applicationVersionSummary_applicationVersion :: Lens.Lens' ApplicationVersionSummary Prelude.Natural
applicationVersionSummary_applicationVersion = Lens.lens (\ApplicationVersionSummary' {applicationVersion} -> applicationVersion) (\s@ApplicationVersionSummary' {} a -> s {applicationVersion = a} :: ApplicationVersionSummary)

-- | The timestamp when the application version was created.
applicationVersionSummary_creationTime :: Lens.Lens' ApplicationVersionSummary Prelude.UTCTime
applicationVersionSummary_creationTime = Lens.lens (\ApplicationVersionSummary' {creationTime} -> creationTime) (\s@ApplicationVersionSummary' {} a -> s {creationTime = a} :: ApplicationVersionSummary) Prelude.. Data._Time

-- | The status of the application.
applicationVersionSummary_status :: Lens.Lens' ApplicationVersionSummary ApplicationVersionLifecycle
applicationVersionSummary_status = Lens.lens (\ApplicationVersionSummary' {status} -> status) (\s@ApplicationVersionSummary' {} a -> s {status = a} :: ApplicationVersionSummary)

instance Data.FromJSON ApplicationVersionSummary where
  parseJSON =
    Data.withObject
      "ApplicationVersionSummary"
      ( \x ->
          ApplicationVersionSummary'
            Prelude.<$> (x Data..:? "statusReason")
            Prelude.<*> (x Data..: "applicationVersion")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ApplicationVersionSummary where
  hashWithSalt _salt ApplicationVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ApplicationVersionSummary where
  rnf ApplicationVersionSummary' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf status

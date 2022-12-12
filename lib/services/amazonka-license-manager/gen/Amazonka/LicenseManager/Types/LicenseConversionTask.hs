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
-- Module      : Amazonka.LicenseManager.Types.LicenseConversionTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseConversionTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.LicenseConversionContext
import Amazonka.LicenseManager.Types.LicenseConversionTaskStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a license type conversion task.
--
-- /See:/ 'newLicenseConversionTask' smart constructor.
data LicenseConversionTask = LicenseConversionTask'
  { -- | Information about the license type this conversion task converted to.
    destinationLicenseContext :: Prelude.Maybe LicenseConversionContext,
    -- | The time the conversion task was completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the license type conversion task.
    licenseConversionTaskId :: Prelude.Maybe Prelude.Text,
    -- | The time the usage operation value of the resource was changed.
    licenseConversionTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource associated with the
    -- license type conversion task.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the license type this conversion task converted from.
    sourceLicenseContext :: Prelude.Maybe LicenseConversionContext,
    -- | The time the conversion task was started at.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the conversion task.
    status :: Prelude.Maybe LicenseConversionTaskStatus,
    -- | The status message for the conversion task.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseConversionTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationLicenseContext', 'licenseConversionTask_destinationLicenseContext' - Information about the license type this conversion task converted to.
--
-- 'endTime', 'licenseConversionTask_endTime' - The time the conversion task was completed.
--
-- 'licenseConversionTaskId', 'licenseConversionTask_licenseConversionTaskId' - The ID of the license type conversion task.
--
-- 'licenseConversionTime', 'licenseConversionTask_licenseConversionTime' - The time the usage operation value of the resource was changed.
--
-- 'resourceArn', 'licenseConversionTask_resourceArn' - The Amazon Resource Name (ARN) of the resource associated with the
-- license type conversion task.
--
-- 'sourceLicenseContext', 'licenseConversionTask_sourceLicenseContext' - Information about the license type this conversion task converted from.
--
-- 'startTime', 'licenseConversionTask_startTime' - The time the conversion task was started at.
--
-- 'status', 'licenseConversionTask_status' - The status of the conversion task.
--
-- 'statusMessage', 'licenseConversionTask_statusMessage' - The status message for the conversion task.
newLicenseConversionTask ::
  LicenseConversionTask
newLicenseConversionTask =
  LicenseConversionTask'
    { destinationLicenseContext =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      licenseConversionTaskId = Prelude.Nothing,
      licenseConversionTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      sourceLicenseContext = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Information about the license type this conversion task converted to.
licenseConversionTask_destinationLicenseContext :: Lens.Lens' LicenseConversionTask (Prelude.Maybe LicenseConversionContext)
licenseConversionTask_destinationLicenseContext = Lens.lens (\LicenseConversionTask' {destinationLicenseContext} -> destinationLicenseContext) (\s@LicenseConversionTask' {} a -> s {destinationLicenseContext = a} :: LicenseConversionTask)

-- | The time the conversion task was completed.
licenseConversionTask_endTime :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.UTCTime)
licenseConversionTask_endTime = Lens.lens (\LicenseConversionTask' {endTime} -> endTime) (\s@LicenseConversionTask' {} a -> s {endTime = a} :: LicenseConversionTask) Prelude.. Lens.mapping Data._Time

-- | The ID of the license type conversion task.
licenseConversionTask_licenseConversionTaskId :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.Text)
licenseConversionTask_licenseConversionTaskId = Lens.lens (\LicenseConversionTask' {licenseConversionTaskId} -> licenseConversionTaskId) (\s@LicenseConversionTask' {} a -> s {licenseConversionTaskId = a} :: LicenseConversionTask)

-- | The time the usage operation value of the resource was changed.
licenseConversionTask_licenseConversionTime :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.UTCTime)
licenseConversionTask_licenseConversionTime = Lens.lens (\LicenseConversionTask' {licenseConversionTime} -> licenseConversionTime) (\s@LicenseConversionTask' {} a -> s {licenseConversionTime = a} :: LicenseConversionTask) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the resource associated with the
-- license type conversion task.
licenseConversionTask_resourceArn :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.Text)
licenseConversionTask_resourceArn = Lens.lens (\LicenseConversionTask' {resourceArn} -> resourceArn) (\s@LicenseConversionTask' {} a -> s {resourceArn = a} :: LicenseConversionTask)

-- | Information about the license type this conversion task converted from.
licenseConversionTask_sourceLicenseContext :: Lens.Lens' LicenseConversionTask (Prelude.Maybe LicenseConversionContext)
licenseConversionTask_sourceLicenseContext = Lens.lens (\LicenseConversionTask' {sourceLicenseContext} -> sourceLicenseContext) (\s@LicenseConversionTask' {} a -> s {sourceLicenseContext = a} :: LicenseConversionTask)

-- | The time the conversion task was started at.
licenseConversionTask_startTime :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.UTCTime)
licenseConversionTask_startTime = Lens.lens (\LicenseConversionTask' {startTime} -> startTime) (\s@LicenseConversionTask' {} a -> s {startTime = a} :: LicenseConversionTask) Prelude.. Lens.mapping Data._Time

-- | The status of the conversion task.
licenseConversionTask_status :: Lens.Lens' LicenseConversionTask (Prelude.Maybe LicenseConversionTaskStatus)
licenseConversionTask_status = Lens.lens (\LicenseConversionTask' {status} -> status) (\s@LicenseConversionTask' {} a -> s {status = a} :: LicenseConversionTask)

-- | The status message for the conversion task.
licenseConversionTask_statusMessage :: Lens.Lens' LicenseConversionTask (Prelude.Maybe Prelude.Text)
licenseConversionTask_statusMessage = Lens.lens (\LicenseConversionTask' {statusMessage} -> statusMessage) (\s@LicenseConversionTask' {} a -> s {statusMessage = a} :: LicenseConversionTask)

instance Data.FromJSON LicenseConversionTask where
  parseJSON =
    Data.withObject
      "LicenseConversionTask"
      ( \x ->
          LicenseConversionTask'
            Prelude.<$> (x Data..:? "DestinationLicenseContext")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "LicenseConversionTaskId")
            Prelude.<*> (x Data..:? "LicenseConversionTime")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "SourceLicenseContext")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable LicenseConversionTask where
  hashWithSalt _salt LicenseConversionTask' {..} =
    _salt
      `Prelude.hashWithSalt` destinationLicenseContext
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` licenseConversionTaskId
      `Prelude.hashWithSalt` licenseConversionTime
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` sourceLicenseContext
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData LicenseConversionTask where
  rnf LicenseConversionTask' {..} =
    Prelude.rnf destinationLicenseContext
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf licenseConversionTaskId
      `Prelude.seq` Prelude.rnf licenseConversionTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf sourceLicenseContext
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage

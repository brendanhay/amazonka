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
-- Module      : Amazonka.Forecast.Types.WhatIfForecastExportSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.WhatIfForecastExportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataDestination
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the what-if forecast export properties used in the
-- ListWhatIfForecastExports operation. To get the complete set of
-- properties, call the DescribeWhatIfForecastExport operation, and provide
-- the @WhatIfForecastExportArn@ that is listed in the summary.
--
-- /See:/ 'newWhatIfForecastExportSummary' smart constructor.
data WhatIfForecastExportSummary = WhatIfForecastExportSummary'
  { -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
    -- the forecast is exported.
    destination :: Prelude.Maybe DataDestination,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The what-if forecast export name.
    whatIfForecastExportName :: Prelude.Maybe Prelude.Text,
    -- | An array of Amazon Resource Names (ARNs) that define the what-if
    -- forecasts included in the export.
    whatIfForecastArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the what-if forecast export.
    whatIfForecastExportArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the what-if forecast export. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
    -- access the analysis.
    status :: Prelude.Maybe Prelude.Text,
    -- | When the what-if forecast export was created.
    creationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WhatIfForecastExportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'whatIfForecastExportSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'destination', 'whatIfForecastExportSummary_destination' - The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
--
-- 'message', 'whatIfForecastExportSummary_message' - If an error occurred, an informational message about the error.
--
-- 'whatIfForecastExportName', 'whatIfForecastExportSummary_whatIfForecastExportName' - The what-if forecast export name.
--
-- 'whatIfForecastArns', 'whatIfForecastExportSummary_whatIfForecastArns' - An array of Amazon Resource Names (ARNs) that define the what-if
-- forecasts included in the export.
--
-- 'whatIfForecastExportArn', 'whatIfForecastExportSummary_whatIfForecastExportArn' - The Amazon Resource Name (ARN) of the what-if forecast export.
--
-- 'status', 'whatIfForecastExportSummary_status' - The status of the what-if forecast export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
--
-- 'creationTime', 'whatIfForecastExportSummary_creationTime' - When the what-if forecast export was created.
newWhatIfForecastExportSummary ::
  WhatIfForecastExportSummary
newWhatIfForecastExportSummary =
  WhatIfForecastExportSummary'
    { lastModificationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      message = Prelude.Nothing,
      whatIfForecastExportName = Prelude.Nothing,
      whatIfForecastArns = Prelude.Nothing,
      whatIfForecastExportArn = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
whatIfForecastExportSummary_lastModificationTime :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.UTCTime)
whatIfForecastExportSummary_lastModificationTime = Lens.lens (\WhatIfForecastExportSummary' {lastModificationTime} -> lastModificationTime) (\s@WhatIfForecastExportSummary' {} a -> s {lastModificationTime = a} :: WhatIfForecastExportSummary) Prelude.. Lens.mapping Data._Time

-- | The path to the Amazon Simple Storage Service (Amazon S3) bucket where
-- the forecast is exported.
whatIfForecastExportSummary_destination :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe DataDestination)
whatIfForecastExportSummary_destination = Lens.lens (\WhatIfForecastExportSummary' {destination} -> destination) (\s@WhatIfForecastExportSummary' {} a -> s {destination = a} :: WhatIfForecastExportSummary)

-- | If an error occurred, an informational message about the error.
whatIfForecastExportSummary_message :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.Text)
whatIfForecastExportSummary_message = Lens.lens (\WhatIfForecastExportSummary' {message} -> message) (\s@WhatIfForecastExportSummary' {} a -> s {message = a} :: WhatIfForecastExportSummary)

-- | The what-if forecast export name.
whatIfForecastExportSummary_whatIfForecastExportName :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.Text)
whatIfForecastExportSummary_whatIfForecastExportName = Lens.lens (\WhatIfForecastExportSummary' {whatIfForecastExportName} -> whatIfForecastExportName) (\s@WhatIfForecastExportSummary' {} a -> s {whatIfForecastExportName = a} :: WhatIfForecastExportSummary)

-- | An array of Amazon Resource Names (ARNs) that define the what-if
-- forecasts included in the export.
whatIfForecastExportSummary_whatIfForecastArns :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
whatIfForecastExportSummary_whatIfForecastArns = Lens.lens (\WhatIfForecastExportSummary' {whatIfForecastArns} -> whatIfForecastArns) (\s@WhatIfForecastExportSummary' {} a -> s {whatIfForecastArns = a} :: WhatIfForecastExportSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the what-if forecast export.
whatIfForecastExportSummary_whatIfForecastExportArn :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.Text)
whatIfForecastExportSummary_whatIfForecastExportArn = Lens.lens (\WhatIfForecastExportSummary' {whatIfForecastExportArn} -> whatIfForecastExportArn) (\s@WhatIfForecastExportSummary' {} a -> s {whatIfForecastExportArn = a} :: WhatIfForecastExportSummary)

-- | The status of the what-if forecast export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
whatIfForecastExportSummary_status :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.Text)
whatIfForecastExportSummary_status = Lens.lens (\WhatIfForecastExportSummary' {status} -> status) (\s@WhatIfForecastExportSummary' {} a -> s {status = a} :: WhatIfForecastExportSummary)

-- | When the what-if forecast export was created.
whatIfForecastExportSummary_creationTime :: Lens.Lens' WhatIfForecastExportSummary (Prelude.Maybe Prelude.UTCTime)
whatIfForecastExportSummary_creationTime = Lens.lens (\WhatIfForecastExportSummary' {creationTime} -> creationTime) (\s@WhatIfForecastExportSummary' {} a -> s {creationTime = a} :: WhatIfForecastExportSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON WhatIfForecastExportSummary where
  parseJSON =
    Data.withObject
      "WhatIfForecastExportSummary"
      ( \x ->
          WhatIfForecastExportSummary'
            Prelude.<$> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "WhatIfForecastExportName")
            Prelude.<*> (x Data..:? "WhatIfForecastArns")
            Prelude.<*> (x Data..:? "WhatIfForecastExportArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "CreationTime")
      )

instance Prelude.Hashable WhatIfForecastExportSummary where
  hashWithSalt _salt WhatIfForecastExportSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` whatIfForecastExportName
      `Prelude.hashWithSalt` whatIfForecastArns
      `Prelude.hashWithSalt` whatIfForecastExportArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData WhatIfForecastExportSummary where
  rnf WhatIfForecastExportSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf whatIfForecastExportName
      `Prelude.seq` Prelude.rnf whatIfForecastArns
      `Prelude.seq` Prelude.rnf whatIfForecastExportArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime

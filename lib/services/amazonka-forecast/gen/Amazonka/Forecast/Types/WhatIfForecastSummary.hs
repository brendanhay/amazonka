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
-- Module      : Amazonka.Forecast.Types.WhatIfForecastSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.WhatIfForecastSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the what-if forecast properties used in the
-- ListWhatIfForecasts operation. To get the complete set of properties,
-- call the DescribeWhatIfForecast operation, and provide the
-- @WhatIfForecastArn@ that is listed in the summary.
--
-- /See:/ 'newWhatIfForecastSummary' smart constructor.
data WhatIfForecastSummary = WhatIfForecastSummary'
  { -- | When the what-if forecast was created.
    creationTime :: Prelude.Maybe Data.POSIX,
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
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the what-if forecast. States include:
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
    -- | The Amazon Resource Name (ARN) of the what-if analysis that contains
    -- this what-if forecast.
    whatIfAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the what-if forecast.
    whatIfForecastArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the what-if forecast.
    whatIfForecastName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WhatIfForecastSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'whatIfForecastSummary_creationTime' - When the what-if forecast was created.
--
-- 'lastModificationTime', 'whatIfForecastSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'whatIfForecastSummary_message' - If an error occurred, an informational message about the error.
--
-- 'status', 'whatIfForecastSummary_status' - The status of the what-if forecast. States include:
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
-- 'whatIfAnalysisArn', 'whatIfForecastSummary_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis that contains
-- this what-if forecast.
--
-- 'whatIfForecastArn', 'whatIfForecastSummary_whatIfForecastArn' - The Amazon Resource Name (ARN) of the what-if forecast.
--
-- 'whatIfForecastName', 'whatIfForecastSummary_whatIfForecastName' - The name of the what-if forecast.
newWhatIfForecastSummary ::
  WhatIfForecastSummary
newWhatIfForecastSummary =
  WhatIfForecastSummary'
    { creationTime =
        Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing,
      whatIfAnalysisArn = Prelude.Nothing,
      whatIfForecastArn = Prelude.Nothing,
      whatIfForecastName = Prelude.Nothing
    }

-- | When the what-if forecast was created.
whatIfForecastSummary_creationTime :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.UTCTime)
whatIfForecastSummary_creationTime = Lens.lens (\WhatIfForecastSummary' {creationTime} -> creationTime) (\s@WhatIfForecastSummary' {} a -> s {creationTime = a} :: WhatIfForecastSummary) Prelude.. Lens.mapping Data._Time

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
whatIfForecastSummary_lastModificationTime :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.UTCTime)
whatIfForecastSummary_lastModificationTime = Lens.lens (\WhatIfForecastSummary' {lastModificationTime} -> lastModificationTime) (\s@WhatIfForecastSummary' {} a -> s {lastModificationTime = a} :: WhatIfForecastSummary) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, an informational message about the error.
whatIfForecastSummary_message :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.Text)
whatIfForecastSummary_message = Lens.lens (\WhatIfForecastSummary' {message} -> message) (\s@WhatIfForecastSummary' {} a -> s {message = a} :: WhatIfForecastSummary)

-- | The status of the what-if forecast. States include:
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
whatIfForecastSummary_status :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.Text)
whatIfForecastSummary_status = Lens.lens (\WhatIfForecastSummary' {status} -> status) (\s@WhatIfForecastSummary' {} a -> s {status = a} :: WhatIfForecastSummary)

-- | The Amazon Resource Name (ARN) of the what-if analysis that contains
-- this what-if forecast.
whatIfForecastSummary_whatIfAnalysisArn :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.Text)
whatIfForecastSummary_whatIfAnalysisArn = Lens.lens (\WhatIfForecastSummary' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@WhatIfForecastSummary' {} a -> s {whatIfAnalysisArn = a} :: WhatIfForecastSummary)

-- | The Amazon Resource Name (ARN) of the what-if forecast.
whatIfForecastSummary_whatIfForecastArn :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.Text)
whatIfForecastSummary_whatIfForecastArn = Lens.lens (\WhatIfForecastSummary' {whatIfForecastArn} -> whatIfForecastArn) (\s@WhatIfForecastSummary' {} a -> s {whatIfForecastArn = a} :: WhatIfForecastSummary)

-- | The name of the what-if forecast.
whatIfForecastSummary_whatIfForecastName :: Lens.Lens' WhatIfForecastSummary (Prelude.Maybe Prelude.Text)
whatIfForecastSummary_whatIfForecastName = Lens.lens (\WhatIfForecastSummary' {whatIfForecastName} -> whatIfForecastName) (\s@WhatIfForecastSummary' {} a -> s {whatIfForecastName = a} :: WhatIfForecastSummary)

instance Data.FromJSON WhatIfForecastSummary where
  parseJSON =
    Data.withObject
      "WhatIfForecastSummary"
      ( \x ->
          WhatIfForecastSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "WhatIfAnalysisArn")
            Prelude.<*> (x Data..:? "WhatIfForecastArn")
            Prelude.<*> (x Data..:? "WhatIfForecastName")
      )

instance Prelude.Hashable WhatIfForecastSummary where
  hashWithSalt _salt WhatIfForecastSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` whatIfAnalysisArn
      `Prelude.hashWithSalt` whatIfForecastArn
      `Prelude.hashWithSalt` whatIfForecastName

instance Prelude.NFData WhatIfForecastSummary where
  rnf WhatIfForecastSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf whatIfAnalysisArn
      `Prelude.seq` Prelude.rnf whatIfForecastArn
      `Prelude.seq` Prelude.rnf whatIfForecastName

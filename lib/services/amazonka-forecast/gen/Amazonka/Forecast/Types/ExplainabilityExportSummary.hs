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
-- Module      : Amazonka.Forecast.Types.ExplainabilityExportSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ExplainabilityExportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.DataDestination
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the Explainability export properties used in the
-- ListExplainabilityExports operation. To get a complete set of
-- properties, call the DescribeExplainabilityExport operation, and provide
-- the @ExplainabilityExportArn@.
--
-- /See:/ 'newExplainabilityExportSummary' smart constructor.
data ExplainabilityExportSummary = ExplainabilityExportSummary'
  { -- | When the Explainability was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    destination :: Prelude.Maybe DataDestination,
    -- | The Amazon Resource Name (ARN) of the Explainability export.
    explainabilityExportArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Explainability export
    explainabilityExportName :: Prelude.Maybe Prelude.Text,
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
    -- | Information about any errors that may have occurred during the
    -- Explainability export.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the Explainability export. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplainabilityExportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'explainabilityExportSummary_creationTime' - When the Explainability was created.
--
-- 'destination', 'explainabilityExportSummary_destination' - Undocumented member.
--
-- 'explainabilityExportArn', 'explainabilityExportSummary_explainabilityExportArn' - The Amazon Resource Name (ARN) of the Explainability export.
--
-- 'explainabilityExportName', 'explainabilityExportSummary_explainabilityExportName' - The name of the Explainability export
--
-- 'lastModificationTime', 'explainabilityExportSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
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
-- 'message', 'explainabilityExportSummary_message' - Information about any errors that may have occurred during the
-- Explainability export.
--
-- 'status', 'explainabilityExportSummary_status' - The status of the Explainability export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
newExplainabilityExportSummary ::
  ExplainabilityExportSummary
newExplainabilityExportSummary =
  ExplainabilityExportSummary'
    { creationTime =
        Prelude.Nothing,
      destination = Prelude.Nothing,
      explainabilityExportArn = Prelude.Nothing,
      explainabilityExportName = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | When the Explainability was created.
explainabilityExportSummary_creationTime :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.UTCTime)
explainabilityExportSummary_creationTime = Lens.lens (\ExplainabilityExportSummary' {creationTime} -> creationTime) (\s@ExplainabilityExportSummary' {} a -> s {creationTime = a} :: ExplainabilityExportSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
explainabilityExportSummary_destination :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe DataDestination)
explainabilityExportSummary_destination = Lens.lens (\ExplainabilityExportSummary' {destination} -> destination) (\s@ExplainabilityExportSummary' {} a -> s {destination = a} :: ExplainabilityExportSummary)

-- | The Amazon Resource Name (ARN) of the Explainability export.
explainabilityExportSummary_explainabilityExportArn :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.Text)
explainabilityExportSummary_explainabilityExportArn = Lens.lens (\ExplainabilityExportSummary' {explainabilityExportArn} -> explainabilityExportArn) (\s@ExplainabilityExportSummary' {} a -> s {explainabilityExportArn = a} :: ExplainabilityExportSummary)

-- | The name of the Explainability export
explainabilityExportSummary_explainabilityExportName :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.Text)
explainabilityExportSummary_explainabilityExportName = Lens.lens (\ExplainabilityExportSummary' {explainabilityExportName} -> explainabilityExportName) (\s@ExplainabilityExportSummary' {} a -> s {explainabilityExportName = a} :: ExplainabilityExportSummary)

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
explainabilityExportSummary_lastModificationTime :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.UTCTime)
explainabilityExportSummary_lastModificationTime = Lens.lens (\ExplainabilityExportSummary' {lastModificationTime} -> lastModificationTime) (\s@ExplainabilityExportSummary' {} a -> s {lastModificationTime = a} :: ExplainabilityExportSummary) Prelude.. Lens.mapping Data._Time

-- | Information about any errors that may have occurred during the
-- Explainability export.
explainabilityExportSummary_message :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.Text)
explainabilityExportSummary_message = Lens.lens (\ExplainabilityExportSummary' {message} -> message) (\s@ExplainabilityExportSummary' {} a -> s {message = a} :: ExplainabilityExportSummary)

-- | The status of the Explainability export. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
explainabilityExportSummary_status :: Lens.Lens' ExplainabilityExportSummary (Prelude.Maybe Prelude.Text)
explainabilityExportSummary_status = Lens.lens (\ExplainabilityExportSummary' {status} -> status) (\s@ExplainabilityExportSummary' {} a -> s {status = a} :: ExplainabilityExportSummary)

instance Data.FromJSON ExplainabilityExportSummary where
  parseJSON =
    Data.withObject
      "ExplainabilityExportSummary"
      ( \x ->
          ExplainabilityExportSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "ExplainabilityExportArn")
            Prelude.<*> (x Data..:? "ExplainabilityExportName")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ExplainabilityExportSummary where
  hashWithSalt _salt ExplainabilityExportSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` explainabilityExportArn
      `Prelude.hashWithSalt` explainabilityExportName
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExplainabilityExportSummary where
  rnf ExplainabilityExportSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf explainabilityExportArn
      `Prelude.seq` Prelude.rnf explainabilityExportName
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf status

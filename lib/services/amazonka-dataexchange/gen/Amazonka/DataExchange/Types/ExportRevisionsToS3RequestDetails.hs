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
-- Module      : Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import Amazonka.DataExchange.Types.RevisionDestinationEntry
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newExportRevisionsToS3RequestDetails' smart constructor.
data ExportRevisionsToS3RequestDetails = ExportRevisionsToS3RequestDetails'
  { -- | Encryption configuration for the export job.
    encryption :: Prelude.Maybe ExportServerSideEncryption,
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text,
    -- | The destination for the revision.
    revisionDestinations :: [RevisionDestinationEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportRevisionsToS3RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'exportRevisionsToS3RequestDetails_encryption' - Encryption configuration for the export job.
--
-- 'dataSetId', 'exportRevisionsToS3RequestDetails_dataSetId' - The unique identifier for the data set associated with this export job.
--
-- 'revisionDestinations', 'exportRevisionsToS3RequestDetails_revisionDestinations' - The destination for the revision.
newExportRevisionsToS3RequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  ExportRevisionsToS3RequestDetails
newExportRevisionsToS3RequestDetails pDataSetId_ =
  ExportRevisionsToS3RequestDetails'
    { encryption =
        Prelude.Nothing,
      dataSetId = pDataSetId_,
      revisionDestinations = Prelude.mempty
    }

-- | Encryption configuration for the export job.
exportRevisionsToS3RequestDetails_encryption :: Lens.Lens' ExportRevisionsToS3RequestDetails (Prelude.Maybe ExportServerSideEncryption)
exportRevisionsToS3RequestDetails_encryption = Lens.lens (\ExportRevisionsToS3RequestDetails' {encryption} -> encryption) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {encryption = a} :: ExportRevisionsToS3RequestDetails)

-- | The unique identifier for the data set associated with this export job.
exportRevisionsToS3RequestDetails_dataSetId :: Lens.Lens' ExportRevisionsToS3RequestDetails Prelude.Text
exportRevisionsToS3RequestDetails_dataSetId = Lens.lens (\ExportRevisionsToS3RequestDetails' {dataSetId} -> dataSetId) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {dataSetId = a} :: ExportRevisionsToS3RequestDetails)

-- | The destination for the revision.
exportRevisionsToS3RequestDetails_revisionDestinations :: Lens.Lens' ExportRevisionsToS3RequestDetails [RevisionDestinationEntry]
exportRevisionsToS3RequestDetails_revisionDestinations = Lens.lens (\ExportRevisionsToS3RequestDetails' {revisionDestinations} -> revisionDestinations) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {revisionDestinations = a} :: ExportRevisionsToS3RequestDetails) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    ExportRevisionsToS3RequestDetails
  where
  hashWithSalt
    _salt
    ExportRevisionsToS3RequestDetails' {..} =
      _salt
        `Prelude.hashWithSalt` encryption
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionDestinations

instance
  Prelude.NFData
    ExportRevisionsToS3RequestDetails
  where
  rnf ExportRevisionsToS3RequestDetails' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionDestinations

instance
  Data.ToJSON
    ExportRevisionsToS3RequestDetails
  where
  toJSON ExportRevisionsToS3RequestDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Encryption" Data..=) Prelude.<$> encryption,
            Prelude.Just ("DataSetId" Data..= dataSetId),
            Prelude.Just
              ( "RevisionDestinations"
                  Data..= revisionDestinations
              )
          ]
      )

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
-- Module      : Amazonka.DataExchange.Types.AutoExportRevisionToS3RequestDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AutoExportRevisionToS3RequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.AutoExportRevisionDestinationEntry
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newAutoExportRevisionToS3RequestDetails' smart constructor.
data AutoExportRevisionToS3RequestDetails = AutoExportRevisionToS3RequestDetails'
  { -- | Encryption configuration for the auto export job.
    encryption :: Prelude.Maybe ExportServerSideEncryption,
    -- | A revision destination is the Amazon S3 bucket folder destination to
    -- where the export will be sent.
    revisionDestination :: AutoExportRevisionDestinationEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoExportRevisionToS3RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'autoExportRevisionToS3RequestDetails_encryption' - Encryption configuration for the auto export job.
--
-- 'revisionDestination', 'autoExportRevisionToS3RequestDetails_revisionDestination' - A revision destination is the Amazon S3 bucket folder destination to
-- where the export will be sent.
newAutoExportRevisionToS3RequestDetails ::
  -- | 'revisionDestination'
  AutoExportRevisionDestinationEntry ->
  AutoExportRevisionToS3RequestDetails
newAutoExportRevisionToS3RequestDetails
  pRevisionDestination_ =
    AutoExportRevisionToS3RequestDetails'
      { encryption =
          Prelude.Nothing,
        revisionDestination =
          pRevisionDestination_
      }

-- | Encryption configuration for the auto export job.
autoExportRevisionToS3RequestDetails_encryption :: Lens.Lens' AutoExportRevisionToS3RequestDetails (Prelude.Maybe ExportServerSideEncryption)
autoExportRevisionToS3RequestDetails_encryption = Lens.lens (\AutoExportRevisionToS3RequestDetails' {encryption} -> encryption) (\s@AutoExportRevisionToS3RequestDetails' {} a -> s {encryption = a} :: AutoExportRevisionToS3RequestDetails)

-- | A revision destination is the Amazon S3 bucket folder destination to
-- where the export will be sent.
autoExportRevisionToS3RequestDetails_revisionDestination :: Lens.Lens' AutoExportRevisionToS3RequestDetails AutoExportRevisionDestinationEntry
autoExportRevisionToS3RequestDetails_revisionDestination = Lens.lens (\AutoExportRevisionToS3RequestDetails' {revisionDestination} -> revisionDestination) (\s@AutoExportRevisionToS3RequestDetails' {} a -> s {revisionDestination = a} :: AutoExportRevisionToS3RequestDetails)

instance
  Data.FromJSON
    AutoExportRevisionToS3RequestDetails
  where
  parseJSON =
    Data.withObject
      "AutoExportRevisionToS3RequestDetails"
      ( \x ->
          AutoExportRevisionToS3RequestDetails'
            Prelude.<$> (x Data..:? "Encryption")
            Prelude.<*> (x Data..: "RevisionDestination")
      )

instance
  Prelude.Hashable
    AutoExportRevisionToS3RequestDetails
  where
  hashWithSalt
    _salt
    AutoExportRevisionToS3RequestDetails' {..} =
      _salt
        `Prelude.hashWithSalt` encryption
        `Prelude.hashWithSalt` revisionDestination

instance
  Prelude.NFData
    AutoExportRevisionToS3RequestDetails
  where
  rnf AutoExportRevisionToS3RequestDetails' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf revisionDestination

instance
  Data.ToJSON
    AutoExportRevisionToS3RequestDetails
  where
  toJSON AutoExportRevisionToS3RequestDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Encryption" Data..=) Prelude.<$> encryption,
            Prelude.Just
              ("RevisionDestination" Data..= revisionDestination)
          ]
      )

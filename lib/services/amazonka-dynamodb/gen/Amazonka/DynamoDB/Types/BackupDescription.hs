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
-- Module      : Amazonka.DynamoDB.Types.BackupDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BackupDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BackupDetails
import Amazonka.DynamoDB.Types.SourceTableDetails
import Amazonka.DynamoDB.Types.SourceTableFeatureDetails
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains the description of the backup created for the table.
--
-- /See:/ 'newBackupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { -- | Contains the details of the backup created for the table.
    backupDetails :: Prelude.Maybe BackupDetails,
    -- | Contains the details of the table when the backup was created.
    sourceTableDetails :: Prelude.Maybe SourceTableDetails,
    -- | Contains the details of the features enabled on the table when the
    -- backup was created. For example, LSIs, GSIs, streams, TTL.
    sourceTableFeatureDetails :: Prelude.Maybe SourceTableFeatureDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupDetails', 'backupDescription_backupDetails' - Contains the details of the backup created for the table.
--
-- 'sourceTableDetails', 'backupDescription_sourceTableDetails' - Contains the details of the table when the backup was created.
--
-- 'sourceTableFeatureDetails', 'backupDescription_sourceTableFeatureDetails' - Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
newBackupDescription ::
  BackupDescription
newBackupDescription =
  BackupDescription'
    { backupDetails = Prelude.Nothing,
      sourceTableDetails = Prelude.Nothing,
      sourceTableFeatureDetails = Prelude.Nothing
    }

-- | Contains the details of the backup created for the table.
backupDescription_backupDetails :: Lens.Lens' BackupDescription (Prelude.Maybe BackupDetails)
backupDescription_backupDetails = Lens.lens (\BackupDescription' {backupDetails} -> backupDetails) (\s@BackupDescription' {} a -> s {backupDetails = a} :: BackupDescription)

-- | Contains the details of the table when the backup was created.
backupDescription_sourceTableDetails :: Lens.Lens' BackupDescription (Prelude.Maybe SourceTableDetails)
backupDescription_sourceTableDetails = Lens.lens (\BackupDescription' {sourceTableDetails} -> sourceTableDetails) (\s@BackupDescription' {} a -> s {sourceTableDetails = a} :: BackupDescription)

-- | Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
backupDescription_sourceTableFeatureDetails :: Lens.Lens' BackupDescription (Prelude.Maybe SourceTableFeatureDetails)
backupDescription_sourceTableFeatureDetails = Lens.lens (\BackupDescription' {sourceTableFeatureDetails} -> sourceTableFeatureDetails) (\s@BackupDescription' {} a -> s {sourceTableFeatureDetails = a} :: BackupDescription)

instance Data.FromJSON BackupDescription where
  parseJSON =
    Data.withObject
      "BackupDescription"
      ( \x ->
          BackupDescription'
            Prelude.<$> (x Data..:? "BackupDetails")
            Prelude.<*> (x Data..:? "SourceTableDetails")
            Prelude.<*> (x Data..:? "SourceTableFeatureDetails")
      )

instance Prelude.Hashable BackupDescription where
  hashWithSalt _salt BackupDescription' {..} =
    _salt
      `Prelude.hashWithSalt` backupDetails
      `Prelude.hashWithSalt` sourceTableDetails
      `Prelude.hashWithSalt` sourceTableFeatureDetails

instance Prelude.NFData BackupDescription where
  rnf BackupDescription' {..} =
    Prelude.rnf backupDetails
      `Prelude.seq` Prelude.rnf sourceTableDetails
      `Prelude.seq` Prelude.rnf sourceTableFeatureDetails

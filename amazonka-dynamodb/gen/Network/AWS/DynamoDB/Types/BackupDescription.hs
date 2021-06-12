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
-- Module      : Network.AWS.DynamoDB.Types.BackupDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import qualified Network.AWS.Lens as Lens

-- | Contains the description of the backup created for the table.
--
-- /See:/ 'newBackupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { -- | Contains the details of the table when the backup was created.
    sourceTableDetails :: Core.Maybe SourceTableDetails,
    -- | Contains the details of the backup created for the table.
    backupDetails :: Core.Maybe BackupDetails,
    -- | Contains the details of the features enabled on the table when the
    -- backup was created. For example, LSIs, GSIs, streams, TTL.
    sourceTableFeatureDetails :: Core.Maybe SourceTableFeatureDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BackupDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTableDetails', 'backupDescription_sourceTableDetails' - Contains the details of the table when the backup was created.
--
-- 'backupDetails', 'backupDescription_backupDetails' - Contains the details of the backup created for the table.
--
-- 'sourceTableFeatureDetails', 'backupDescription_sourceTableFeatureDetails' - Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
newBackupDescription ::
  BackupDescription
newBackupDescription =
  BackupDescription'
    { sourceTableDetails =
        Core.Nothing,
      backupDetails = Core.Nothing,
      sourceTableFeatureDetails = Core.Nothing
    }

-- | Contains the details of the table when the backup was created.
backupDescription_sourceTableDetails :: Lens.Lens' BackupDescription (Core.Maybe SourceTableDetails)
backupDescription_sourceTableDetails = Lens.lens (\BackupDescription' {sourceTableDetails} -> sourceTableDetails) (\s@BackupDescription' {} a -> s {sourceTableDetails = a} :: BackupDescription)

-- | Contains the details of the backup created for the table.
backupDescription_backupDetails :: Lens.Lens' BackupDescription (Core.Maybe BackupDetails)
backupDescription_backupDetails = Lens.lens (\BackupDescription' {backupDetails} -> backupDetails) (\s@BackupDescription' {} a -> s {backupDetails = a} :: BackupDescription)

-- | Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
backupDescription_sourceTableFeatureDetails :: Lens.Lens' BackupDescription (Core.Maybe SourceTableFeatureDetails)
backupDescription_sourceTableFeatureDetails = Lens.lens (\BackupDescription' {sourceTableFeatureDetails} -> sourceTableFeatureDetails) (\s@BackupDescription' {} a -> s {sourceTableFeatureDetails = a} :: BackupDescription)

instance Core.FromJSON BackupDescription where
  parseJSON =
    Core.withObject
      "BackupDescription"
      ( \x ->
          BackupDescription'
            Core.<$> (x Core..:? "SourceTableDetails")
            Core.<*> (x Core..:? "BackupDetails")
            Core.<*> (x Core..:? "SourceTableFeatureDetails")
      )

instance Core.Hashable BackupDescription

instance Core.NFData BackupDescription

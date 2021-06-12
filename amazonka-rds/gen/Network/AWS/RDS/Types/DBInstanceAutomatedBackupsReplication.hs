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
-- Module      : Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceAutomatedBackupsReplication where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Automated backups of a DB instance replicated to another AWS Region.
-- They consist of system backups, transaction logs, and database instance
-- properties.
--
-- /See:/ 'newDBInstanceAutomatedBackupsReplication' smart constructor.
data DBInstanceAutomatedBackupsReplication = DBInstanceAutomatedBackupsReplication'
  { -- | The Amazon Resource Name (ARN) of the replicated automated backups.
    dbInstanceAutomatedBackupsArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBInstanceAutomatedBackupsReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackupsArn', 'dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) of the replicated automated backups.
newDBInstanceAutomatedBackupsReplication ::
  DBInstanceAutomatedBackupsReplication
newDBInstanceAutomatedBackupsReplication =
  DBInstanceAutomatedBackupsReplication'
    { dbInstanceAutomatedBackupsArn =
        Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replicated automated backups.
dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn :: Lens.Lens' DBInstanceAutomatedBackupsReplication (Core.Maybe Core.Text)
dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn = Lens.lens (\DBInstanceAutomatedBackupsReplication' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DBInstanceAutomatedBackupsReplication' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DBInstanceAutomatedBackupsReplication)

instance
  Core.FromXML
    DBInstanceAutomatedBackupsReplication
  where
  parseXML x =
    DBInstanceAutomatedBackupsReplication'
      Core.<$> (x Core..@? "DBInstanceAutomatedBackupsArn")

instance
  Core.Hashable
    DBInstanceAutomatedBackupsReplication

instance
  Core.NFData
    DBInstanceAutomatedBackupsReplication

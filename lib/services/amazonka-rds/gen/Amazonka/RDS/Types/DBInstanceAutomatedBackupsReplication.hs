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
-- Module      : Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Automated backups of a DB instance replicated to another Amazon Web
-- Services Region. They consist of system backups, transaction logs, and
-- database instance properties.
--
-- /See:/ 'newDBInstanceAutomatedBackupsReplication' smart constructor.
data DBInstanceAutomatedBackupsReplication = DBInstanceAutomatedBackupsReplication'
  { -- | The Amazon Resource Name (ARN) of the replicated automated backups.
    dbInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replicated automated backups.
dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn :: Lens.Lens' DBInstanceAutomatedBackupsReplication (Prelude.Maybe Prelude.Text)
dbInstanceAutomatedBackupsReplication_dbInstanceAutomatedBackupsArn = Lens.lens (\DBInstanceAutomatedBackupsReplication' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DBInstanceAutomatedBackupsReplication' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DBInstanceAutomatedBackupsReplication)

instance
  Data.FromXML
    DBInstanceAutomatedBackupsReplication
  where
  parseXML x =
    DBInstanceAutomatedBackupsReplication'
      Prelude.<$> (x Data..@? "DBInstanceAutomatedBackupsArn")

instance
  Prelude.Hashable
    DBInstanceAutomatedBackupsReplication
  where
  hashWithSalt
    _salt
    DBInstanceAutomatedBackupsReplication' {..} =
      _salt
        `Prelude.hashWithSalt` dbInstanceAutomatedBackupsArn

instance
  Prelude.NFData
    DBInstanceAutomatedBackupsReplication
  where
  rnf DBInstanceAutomatedBackupsReplication' {..} =
    Prelude.rnf dbInstanceAutomatedBackupsArn

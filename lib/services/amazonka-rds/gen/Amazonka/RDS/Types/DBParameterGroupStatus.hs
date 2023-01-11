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
-- Module      : Amazonka.RDS.Types.DBParameterGroupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBParameterGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of the DB parameter group.
--
-- This data type is used as a response element in the following actions:
--
-- -   @CreateDBInstance@
--
-- -   @CreateDBInstanceReadReplica@
--
-- -   @DeleteDBInstance@
--
-- -   @ModifyDBInstance@
--
-- -   @RebootDBInstance@
--
-- -   @RestoreDBInstanceFromDBSnapshot@
--
-- /See:/ 'newDBParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
  { -- | The name of the DB parameter group.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBParameterGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'dbParameterGroupStatus_dbParameterGroupName' - The name of the DB parameter group.
--
-- 'parameterApplyStatus', 'dbParameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
newDBParameterGroupStatus ::
  DBParameterGroupStatus
newDBParameterGroupStatus =
  DBParameterGroupStatus'
    { dbParameterGroupName =
        Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing
    }

-- | The name of the DB parameter group.
dbParameterGroupStatus_dbParameterGroupName :: Lens.Lens' DBParameterGroupStatus (Prelude.Maybe Prelude.Text)
dbParameterGroupStatus_dbParameterGroupName = Lens.lens (\DBParameterGroupStatus' {dbParameterGroupName} -> dbParameterGroupName) (\s@DBParameterGroupStatus' {} a -> s {dbParameterGroupName = a} :: DBParameterGroupStatus)

-- | The status of parameter updates.
dbParameterGroupStatus_parameterApplyStatus :: Lens.Lens' DBParameterGroupStatus (Prelude.Maybe Prelude.Text)
dbParameterGroupStatus_parameterApplyStatus = Lens.lens (\DBParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@DBParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: DBParameterGroupStatus)

instance Data.FromXML DBParameterGroupStatus where
  parseXML x =
    DBParameterGroupStatus'
      Prelude.<$> (x Data..@? "DBParameterGroupName")
      Prelude.<*> (x Data..@? "ParameterApplyStatus")

instance Prelude.Hashable DBParameterGroupStatus where
  hashWithSalt _salt DBParameterGroupStatus' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` parameterApplyStatus

instance Prelude.NFData DBParameterGroupStatus where
  rnf DBParameterGroupStatus' {..} =
    Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf parameterApplyStatus

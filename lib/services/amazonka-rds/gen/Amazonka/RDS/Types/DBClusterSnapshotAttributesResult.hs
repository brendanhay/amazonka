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
-- Module      : Amazonka.RDS.Types.DBClusterSnapshotAttributesResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBClusterSnapshotAttributesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBClusterSnapshotAttribute

-- | Contains the results of a successful call to the
-- @DescribeDBClusterSnapshotAttributes@ API action.
--
-- Manual DB cluster snapshot attributes are used to authorize other Amazon
-- Web Services accounts to copy or restore a manual DB cluster snapshot.
-- For more information, see the @ModifyDBClusterSnapshotAttribute@ API
-- action.
--
-- /See:/ 'newDBClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { -- | The list of attributes and values for the manual DB cluster snapshot.
    dbClusterSnapshotAttributes :: Prelude.Maybe [DBClusterSnapshotAttribute],
    -- | The identifier of the manual DB cluster snapshot that the attributes
    -- apply to.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterSnapshotAttributesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotAttributes', 'dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes' - The list of attributes and values for the manual DB cluster snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier' - The identifier of the manual DB cluster snapshot that the attributes
-- apply to.
newDBClusterSnapshotAttributesResult ::
  DBClusterSnapshotAttributesResult
newDBClusterSnapshotAttributesResult =
  DBClusterSnapshotAttributesResult'
    { dbClusterSnapshotAttributes =
        Prelude.Nothing,
      dbClusterSnapshotIdentifier =
        Prelude.Nothing
    }

-- | The list of attributes and values for the manual DB cluster snapshot.
dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes :: Lens.Lens' DBClusterSnapshotAttributesResult (Prelude.Maybe [DBClusterSnapshotAttribute])
dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes = Lens.lens (\DBClusterSnapshotAttributesResult' {dbClusterSnapshotAttributes} -> dbClusterSnapshotAttributes) (\s@DBClusterSnapshotAttributesResult' {} a -> s {dbClusterSnapshotAttributes = a} :: DBClusterSnapshotAttributesResult) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the manual DB cluster snapshot that the attributes
-- apply to.
dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshotAttributesResult (Prelude.Maybe Prelude.Text)
dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier = Lens.lens (\DBClusterSnapshotAttributesResult' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DBClusterSnapshotAttributesResult' {} a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshotAttributesResult)

instance
  Data.FromXML
    DBClusterSnapshotAttributesResult
  where
  parseXML x =
    DBClusterSnapshotAttributesResult'
      Prelude.<$> ( x
                      Data..@? "DBClusterSnapshotAttributes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "DBClusterSnapshotAttribute")
                  )
      Prelude.<*> (x Data..@? "DBClusterSnapshotIdentifier")

instance
  Prelude.Hashable
    DBClusterSnapshotAttributesResult
  where
  hashWithSalt
    _salt
    DBClusterSnapshotAttributesResult' {..} =
      _salt
        `Prelude.hashWithSalt` dbClusterSnapshotAttributes
        `Prelude.hashWithSalt` dbClusterSnapshotIdentifier

instance
  Prelude.NFData
    DBClusterSnapshotAttributesResult
  where
  rnf DBClusterSnapshotAttributesResult' {..} =
    Prelude.rnf dbClusterSnapshotAttributes
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier

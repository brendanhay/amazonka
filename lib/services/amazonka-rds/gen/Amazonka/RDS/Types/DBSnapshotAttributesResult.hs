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
-- Module      : Amazonka.RDS.Types.DBSnapshotAttributesResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBSnapshotAttributesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBSnapshotAttribute

-- | Contains the results of a successful call to the
-- @DescribeDBSnapshotAttributes@ API action.
--
-- Manual DB snapshot attributes are used to authorize other Amazon Web
-- Services accounts to copy or restore a manual DB snapshot. For more
-- information, see the @ModifyDBSnapshotAttribute@ API action.
--
-- /See:/ 'newDBSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { -- | The list of attributes and values for the manual DB snapshot.
    dbSnapshotAttributes :: Prelude.Maybe [DBSnapshotAttribute],
    -- | The identifier of the manual DB snapshot that the attributes apply to.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSnapshotAttributesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotAttributes', 'dbSnapshotAttributesResult_dbSnapshotAttributes' - The list of attributes and values for the manual DB snapshot.
--
-- 'dbSnapshotIdentifier', 'dbSnapshotAttributesResult_dbSnapshotIdentifier' - The identifier of the manual DB snapshot that the attributes apply to.
newDBSnapshotAttributesResult ::
  DBSnapshotAttributesResult
newDBSnapshotAttributesResult =
  DBSnapshotAttributesResult'
    { dbSnapshotAttributes =
        Prelude.Nothing,
      dbSnapshotIdentifier = Prelude.Nothing
    }

-- | The list of attributes and values for the manual DB snapshot.
dbSnapshotAttributesResult_dbSnapshotAttributes :: Lens.Lens' DBSnapshotAttributesResult (Prelude.Maybe [DBSnapshotAttribute])
dbSnapshotAttributesResult_dbSnapshotAttributes = Lens.lens (\DBSnapshotAttributesResult' {dbSnapshotAttributes} -> dbSnapshotAttributes) (\s@DBSnapshotAttributesResult' {} a -> s {dbSnapshotAttributes = a} :: DBSnapshotAttributesResult) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the manual DB snapshot that the attributes apply to.
dbSnapshotAttributesResult_dbSnapshotIdentifier :: Lens.Lens' DBSnapshotAttributesResult (Prelude.Maybe Prelude.Text)
dbSnapshotAttributesResult_dbSnapshotIdentifier = Lens.lens (\DBSnapshotAttributesResult' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DBSnapshotAttributesResult' {} a -> s {dbSnapshotIdentifier = a} :: DBSnapshotAttributesResult)

instance Data.FromXML DBSnapshotAttributesResult where
  parseXML x =
    DBSnapshotAttributesResult'
      Prelude.<$> ( x
                      Data..@? "DBSnapshotAttributes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBSnapshotAttribute")
                  )
      Prelude.<*> (x Data..@? "DBSnapshotIdentifier")

instance Prelude.Hashable DBSnapshotAttributesResult where
  hashWithSalt _salt DBSnapshotAttributesResult' {..} =
    _salt
      `Prelude.hashWithSalt` dbSnapshotAttributes
      `Prelude.hashWithSalt` dbSnapshotIdentifier

instance Prelude.NFData DBSnapshotAttributesResult where
  rnf DBSnapshotAttributesResult' {..} =
    Prelude.rnf dbSnapshotAttributes
      `Prelude.seq` Prelude.rnf dbSnapshotIdentifier

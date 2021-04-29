{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttributesResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshotAttributesResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.DBSnapshotAttribute

-- | Contains the results of a successful call to the
-- @DescribeDBSnapshotAttributes@ API action.
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts
-- to copy or restore a manual DB snapshot. For more information, see the
-- @ModifyDBSnapshotAttribute@ API action.
--
-- /See:/ 'newDBSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { -- | The identifier of the manual DB snapshot that the attributes apply to.
    dbSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The list of attributes and values for the manual DB snapshot.
    dbSnapshotAttributes :: Prelude.Maybe [DBSnapshotAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DBSnapshotAttributesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotIdentifier', 'dbSnapshotAttributesResult_dbSnapshotIdentifier' - The identifier of the manual DB snapshot that the attributes apply to.
--
-- 'dbSnapshotAttributes', 'dbSnapshotAttributesResult_dbSnapshotAttributes' - The list of attributes and values for the manual DB snapshot.
newDBSnapshotAttributesResult ::
  DBSnapshotAttributesResult
newDBSnapshotAttributesResult =
  DBSnapshotAttributesResult'
    { dbSnapshotIdentifier =
        Prelude.Nothing,
      dbSnapshotAttributes = Prelude.Nothing
    }

-- | The identifier of the manual DB snapshot that the attributes apply to.
dbSnapshotAttributesResult_dbSnapshotIdentifier :: Lens.Lens' DBSnapshotAttributesResult (Prelude.Maybe Prelude.Text)
dbSnapshotAttributesResult_dbSnapshotIdentifier = Lens.lens (\DBSnapshotAttributesResult' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DBSnapshotAttributesResult' {} a -> s {dbSnapshotIdentifier = a} :: DBSnapshotAttributesResult)

-- | The list of attributes and values for the manual DB snapshot.
dbSnapshotAttributesResult_dbSnapshotAttributes :: Lens.Lens' DBSnapshotAttributesResult (Prelude.Maybe [DBSnapshotAttribute])
dbSnapshotAttributesResult_dbSnapshotAttributes = Lens.lens (\DBSnapshotAttributesResult' {dbSnapshotAttributes} -> dbSnapshotAttributes) (\s@DBSnapshotAttributesResult' {} a -> s {dbSnapshotAttributes = a} :: DBSnapshotAttributesResult) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML DBSnapshotAttributesResult where
  parseXML x =
    DBSnapshotAttributesResult'
      Prelude.<$> (x Prelude..@? "DBSnapshotIdentifier")
      Prelude.<*> ( x Prelude..@? "DBSnapshotAttributes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "DBSnapshotAttribute")
                  )

instance Prelude.Hashable DBSnapshotAttributesResult

instance Prelude.NFData DBSnapshotAttributesResult

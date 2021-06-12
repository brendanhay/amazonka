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
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute

-- | Contains the results of a successful call to the
-- @DescribeDBClusterSnapshotAttributes@ API action.
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS
-- accounts to copy or restore a manual DB cluster snapshot. For more
-- information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /See:/ 'newDBClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { -- | The list of attributes and values for the manual DB cluster snapshot.
    dbClusterSnapshotAttributes :: Core.Maybe [DBClusterSnapshotAttribute],
    -- | The identifier of the manual DB cluster snapshot that the attributes
    -- apply to.
    dbClusterSnapshotIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      dbClusterSnapshotIdentifier =
        Core.Nothing
    }

-- | The list of attributes and values for the manual DB cluster snapshot.
dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes :: Lens.Lens' DBClusterSnapshotAttributesResult (Core.Maybe [DBClusterSnapshotAttribute])
dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes = Lens.lens (\DBClusterSnapshotAttributesResult' {dbClusterSnapshotAttributes} -> dbClusterSnapshotAttributes) (\s@DBClusterSnapshotAttributesResult' {} a -> s {dbClusterSnapshotAttributes = a} :: DBClusterSnapshotAttributesResult) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the manual DB cluster snapshot that the attributes
-- apply to.
dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshotAttributesResult (Core.Maybe Core.Text)
dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier = Lens.lens (\DBClusterSnapshotAttributesResult' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DBClusterSnapshotAttributesResult' {} a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshotAttributesResult)

instance
  Core.FromXML
    DBClusterSnapshotAttributesResult
  where
  parseXML x =
    DBClusterSnapshotAttributesResult'
      Core.<$> ( x Core..@? "DBClusterSnapshotAttributes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may
                     (Core.parseXMLList "DBClusterSnapshotAttribute")
               )
      Core.<*> (x Core..@? "DBClusterSnapshotIdentifier")

instance
  Core.Hashable
    DBClusterSnapshotAttributesResult

instance
  Core.NFData
    DBClusterSnapshotAttributesResult

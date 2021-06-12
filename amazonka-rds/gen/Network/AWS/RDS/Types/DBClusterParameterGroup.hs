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
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
-- This data type is used as a response element in the
-- @DescribeDBClusterParameterGroups@ action.
--
-- /See:/ 'newDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
    dbClusterParameterGroupArn :: Core.Maybe Core.Text,
    -- | The name of the DB parameter group family that this DB cluster parameter
    -- group is compatible with.
    dbParameterGroupFamily :: Core.Maybe Core.Text,
    -- | Provides the customer-specified description for this DB cluster
    -- parameter group.
    description :: Core.Maybe Core.Text,
    -- | The name of the DB cluster parameter group.
    dbClusterParameterGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroupArn', 'dbClusterParameterGroup_dbClusterParameterGroupArn' - The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- 'dbParameterGroupFamily', 'dbClusterParameterGroup_dbParameterGroupFamily' - The name of the DB parameter group family that this DB cluster parameter
-- group is compatible with.
--
-- 'description', 'dbClusterParameterGroup_description' - Provides the customer-specified description for this DB cluster
-- parameter group.
--
-- 'dbClusterParameterGroupName', 'dbClusterParameterGroup_dbClusterParameterGroupName' - The name of the DB cluster parameter group.
newDBClusterParameterGroup ::
  DBClusterParameterGroup
newDBClusterParameterGroup =
  DBClusterParameterGroup'
    { dbClusterParameterGroupArn =
        Core.Nothing,
      dbParameterGroupFamily = Core.Nothing,
      description = Core.Nothing,
      dbClusterParameterGroupName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupArn :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Core.Text)
dbClusterParameterGroup_dbClusterParameterGroupArn = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupArn} -> dbClusterParameterGroupArn) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupArn = a} :: DBClusterParameterGroup)

-- | The name of the DB parameter group family that this DB cluster parameter
-- group is compatible with.
dbClusterParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Core.Text)
dbClusterParameterGroup_dbParameterGroupFamily = Lens.lens (\DBClusterParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBClusterParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBClusterParameterGroup)

-- | Provides the customer-specified description for this DB cluster
-- parameter group.
dbClusterParameterGroup_description :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Core.Text)
dbClusterParameterGroup_description = Lens.lens (\DBClusterParameterGroup' {description} -> description) (\s@DBClusterParameterGroup' {} a -> s {description = a} :: DBClusterParameterGroup)

-- | The name of the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Core.Text)
dbClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroup)

instance Core.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Core.<$> (x Core..@? "DBClusterParameterGroupArn")
      Core.<*> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DBClusterParameterGroupName")

instance Core.Hashable DBClusterParameterGroup

instance Core.NFData DBClusterParameterGroup

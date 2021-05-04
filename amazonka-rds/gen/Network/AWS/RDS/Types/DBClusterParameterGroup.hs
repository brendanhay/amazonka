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
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
-- This data type is used as a response element in the
-- @DescribeDBClusterParameterGroups@ action.
--
-- /See:/ 'newDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
    dbClusterParameterGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group family that this DB cluster parameter
    -- group is compatible with.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | Provides the customer-specified description for this DB cluster
    -- parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      description = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupArn :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupArn = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupArn} -> dbClusterParameterGroupArn) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupArn = a} :: DBClusterParameterGroup)

-- | The name of the DB parameter group family that this DB cluster parameter
-- group is compatible with.
dbClusterParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbParameterGroupFamily = Lens.lens (\DBClusterParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBClusterParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBClusterParameterGroup)

-- | Provides the customer-specified description for this DB cluster
-- parameter group.
dbClusterParameterGroup_description :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_description = Lens.lens (\DBClusterParameterGroup' {description} -> description) (\s@DBClusterParameterGroup' {} a -> s {description = a} :: DBClusterParameterGroup)

-- | The name of the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroup)

instance Prelude.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Prelude.<$> (x Prelude..@? "DBClusterParameterGroupArn")
      Prelude.<*> (x Prelude..@? "DBParameterGroupFamily")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "DBClusterParameterGroupName")

instance Prelude.Hashable DBClusterParameterGroup

instance Prelude.NFData DBClusterParameterGroup

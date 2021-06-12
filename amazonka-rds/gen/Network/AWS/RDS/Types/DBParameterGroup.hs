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
-- Module      : Network.AWS.RDS.Types.DBParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the details of an Amazon RDS DB parameter group.
--
-- This data type is used as a response element in the
-- @DescribeDBParameterGroups@ action.
--
-- /See:/ 'newDBParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB parameter group.
    dbParameterGroupArn :: Core.Maybe Core.Text,
    -- | The name of the DB parameter group.
    dbParameterGroupName :: Core.Maybe Core.Text,
    -- | The name of the DB parameter group family that this DB parameter group
    -- is compatible with.
    dbParameterGroupFamily :: Core.Maybe Core.Text,
    -- | Provides the customer-specified description for this DB parameter group.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupArn', 'dbParameterGroup_dbParameterGroupArn' - The Amazon Resource Name (ARN) for the DB parameter group.
--
-- 'dbParameterGroupName', 'dbParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- 'dbParameterGroupFamily', 'dbParameterGroup_dbParameterGroupFamily' - The name of the DB parameter group family that this DB parameter group
-- is compatible with.
--
-- 'description', 'dbParameterGroup_description' - Provides the customer-specified description for this DB parameter group.
newDBParameterGroup ::
  DBParameterGroup
newDBParameterGroup =
  DBParameterGroup'
    { dbParameterGroupArn =
        Core.Nothing,
      dbParameterGroupName = Core.Nothing,
      dbParameterGroupFamily = Core.Nothing,
      description = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB parameter group.
dbParameterGroup_dbParameterGroupArn :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbParameterGroup_dbParameterGroupArn = Lens.lens (\DBParameterGroup' {dbParameterGroupArn} -> dbParameterGroupArn) (\s@DBParameterGroup' {} a -> s {dbParameterGroupArn = a} :: DBParameterGroup)

-- | The name of the DB parameter group.
dbParameterGroup_dbParameterGroupName :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbParameterGroup_dbParameterGroupName = Lens.lens (\DBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@DBParameterGroup' {} a -> s {dbParameterGroupName = a} :: DBParameterGroup)

-- | The name of the DB parameter group family that this DB parameter group
-- is compatible with.
dbParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbParameterGroup_dbParameterGroupFamily = Lens.lens (\DBParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBParameterGroup)

-- | Provides the customer-specified description for this DB parameter group.
dbParameterGroup_description :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbParameterGroup_description = Lens.lens (\DBParameterGroup' {description} -> description) (\s@DBParameterGroup' {} a -> s {description = a} :: DBParameterGroup)

instance Core.FromXML DBParameterGroup where
  parseXML x =
    DBParameterGroup'
      Core.<$> (x Core..@? "DBParameterGroupArn")
      Core.<*> (x Core..@? "DBParameterGroupName")
      Core.<*> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "Description")

instance Core.Hashable DBParameterGroup

instance Core.NFData DBParameterGroup

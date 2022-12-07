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
-- Module      : Amazonka.RDS.Types.DBParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an Amazon RDS DB parameter group.
--
-- This data type is used as a response element in the
-- @DescribeDBParameterGroups@ action.
--
-- /See:/ 'newDBParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { -- | The name of the DB parameter group.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the customer-specified description for this DB parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group family that this DB parameter group
    -- is compatible with.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB parameter group.
    dbParameterGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'dbParameterGroup_dbParameterGroupName' - The name of the DB parameter group.
--
-- 'description', 'dbParameterGroup_description' - Provides the customer-specified description for this DB parameter group.
--
-- 'dbParameterGroupFamily', 'dbParameterGroup_dbParameterGroupFamily' - The name of the DB parameter group family that this DB parameter group
-- is compatible with.
--
-- 'dbParameterGroupArn', 'dbParameterGroup_dbParameterGroupArn' - The Amazon Resource Name (ARN) for the DB parameter group.
newDBParameterGroup ::
  DBParameterGroup
newDBParameterGroup =
  DBParameterGroup'
    { dbParameterGroupName =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      dbParameterGroupArn = Prelude.Nothing
    }

-- | The name of the DB parameter group.
dbParameterGroup_dbParameterGroupName :: Lens.Lens' DBParameterGroup (Prelude.Maybe Prelude.Text)
dbParameterGroup_dbParameterGroupName = Lens.lens (\DBParameterGroup' {dbParameterGroupName} -> dbParameterGroupName) (\s@DBParameterGroup' {} a -> s {dbParameterGroupName = a} :: DBParameterGroup)

-- | Provides the customer-specified description for this DB parameter group.
dbParameterGroup_description :: Lens.Lens' DBParameterGroup (Prelude.Maybe Prelude.Text)
dbParameterGroup_description = Lens.lens (\DBParameterGroup' {description} -> description) (\s@DBParameterGroup' {} a -> s {description = a} :: DBParameterGroup)

-- | The name of the DB parameter group family that this DB parameter group
-- is compatible with.
dbParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBParameterGroup (Prelude.Maybe Prelude.Text)
dbParameterGroup_dbParameterGroupFamily = Lens.lens (\DBParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBParameterGroup)

-- | The Amazon Resource Name (ARN) for the DB parameter group.
dbParameterGroup_dbParameterGroupArn :: Lens.Lens' DBParameterGroup (Prelude.Maybe Prelude.Text)
dbParameterGroup_dbParameterGroupArn = Lens.lens (\DBParameterGroup' {dbParameterGroupArn} -> dbParameterGroupArn) (\s@DBParameterGroup' {} a -> s {dbParameterGroupArn = a} :: DBParameterGroup)

instance Data.FromXML DBParameterGroup where
  parseXML x =
    DBParameterGroup'
      Prelude.<$> (x Data..@? "DBParameterGroupName")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "DBParameterGroupFamily")
      Prelude.<*> (x Data..@? "DBParameterGroupArn")

instance Prelude.Hashable DBParameterGroup where
  hashWithSalt _salt DBParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` dbParameterGroupArn

instance Prelude.NFData DBParameterGroup where
  rnf DBParameterGroup' {..} =
    Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf dbParameterGroupArn

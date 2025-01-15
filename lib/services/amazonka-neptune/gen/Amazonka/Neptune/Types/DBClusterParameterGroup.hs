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
-- Module      : Amazonka.Neptune.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBClusterParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an Amazon Neptune DB cluster parameter group.
--
-- This data type is used as a response element in the
-- DescribeDBClusterParameterGroups action.
--
-- /See:/ 'newDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
    dbClusterParameterGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the name of the DB cluster parameter group.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | Provides the name of the DB parameter group family that this DB cluster
    -- parameter group is compatible with.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | Provides the customer-specified description for this DB cluster
    -- parameter group.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dbClusterParameterGroupName', 'dbClusterParameterGroup_dbClusterParameterGroupName' - Provides the name of the DB cluster parameter group.
--
-- 'dbParameterGroupFamily', 'dbClusterParameterGroup_dbParameterGroupFamily' - Provides the name of the DB parameter group family that this DB cluster
-- parameter group is compatible with.
--
-- 'description', 'dbClusterParameterGroup_description' - Provides the customer-specified description for this DB cluster
-- parameter group.
newDBClusterParameterGroup ::
  DBClusterParameterGroup
newDBClusterParameterGroup =
  DBClusterParameterGroup'
    { dbClusterParameterGroupArn =
        Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupArn :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupArn = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupArn} -> dbClusterParameterGroupArn) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupArn = a} :: DBClusterParameterGroup)

-- | Provides the name of the DB cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroup)

-- | Provides the name of the DB parameter group family that this DB cluster
-- parameter group is compatible with.
dbClusterParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbParameterGroupFamily = Lens.lens (\DBClusterParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBClusterParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBClusterParameterGroup)

-- | Provides the customer-specified description for this DB cluster
-- parameter group.
dbClusterParameterGroup_description :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_description = Lens.lens (\DBClusterParameterGroup' {description} -> description) (\s@DBClusterParameterGroup' {} a -> s {description = a} :: DBClusterParameterGroup)

instance Data.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Prelude.<$> (x Data..@? "DBClusterParameterGroupArn")
      Prelude.<*> (x Data..@? "DBClusterParameterGroupName")
      Prelude.<*> (x Data..@? "DBParameterGroupFamily")
      Prelude.<*> (x Data..@? "Description")

instance Prelude.Hashable DBClusterParameterGroup where
  hashWithSalt _salt DBClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterParameterGroupArn
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` description

instance Prelude.NFData DBClusterParameterGroup where
  rnf DBClusterParameterGroup' {..} =
    Prelude.rnf dbClusterParameterGroupArn `Prelude.seq`
      Prelude.rnf dbClusterParameterGroupName `Prelude.seq`
        Prelude.rnf dbParameterGroupFamily `Prelude.seq`
          Prelude.rnf description

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
-- Module      : Amazonka.DocumentDB.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBClusterParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a cluster parameter group.
--
-- /See:/ 'newDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { -- | Provides the customer-specified description for this cluster parameter
    -- group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the cluster parameter group.
    dbClusterParameterGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the name of the parameter group family that this cluster
    -- parameter group is compatible with.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | Provides the name of the cluster parameter group.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text
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
-- 'description', 'dbClusterParameterGroup_description' - Provides the customer-specified description for this cluster parameter
-- group.
--
-- 'dbClusterParameterGroupArn', 'dbClusterParameterGroup_dbClusterParameterGroupArn' - The Amazon Resource Name (ARN) for the cluster parameter group.
--
-- 'dbParameterGroupFamily', 'dbClusterParameterGroup_dbParameterGroupFamily' - Provides the name of the parameter group family that this cluster
-- parameter group is compatible with.
--
-- 'dbClusterParameterGroupName', 'dbClusterParameterGroup_dbClusterParameterGroupName' - Provides the name of the cluster parameter group.
newDBClusterParameterGroup ::
  DBClusterParameterGroup
newDBClusterParameterGroup =
  DBClusterParameterGroup'
    { description =
        Prelude.Nothing,
      dbClusterParameterGroupArn = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing
    }

-- | Provides the customer-specified description for this cluster parameter
-- group.
dbClusterParameterGroup_description :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_description = Lens.lens (\DBClusterParameterGroup' {description} -> description) (\s@DBClusterParameterGroup' {} a -> s {description = a} :: DBClusterParameterGroup)

-- | The Amazon Resource Name (ARN) for the cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupArn :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupArn = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupArn} -> dbClusterParameterGroupArn) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupArn = a} :: DBClusterParameterGroup)

-- | Provides the name of the parameter group family that this cluster
-- parameter group is compatible with.
dbClusterParameterGroup_dbParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbParameterGroupFamily = Lens.lens (\DBClusterParameterGroup' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBClusterParameterGroup' {} a -> s {dbParameterGroupFamily = a} :: DBClusterParameterGroup)

-- | Provides the name of the cluster parameter group.
dbClusterParameterGroup_dbClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Prelude.Maybe Prelude.Text)
dbClusterParameterGroup_dbClusterParameterGroupName = Lens.lens (\DBClusterParameterGroup' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DBClusterParameterGroup' {} a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroup)

instance Core.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Prelude.<$> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "DBClusterParameterGroupArn")
      Prelude.<*> (x Core..@? "DBParameterGroupFamily")
      Prelude.<*> (x Core..@? "DBClusterParameterGroupName")

instance Prelude.Hashable DBClusterParameterGroup where
  hashWithSalt _salt DBClusterParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dbClusterParameterGroupArn
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` dbClusterParameterGroupName

instance Prelude.NFData DBClusterParameterGroup where
  rnf DBClusterParameterGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupArn
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName

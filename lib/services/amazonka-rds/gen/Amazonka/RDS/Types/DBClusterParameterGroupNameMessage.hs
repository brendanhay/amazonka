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
-- Module      : Amazonka.RDS.Types.DBClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBClusterParameterGroupNameMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newDBClusterParameterGroupNameMessage' smart constructor.
data DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters or numbers.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- This value is stored as a lowercase string.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterParameterGroupNameMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroupName', 'dbClusterParameterGroupNameMessage_dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
newDBClusterParameterGroupNameMessage ::
  DBClusterParameterGroupNameMessage
newDBClusterParameterGroupNameMessage =
  DBClusterParameterGroupNameMessage'
    { dbClusterParameterGroupName =
        Prelude.Nothing
    }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
dbClusterParameterGroupNameMessage_dbClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroupNameMessage (Prelude.Maybe Prelude.Text)
dbClusterParameterGroupNameMessage_dbClusterParameterGroupName = Lens.lens (\DBClusterParameterGroupNameMessage' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DBClusterParameterGroupNameMessage' {} a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroupNameMessage)

instance
  Data.FromXML
    DBClusterParameterGroupNameMessage
  where
  parseXML x =
    DBClusterParameterGroupNameMessage'
      Prelude.<$> (x Data..@? "DBClusterParameterGroupName")

instance
  Prelude.Hashable
    DBClusterParameterGroupNameMessage
  where
  hashWithSalt
    _salt
    DBClusterParameterGroupNameMessage' {..} =
      _salt
        `Prelude.hashWithSalt` dbClusterParameterGroupName

instance
  Prelude.NFData
    DBClusterParameterGroupNameMessage
  where
  rnf DBClusterParameterGroupNameMessage' {..} =
    Prelude.rnf dbClusterParameterGroupName

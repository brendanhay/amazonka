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
-- Module      : Amazonka.RDS.Types.DBParameterGroupNameMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBParameterGroupNameMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the result of a successful invocation of the
-- @ModifyDBParameterGroup@ or @ResetDBParameterGroup@ action.
--
-- /See:/ 'newDBParameterGroupNameMessage' smart constructor.
data DBParameterGroupNameMessage = DBParameterGroupNameMessage'
  { -- | The name of the DB parameter group.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBParameterGroupNameMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'dbParameterGroupNameMessage_dbParameterGroupName' - The name of the DB parameter group.
newDBParameterGroupNameMessage ::
  DBParameterGroupNameMessage
newDBParameterGroupNameMessage =
  DBParameterGroupNameMessage'
    { dbParameterGroupName =
        Prelude.Nothing
    }

-- | The name of the DB parameter group.
dbParameterGroupNameMessage_dbParameterGroupName :: Lens.Lens' DBParameterGroupNameMessage (Prelude.Maybe Prelude.Text)
dbParameterGroupNameMessage_dbParameterGroupName = Lens.lens (\DBParameterGroupNameMessage' {dbParameterGroupName} -> dbParameterGroupName) (\s@DBParameterGroupNameMessage' {} a -> s {dbParameterGroupName = a} :: DBParameterGroupNameMessage)

instance Data.FromXML DBParameterGroupNameMessage where
  parseXML x =
    DBParameterGroupNameMessage'
      Prelude.<$> (x Data..@? "DBParameterGroupName")

instance Prelude.Hashable DBParameterGroupNameMessage where
  hashWithSalt _salt DBParameterGroupNameMessage' {..} =
    _salt `Prelude.hashWithSalt` dbParameterGroupName

instance Prelude.NFData DBParameterGroupNameMessage where
  rnf DBParameterGroupNameMessage' {..} =
    Prelude.rnf dbParameterGroupName

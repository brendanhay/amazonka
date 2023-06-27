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
-- Module      : Amazonka.Neptune.Types.DBSecurityGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBSecurityGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies membership in a designated DB security group.
--
-- /See:/ 'newDBSecurityGroupMembership' smart constructor.
data DBSecurityGroupMembership = DBSecurityGroupMembership'
  { -- | The name of the DB security group.
    dbSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the DB security group.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBSecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupName', 'dbSecurityGroupMembership_dbSecurityGroupName' - The name of the DB security group.
--
-- 'status', 'dbSecurityGroupMembership_status' - The status of the DB security group.
newDBSecurityGroupMembership ::
  DBSecurityGroupMembership
newDBSecurityGroupMembership =
  DBSecurityGroupMembership'
    { dbSecurityGroupName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the DB security group.
dbSecurityGroupMembership_dbSecurityGroupName :: Lens.Lens' DBSecurityGroupMembership (Prelude.Maybe Prelude.Text)
dbSecurityGroupMembership_dbSecurityGroupName = Lens.lens (\DBSecurityGroupMembership' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DBSecurityGroupMembership' {} a -> s {dbSecurityGroupName = a} :: DBSecurityGroupMembership)

-- | The status of the DB security group.
dbSecurityGroupMembership_status :: Lens.Lens' DBSecurityGroupMembership (Prelude.Maybe Prelude.Text)
dbSecurityGroupMembership_status = Lens.lens (\DBSecurityGroupMembership' {status} -> status) (\s@DBSecurityGroupMembership' {} a -> s {status = a} :: DBSecurityGroupMembership)

instance Data.FromXML DBSecurityGroupMembership where
  parseXML x =
    DBSecurityGroupMembership'
      Prelude.<$> (x Data..@? "DBSecurityGroupName")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable DBSecurityGroupMembership where
  hashWithSalt _salt DBSecurityGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` dbSecurityGroupName
      `Prelude.hashWithSalt` status

instance Prelude.NFData DBSecurityGroupMembership where
  rnf DBSecurityGroupMembership' {..} =
    Prelude.rnf dbSecurityGroupName
      `Prelude.seq` Prelude.rnf status

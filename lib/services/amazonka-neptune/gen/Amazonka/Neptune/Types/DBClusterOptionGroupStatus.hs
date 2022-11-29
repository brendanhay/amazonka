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
-- Module      : Amazonka.Neptune.Types.DBClusterOptionGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBClusterOptionGroupStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Not supported by Neptune.
--
-- /See:/ 'newDBClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
  { -- | Not supported by Neptune.
    status :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    dbClusterOptionGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterOptionGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbClusterOptionGroupStatus_status' - Not supported by Neptune.
--
-- 'dbClusterOptionGroupName', 'dbClusterOptionGroupStatus_dbClusterOptionGroupName' - Not supported by Neptune.
newDBClusterOptionGroupStatus ::
  DBClusterOptionGroupStatus
newDBClusterOptionGroupStatus =
  DBClusterOptionGroupStatus'
    { status =
        Prelude.Nothing,
      dbClusterOptionGroupName = Prelude.Nothing
    }

-- | Not supported by Neptune.
dbClusterOptionGroupStatus_status :: Lens.Lens' DBClusterOptionGroupStatus (Prelude.Maybe Prelude.Text)
dbClusterOptionGroupStatus_status = Lens.lens (\DBClusterOptionGroupStatus' {status} -> status) (\s@DBClusterOptionGroupStatus' {} a -> s {status = a} :: DBClusterOptionGroupStatus)

-- | Not supported by Neptune.
dbClusterOptionGroupStatus_dbClusterOptionGroupName :: Lens.Lens' DBClusterOptionGroupStatus (Prelude.Maybe Prelude.Text)
dbClusterOptionGroupStatus_dbClusterOptionGroupName = Lens.lens (\DBClusterOptionGroupStatus' {dbClusterOptionGroupName} -> dbClusterOptionGroupName) (\s@DBClusterOptionGroupStatus' {} a -> s {dbClusterOptionGroupName = a} :: DBClusterOptionGroupStatus)

instance Core.FromXML DBClusterOptionGroupStatus where
  parseXML x =
    DBClusterOptionGroupStatus'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "DBClusterOptionGroupName")

instance Prelude.Hashable DBClusterOptionGroupStatus where
  hashWithSalt _salt DBClusterOptionGroupStatus' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` dbClusterOptionGroupName

instance Prelude.NFData DBClusterOptionGroupStatus where
  rnf DBClusterOptionGroupStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf dbClusterOptionGroupName

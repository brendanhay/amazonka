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
-- Module      : Network.AWS.RDS.Types.DBClusterOptionGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterOptionGroupStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains status information for a DB cluster option group.
--
-- /See:/ 'newDBClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
  { -- | Specifies the status of the DB cluster option group.
    status :: Core.Maybe Core.Text,
    -- | Specifies the name of the DB cluster option group.
    dbClusterOptionGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBClusterOptionGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbClusterOptionGroupStatus_status' - Specifies the status of the DB cluster option group.
--
-- 'dbClusterOptionGroupName', 'dbClusterOptionGroupStatus_dbClusterOptionGroupName' - Specifies the name of the DB cluster option group.
newDBClusterOptionGroupStatus ::
  DBClusterOptionGroupStatus
newDBClusterOptionGroupStatus =
  DBClusterOptionGroupStatus'
    { status = Core.Nothing,
      dbClusterOptionGroupName = Core.Nothing
    }

-- | Specifies the status of the DB cluster option group.
dbClusterOptionGroupStatus_status :: Lens.Lens' DBClusterOptionGroupStatus (Core.Maybe Core.Text)
dbClusterOptionGroupStatus_status = Lens.lens (\DBClusterOptionGroupStatus' {status} -> status) (\s@DBClusterOptionGroupStatus' {} a -> s {status = a} :: DBClusterOptionGroupStatus)

-- | Specifies the name of the DB cluster option group.
dbClusterOptionGroupStatus_dbClusterOptionGroupName :: Lens.Lens' DBClusterOptionGroupStatus (Core.Maybe Core.Text)
dbClusterOptionGroupStatus_dbClusterOptionGroupName = Lens.lens (\DBClusterOptionGroupStatus' {dbClusterOptionGroupName} -> dbClusterOptionGroupName) (\s@DBClusterOptionGroupStatus' {} a -> s {dbClusterOptionGroupName = a} :: DBClusterOptionGroupStatus)

instance Core.FromXML DBClusterOptionGroupStatus where
  parseXML x =
    DBClusterOptionGroupStatus'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "DBClusterOptionGroupName")

instance Core.Hashable DBClusterOptionGroupStatus

instance Core.NFData DBClusterOptionGroupStatus

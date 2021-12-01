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
-- Module      : Amazonka.Neptune.Types.DBInstanceStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBInstanceStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a list of status information for a DB instance.
--
-- /See:/ 'newDBInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { -- | Status of the DB instance. For a StatusType of read replica, the values
    -- can be replicating, error, stopped, or terminated.
    status :: Prelude.Maybe Prelude.Text,
    -- | Boolean value that is true if the instance is operating normally, or
    -- false if the instance is in an error state.
    normal :: Prelude.Maybe Prelude.Bool,
    -- | This value is currently \"read replication.\"
    statusType :: Prelude.Maybe Prelude.Text,
    -- | Details of the error if there is an error for the instance. If the
    -- instance is not in an error state, this value is blank.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBInstanceStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbInstanceStatusInfo_status' - Status of the DB instance. For a StatusType of read replica, the values
-- can be replicating, error, stopped, or terminated.
--
-- 'normal', 'dbInstanceStatusInfo_normal' - Boolean value that is true if the instance is operating normally, or
-- false if the instance is in an error state.
--
-- 'statusType', 'dbInstanceStatusInfo_statusType' - This value is currently \"read replication.\"
--
-- 'message', 'dbInstanceStatusInfo_message' - Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
newDBInstanceStatusInfo ::
  DBInstanceStatusInfo
newDBInstanceStatusInfo =
  DBInstanceStatusInfo'
    { status = Prelude.Nothing,
      normal = Prelude.Nothing,
      statusType = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Status of the DB instance. For a StatusType of read replica, the values
-- can be replicating, error, stopped, or terminated.
dbInstanceStatusInfo_status :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_status = Lens.lens (\DBInstanceStatusInfo' {status} -> status) (\s@DBInstanceStatusInfo' {} a -> s {status = a} :: DBInstanceStatusInfo)

-- | Boolean value that is true if the instance is operating normally, or
-- false if the instance is in an error state.
dbInstanceStatusInfo_normal :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Bool)
dbInstanceStatusInfo_normal = Lens.lens (\DBInstanceStatusInfo' {normal} -> normal) (\s@DBInstanceStatusInfo' {} a -> s {normal = a} :: DBInstanceStatusInfo)

-- | This value is currently \"read replication.\"
dbInstanceStatusInfo_statusType :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_statusType = Lens.lens (\DBInstanceStatusInfo' {statusType} -> statusType) (\s@DBInstanceStatusInfo' {} a -> s {statusType = a} :: DBInstanceStatusInfo)

-- | Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
dbInstanceStatusInfo_message :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_message = Lens.lens (\DBInstanceStatusInfo' {message} -> message) (\s@DBInstanceStatusInfo' {} a -> s {message = a} :: DBInstanceStatusInfo)

instance Core.FromXML DBInstanceStatusInfo where
  parseXML x =
    DBInstanceStatusInfo'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "Normal")
      Prelude.<*> (x Core..@? "StatusType")
      Prelude.<*> (x Core..@? "Message")

instance Prelude.Hashable DBInstanceStatusInfo where
  hashWithSalt salt' DBInstanceStatusInfo' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` statusType
      `Prelude.hashWithSalt` normal
      `Prelude.hashWithSalt` status

instance Prelude.NFData DBInstanceStatusInfo where
  rnf DBInstanceStatusInfo' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf statusType
      `Prelude.seq` Prelude.rnf normal

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
-- Module      : Amazonka.DocumentDB.Types.DBInstanceStatusInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBInstanceStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a list of status information for an instance.
--
-- /See:/ 'newDBInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { -- | Details of the error if there is an error for the instance. If the
    -- instance is not in an error state, this value is blank.
    message :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that is @true@ if the instance is operating normally, or
    -- @false@ if the instance is in an error state.
    normal :: Prelude.Maybe Prelude.Bool,
    -- | Status of the instance. For a @StatusType@ of read replica, the values
    -- can be @replicating@, error, @stopped@, or @terminated@.
    status :: Prelude.Maybe Prelude.Text,
    -- | This value is currently \"@read replication@.\"
    statusType :: Prelude.Maybe Prelude.Text
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
-- 'message', 'dbInstanceStatusInfo_message' - Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
--
-- 'normal', 'dbInstanceStatusInfo_normal' - A Boolean value that is @true@ if the instance is operating normally, or
-- @false@ if the instance is in an error state.
--
-- 'status', 'dbInstanceStatusInfo_status' - Status of the instance. For a @StatusType@ of read replica, the values
-- can be @replicating@, error, @stopped@, or @terminated@.
--
-- 'statusType', 'dbInstanceStatusInfo_statusType' - This value is currently \"@read replication@.\"
newDBInstanceStatusInfo ::
  DBInstanceStatusInfo
newDBInstanceStatusInfo =
  DBInstanceStatusInfo'
    { message = Prelude.Nothing,
      normal = Prelude.Nothing,
      status = Prelude.Nothing,
      statusType = Prelude.Nothing
    }

-- | Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
dbInstanceStatusInfo_message :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_message = Lens.lens (\DBInstanceStatusInfo' {message} -> message) (\s@DBInstanceStatusInfo' {} a -> s {message = a} :: DBInstanceStatusInfo)

-- | A Boolean value that is @true@ if the instance is operating normally, or
-- @false@ if the instance is in an error state.
dbInstanceStatusInfo_normal :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Bool)
dbInstanceStatusInfo_normal = Lens.lens (\DBInstanceStatusInfo' {normal} -> normal) (\s@DBInstanceStatusInfo' {} a -> s {normal = a} :: DBInstanceStatusInfo)

-- | Status of the instance. For a @StatusType@ of read replica, the values
-- can be @replicating@, error, @stopped@, or @terminated@.
dbInstanceStatusInfo_status :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_status = Lens.lens (\DBInstanceStatusInfo' {status} -> status) (\s@DBInstanceStatusInfo' {} a -> s {status = a} :: DBInstanceStatusInfo)

-- | This value is currently \"@read replication@.\"
dbInstanceStatusInfo_statusType :: Lens.Lens' DBInstanceStatusInfo (Prelude.Maybe Prelude.Text)
dbInstanceStatusInfo_statusType = Lens.lens (\DBInstanceStatusInfo' {statusType} -> statusType) (\s@DBInstanceStatusInfo' {} a -> s {statusType = a} :: DBInstanceStatusInfo)

instance Data.FromXML DBInstanceStatusInfo where
  parseXML x =
    DBInstanceStatusInfo'
      Prelude.<$> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "Normal")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusType")

instance Prelude.Hashable DBInstanceStatusInfo where
  hashWithSalt _salt DBInstanceStatusInfo' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` normal
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusType

instance Prelude.NFData DBInstanceStatusInfo where
  rnf DBInstanceStatusInfo' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf normal
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusType

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.DeleteDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned instance.
module Amazonka.DocumentDB.DeleteDBInstance
  ( -- * Creating a Request
    DeleteDBInstance (..),
    newDeleteDBInstance,

    -- * Request Lenses
    deleteDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    DeleteDBInstanceResponse (..),
    newDeleteDBInstanceResponse,

    -- * Response Lenses
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DeleteDBInstance.
--
-- /See:/ 'newDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { -- | The instance identifier for the instance to be deleted. This parameter
    -- isn\'t case sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the name of an existing instance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'deleteDBInstance_dbInstanceIdentifier' - The instance identifier for the instance to be deleted. This parameter
-- isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must match the name of an existing instance.
newDeleteDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  DeleteDBInstance
newDeleteDBInstance pDBInstanceIdentifier_ =
  DeleteDBInstance'
    { dbInstanceIdentifier =
        pDBInstanceIdentifier_
    }

-- | The instance identifier for the instance to be deleted. This parameter
-- isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must match the name of an existing instance.
deleteDBInstance_dbInstanceIdentifier :: Lens.Lens' DeleteDBInstance Prelude.Text
deleteDBInstance_dbInstanceIdentifier = Lens.lens (\DeleteDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DeleteDBInstance' {} a -> s {dbInstanceIdentifier = a} :: DeleteDBInstance)

instance Core.AWSRequest DeleteDBInstance where
  type
    AWSResponse DeleteDBInstance =
      DeleteDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBInstanceResult"
      ( \s h x ->
          DeleteDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBInstance where
  hashWithSalt _salt DeleteDBInstance' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData DeleteDBInstance where
  rnf DeleteDBInstance' {..} =
    Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders DeleteDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBInstance where
  toQuery DeleteDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newDeleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'deleteDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'deleteDBInstanceResponse_httpStatus' - The response's http status code.
newDeleteDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBInstanceResponse
newDeleteDBInstanceResponse pHttpStatus_ =
  DeleteDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBInstanceResponse_dbInstance :: Lens.Lens' DeleteDBInstanceResponse (Prelude.Maybe DBInstance)
deleteDBInstanceResponse_dbInstance = Lens.lens (\DeleteDBInstanceResponse' {dbInstance} -> dbInstance) (\s@DeleteDBInstanceResponse' {} a -> s {dbInstance = a} :: DeleteDBInstanceResponse)

-- | The response's http status code.
deleteDBInstanceResponse_httpStatus :: Lens.Lens' DeleteDBInstanceResponse Prelude.Int
deleteDBInstanceResponse_httpStatus = Lens.lens (\DeleteDBInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteDBInstanceResponse' {} a -> s {httpStatus = a} :: DeleteDBInstanceResponse)

instance Prelude.NFData DeleteDBInstanceResponse where
  rnf DeleteDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus

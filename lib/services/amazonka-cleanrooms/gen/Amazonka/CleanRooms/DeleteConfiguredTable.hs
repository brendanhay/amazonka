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
-- Module      : Amazonka.CleanRooms.DeleteConfiguredTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configured table.
module Amazonka.CleanRooms.DeleteConfiguredTable
  ( -- * Creating a Request
    DeleteConfiguredTable (..),
    newDeleteConfiguredTable,

    -- * Request Lenses
    deleteConfiguredTable_configuredTableIdentifier,

    -- * Destructuring the Response
    DeleteConfiguredTableResponse (..),
    newDeleteConfiguredTableResponse,

    -- * Response Lenses
    deleteConfiguredTableResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfiguredTable' smart constructor.
data DeleteConfiguredTable = DeleteConfiguredTable'
  { -- | The unique ID for the configured table to delete.
    configuredTableIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'deleteConfiguredTable_configuredTableIdentifier' - The unique ID for the configured table to delete.
newDeleteConfiguredTable ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  DeleteConfiguredTable
newDeleteConfiguredTable pConfiguredTableIdentifier_ =
  DeleteConfiguredTable'
    { configuredTableIdentifier =
        pConfiguredTableIdentifier_
    }

-- | The unique ID for the configured table to delete.
deleteConfiguredTable_configuredTableIdentifier :: Lens.Lens' DeleteConfiguredTable Prelude.Text
deleteConfiguredTable_configuredTableIdentifier = Lens.lens (\DeleteConfiguredTable' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@DeleteConfiguredTable' {} a -> s {configuredTableIdentifier = a} :: DeleteConfiguredTable)

instance Core.AWSRequest DeleteConfiguredTable where
  type
    AWSResponse DeleteConfiguredTable =
      DeleteConfiguredTableResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConfiguredTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConfiguredTable where
  hashWithSalt _salt DeleteConfiguredTable' {..} =
    _salt
      `Prelude.hashWithSalt` configuredTableIdentifier

instance Prelude.NFData DeleteConfiguredTable where
  rnf DeleteConfiguredTable' {..} =
    Prelude.rnf configuredTableIdentifier

instance Data.ToHeaders DeleteConfiguredTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConfiguredTable where
  toPath DeleteConfiguredTable' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier
      ]

instance Data.ToQuery DeleteConfiguredTable where
  toQuery = Prelude.const Prelude.mempty

-- | The empty output for a successful deletion.
--
-- /See:/ 'newDeleteConfiguredTableResponse' smart constructor.
data DeleteConfiguredTableResponse = DeleteConfiguredTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfiguredTableResponse_httpStatus' - The response's http status code.
newDeleteConfiguredTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfiguredTableResponse
newDeleteConfiguredTableResponse pHttpStatus_ =
  DeleteConfiguredTableResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConfiguredTableResponse_httpStatus :: Lens.Lens' DeleteConfiguredTableResponse Prelude.Int
deleteConfiguredTableResponse_httpStatus = Lens.lens (\DeleteConfiguredTableResponse' {httpStatus} -> httpStatus) (\s@DeleteConfiguredTableResponse' {} a -> s {httpStatus = a} :: DeleteConfiguredTableResponse)

instance Prelude.NFData DeleteConfiguredTableResponse where
  rnf DeleteConfiguredTableResponse' {..} =
    Prelude.rnf httpStatus

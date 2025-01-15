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
-- Module      : Amazonka.DMS.DeleteFleetAdvisorDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Fleet Advisor collector databases.
module Amazonka.DMS.DeleteFleetAdvisorDatabases
  ( -- * Creating a Request
    DeleteFleetAdvisorDatabases (..),
    newDeleteFleetAdvisorDatabases,

    -- * Request Lenses
    deleteFleetAdvisorDatabases_databaseIds,

    -- * Destructuring the Response
    DeleteFleetAdvisorDatabasesResponse (..),
    newDeleteFleetAdvisorDatabasesResponse,

    -- * Response Lenses
    deleteFleetAdvisorDatabasesResponse_databaseIds,
    deleteFleetAdvisorDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFleetAdvisorDatabases' smart constructor.
data DeleteFleetAdvisorDatabases = DeleteFleetAdvisorDatabases'
  { -- | The IDs of the Fleet Advisor collector databases to delete.
    databaseIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetAdvisorDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseIds', 'deleteFleetAdvisorDatabases_databaseIds' - The IDs of the Fleet Advisor collector databases to delete.
newDeleteFleetAdvisorDatabases ::
  DeleteFleetAdvisorDatabases
newDeleteFleetAdvisorDatabases =
  DeleteFleetAdvisorDatabases'
    { databaseIds =
        Prelude.mempty
    }

-- | The IDs of the Fleet Advisor collector databases to delete.
deleteFleetAdvisorDatabases_databaseIds :: Lens.Lens' DeleteFleetAdvisorDatabases [Prelude.Text]
deleteFleetAdvisorDatabases_databaseIds = Lens.lens (\DeleteFleetAdvisorDatabases' {databaseIds} -> databaseIds) (\s@DeleteFleetAdvisorDatabases' {} a -> s {databaseIds = a} :: DeleteFleetAdvisorDatabases) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteFleetAdvisorDatabases where
  type
    AWSResponse DeleteFleetAdvisorDatabases =
      DeleteFleetAdvisorDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFleetAdvisorDatabasesResponse'
            Prelude.<$> (x Data..?> "DatabaseIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFleetAdvisorDatabases where
  hashWithSalt _salt DeleteFleetAdvisorDatabases' {..} =
    _salt `Prelude.hashWithSalt` databaseIds

instance Prelude.NFData DeleteFleetAdvisorDatabases where
  rnf DeleteFleetAdvisorDatabases' {..} =
    Prelude.rnf databaseIds

instance Data.ToHeaders DeleteFleetAdvisorDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DeleteFleetAdvisorDatabases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFleetAdvisorDatabases where
  toJSON DeleteFleetAdvisorDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatabaseIds" Data..= databaseIds)]
      )

instance Data.ToPath DeleteFleetAdvisorDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFleetAdvisorDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetAdvisorDatabasesResponse' smart constructor.
data DeleteFleetAdvisorDatabasesResponse = DeleteFleetAdvisorDatabasesResponse'
  { -- | The IDs of the databases that the operation deleted.
    databaseIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetAdvisorDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseIds', 'deleteFleetAdvisorDatabasesResponse_databaseIds' - The IDs of the databases that the operation deleted.
--
-- 'httpStatus', 'deleteFleetAdvisorDatabasesResponse_httpStatus' - The response's http status code.
newDeleteFleetAdvisorDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFleetAdvisorDatabasesResponse
newDeleteFleetAdvisorDatabasesResponse pHttpStatus_ =
  DeleteFleetAdvisorDatabasesResponse'
    { databaseIds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the databases that the operation deleted.
deleteFleetAdvisorDatabasesResponse_databaseIds :: Lens.Lens' DeleteFleetAdvisorDatabasesResponse (Prelude.Maybe [Prelude.Text])
deleteFleetAdvisorDatabasesResponse_databaseIds = Lens.lens (\DeleteFleetAdvisorDatabasesResponse' {databaseIds} -> databaseIds) (\s@DeleteFleetAdvisorDatabasesResponse' {} a -> s {databaseIds = a} :: DeleteFleetAdvisorDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteFleetAdvisorDatabasesResponse_httpStatus :: Lens.Lens' DeleteFleetAdvisorDatabasesResponse Prelude.Int
deleteFleetAdvisorDatabasesResponse_httpStatus = Lens.lens (\DeleteFleetAdvisorDatabasesResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetAdvisorDatabasesResponse' {} a -> s {httpStatus = a} :: DeleteFleetAdvisorDatabasesResponse)

instance
  Prelude.NFData
    DeleteFleetAdvisorDatabasesResponse
  where
  rnf DeleteFleetAdvisorDatabasesResponse' {..} =
    Prelude.rnf databaseIds `Prelude.seq`
      Prelude.rnf httpStatus

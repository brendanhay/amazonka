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
-- Module      : Amazonka.TimeStreamWrite.DeleteDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given Timestream database. /This is an irreversible operation.
-- After a database is deleted, the time series data from its tables cannot
-- be recovered./
--
-- All tables in the database must be deleted first, or a
-- ValidationException error will be thrown.
--
-- Due to the nature of distributed retries, the operation can return
-- either success or a ResourceNotFoundException. Clients should consider
-- them equivalent.
--
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.delete-db.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.DeleteDatabase
  ( -- * Creating a Request
    DeleteDatabase (..),
    newDeleteDatabase,

    -- * Request Lenses
    deleteDatabase_databaseName,

    -- * Destructuring the Response
    DeleteDatabaseResponse (..),
    newDeleteDatabaseResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newDeleteDatabase' smart constructor.
data DeleteDatabase = DeleteDatabase'
  { -- | The name of the Timestream database to be deleted.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'deleteDatabase_databaseName' - The name of the Timestream database to be deleted.
newDeleteDatabase ::
  -- | 'databaseName'
  Prelude.Text ->
  DeleteDatabase
newDeleteDatabase pDatabaseName_ =
  DeleteDatabase' {databaseName = pDatabaseName_}

-- | The name of the Timestream database to be deleted.
deleteDatabase_databaseName :: Lens.Lens' DeleteDatabase Prelude.Text
deleteDatabase_databaseName = Lens.lens (\DeleteDatabase' {databaseName} -> databaseName) (\s@DeleteDatabase' {} a -> s {databaseName = a} :: DeleteDatabase)

instance Core.AWSRequest DeleteDatabase where
  type
    AWSResponse DeleteDatabase =
      DeleteDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDatabaseResponse'

instance Prelude.Hashable DeleteDatabase where
  hashWithSalt _salt DeleteDatabase' {..} =
    _salt `Prelude.hashWithSalt` databaseName

instance Prelude.NFData DeleteDatabase where
  rnf DeleteDatabase' {..} = Prelude.rnf databaseName

instance Data.ToHeaders DeleteDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DeleteDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDatabase where
  toJSON DeleteDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DatabaseName" Data..= databaseName)]
      )

instance Data.ToPath DeleteDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatabaseResponse' smart constructor.
data DeleteDatabaseResponse = DeleteDatabaseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDatabaseResponse ::
  DeleteDatabaseResponse
newDeleteDatabaseResponse = DeleteDatabaseResponse'

instance Prelude.NFData DeleteDatabaseResponse where
  rnf _ = ()

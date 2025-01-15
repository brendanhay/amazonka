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
-- Module      : Amazonka.Lightsail.StopRelationalDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific database that is currently running in Amazon Lightsail.
--
-- The @stop relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.StopRelationalDatabase
  ( -- * Creating a Request
    StopRelationalDatabase (..),
    newStopRelationalDatabase,

    -- * Request Lenses
    stopRelationalDatabase_relationalDatabaseSnapshotName,
    stopRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    StopRelationalDatabaseResponse (..),
    newStopRelationalDatabaseResponse,

    -- * Response Lenses
    stopRelationalDatabaseResponse_operations,
    stopRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopRelationalDatabase' smart constructor.
data StopRelationalDatabase = StopRelationalDatabase'
  { -- | The name of your new database snapshot to be created before stopping
    -- your database.
    relationalDatabaseSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | The name of your database to stop.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseSnapshotName', 'stopRelationalDatabase_relationalDatabaseSnapshotName' - The name of your new database snapshot to be created before stopping
-- your database.
--
-- 'relationalDatabaseName', 'stopRelationalDatabase_relationalDatabaseName' - The name of your database to stop.
newStopRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  StopRelationalDatabase
newStopRelationalDatabase pRelationalDatabaseName_ =
  StopRelationalDatabase'
    { relationalDatabaseSnapshotName =
        Prelude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of your new database snapshot to be created before stopping
-- your database.
stopRelationalDatabase_relationalDatabaseSnapshotName :: Lens.Lens' StopRelationalDatabase (Prelude.Maybe Prelude.Text)
stopRelationalDatabase_relationalDatabaseSnapshotName = Lens.lens (\StopRelationalDatabase' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@StopRelationalDatabase' {} a -> s {relationalDatabaseSnapshotName = a} :: StopRelationalDatabase)

-- | The name of your database to stop.
stopRelationalDatabase_relationalDatabaseName :: Lens.Lens' StopRelationalDatabase Prelude.Text
stopRelationalDatabase_relationalDatabaseName = Lens.lens (\StopRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@StopRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: StopRelationalDatabase)

instance Core.AWSRequest StopRelationalDatabase where
  type
    AWSResponse StopRelationalDatabase =
      StopRelationalDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRelationalDatabaseResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopRelationalDatabase where
  hashWithSalt _salt StopRelationalDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` relationalDatabaseSnapshotName
      `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData StopRelationalDatabase where
  rnf StopRelationalDatabase' {..} =
    Prelude.rnf relationalDatabaseSnapshotName `Prelude.seq`
      Prelude.rnf relationalDatabaseName

instance Data.ToHeaders StopRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.StopRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopRelationalDatabase where
  toJSON StopRelationalDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("relationalDatabaseSnapshotName" Data..=)
              Prelude.<$> relationalDatabaseSnapshotName,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath StopRelationalDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery StopRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopRelationalDatabaseResponse' smart constructor.
data StopRelationalDatabaseResponse = StopRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'stopRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'stopRelationalDatabaseResponse_httpStatus' - The response's http status code.
newStopRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopRelationalDatabaseResponse
newStopRelationalDatabaseResponse pHttpStatus_ =
  StopRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
stopRelationalDatabaseResponse_operations :: Lens.Lens' StopRelationalDatabaseResponse (Prelude.Maybe [Operation])
stopRelationalDatabaseResponse_operations = Lens.lens (\StopRelationalDatabaseResponse' {operations} -> operations) (\s@StopRelationalDatabaseResponse' {} a -> s {operations = a} :: StopRelationalDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopRelationalDatabaseResponse_httpStatus :: Lens.Lens' StopRelationalDatabaseResponse Prelude.Int
stopRelationalDatabaseResponse_httpStatus = Lens.lens (\StopRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@StopRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: StopRelationalDatabaseResponse)

instance
  Prelude.NFData
    StopRelationalDatabaseResponse
  where
  rnf StopRelationalDatabaseResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus

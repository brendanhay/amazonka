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
-- Module      : Network.AWS.Lightsail.StopRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific database that is currently running in Amazon Lightsail.
--
-- The @stop relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.StopRelationalDatabase
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopRelationalDatabase' smart constructor.
data StopRelationalDatabase = StopRelationalDatabase'
  { -- | The name of your new database snapshot to be created before stopping
    -- your database.
    relationalDatabaseSnapshotName :: Core.Maybe Core.Text,
    -- | The name of your database to stop.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopRelationalDatabase
newStopRelationalDatabase pRelationalDatabaseName_ =
  StopRelationalDatabase'
    { relationalDatabaseSnapshotName =
        Core.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of your new database snapshot to be created before stopping
-- your database.
stopRelationalDatabase_relationalDatabaseSnapshotName :: Lens.Lens' StopRelationalDatabase (Core.Maybe Core.Text)
stopRelationalDatabase_relationalDatabaseSnapshotName = Lens.lens (\StopRelationalDatabase' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@StopRelationalDatabase' {} a -> s {relationalDatabaseSnapshotName = a} :: StopRelationalDatabase)

-- | The name of your database to stop.
stopRelationalDatabase_relationalDatabaseName :: Lens.Lens' StopRelationalDatabase Core.Text
stopRelationalDatabase_relationalDatabaseName = Lens.lens (\StopRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@StopRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: StopRelationalDatabase)

instance Core.AWSRequest StopRelationalDatabase where
  type
    AWSResponse StopRelationalDatabase =
      StopRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRelationalDatabaseResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopRelationalDatabase

instance Core.NFData StopRelationalDatabase

instance Core.ToHeaders StopRelationalDatabase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.StopRelationalDatabase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopRelationalDatabase where
  toJSON StopRelationalDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ ("relationalDatabaseSnapshotName" Core..=)
              Core.<$> relationalDatabaseSnapshotName,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath StopRelationalDatabase where
  toPath = Core.const "/"

instance Core.ToQuery StopRelationalDatabase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopRelationalDatabaseResponse' smart constructor.
data StopRelationalDatabaseResponse = StopRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopRelationalDatabaseResponse
newStopRelationalDatabaseResponse pHttpStatus_ =
  StopRelationalDatabaseResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
stopRelationalDatabaseResponse_operations :: Lens.Lens' StopRelationalDatabaseResponse (Core.Maybe [Operation])
stopRelationalDatabaseResponse_operations = Lens.lens (\StopRelationalDatabaseResponse' {operations} -> operations) (\s@StopRelationalDatabaseResponse' {} a -> s {operations = a} :: StopRelationalDatabaseResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopRelationalDatabaseResponse_httpStatus :: Lens.Lens' StopRelationalDatabaseResponse Core.Int
stopRelationalDatabaseResponse_httpStatus = Lens.lens (\StopRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@StopRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: StopRelationalDatabaseResponse)

instance Core.NFData StopRelationalDatabaseResponse

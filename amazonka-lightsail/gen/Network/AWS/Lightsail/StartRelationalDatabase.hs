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
-- Module      : Network.AWS.Lightsail.StartRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific database from a stopped state in Amazon Lightsail. To
-- restart a database, use the @reboot relational database@ operation.
--
-- The @start relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.StartRelationalDatabase
  ( -- * Creating a Request
    StartRelationalDatabase (..),
    newStartRelationalDatabase,

    -- * Request Lenses
    startRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    StartRelationalDatabaseResponse (..),
    newStartRelationalDatabaseResponse,

    -- * Response Lenses
    startRelationalDatabaseResponse_operations,
    startRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartRelationalDatabase' smart constructor.
data StartRelationalDatabase = StartRelationalDatabase'
  { -- | The name of your database to start.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseName', 'startRelationalDatabase_relationalDatabaseName' - The name of your database to start.
newStartRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  StartRelationalDatabase
newStartRelationalDatabase pRelationalDatabaseName_ =
  StartRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database to start.
startRelationalDatabase_relationalDatabaseName :: Lens.Lens' StartRelationalDatabase Core.Text
startRelationalDatabase_relationalDatabaseName = Lens.lens (\StartRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@StartRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: StartRelationalDatabase)

instance Core.AWSRequest StartRelationalDatabase where
  type
    AWSResponse StartRelationalDatabase =
      StartRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRelationalDatabaseResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartRelationalDatabase

instance Core.NFData StartRelationalDatabase

instance Core.ToHeaders StartRelationalDatabase where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.StartRelationalDatabase" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartRelationalDatabase where
  toJSON StartRelationalDatabase' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath StartRelationalDatabase where
  toPath = Core.const "/"

instance Core.ToQuery StartRelationalDatabase where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartRelationalDatabaseResponse' smart constructor.
data StartRelationalDatabaseResponse = StartRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'startRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'startRelationalDatabaseResponse_httpStatus' - The response's http status code.
newStartRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartRelationalDatabaseResponse
newStartRelationalDatabaseResponse pHttpStatus_ =
  StartRelationalDatabaseResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
startRelationalDatabaseResponse_operations :: Lens.Lens' StartRelationalDatabaseResponse (Core.Maybe [Operation])
startRelationalDatabaseResponse_operations = Lens.lens (\StartRelationalDatabaseResponse' {operations} -> operations) (\s@StartRelationalDatabaseResponse' {} a -> s {operations = a} :: StartRelationalDatabaseResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
startRelationalDatabaseResponse_httpStatus :: Lens.Lens' StartRelationalDatabaseResponse Core.Int
startRelationalDatabaseResponse_httpStatus = Lens.lens (\StartRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@StartRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: StartRelationalDatabaseResponse)

instance Core.NFData StartRelationalDatabaseResponse

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
-- Module      : Amazonka.Lightsail.StartRelationalDatabase
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.StartRelationalDatabase
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRelationalDatabase' smart constructor.
data StartRelationalDatabase = StartRelationalDatabase'
  { -- | The name of your database to start.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartRelationalDatabase
newStartRelationalDatabase pRelationalDatabaseName_ =
  StartRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database to start.
startRelationalDatabase_relationalDatabaseName :: Lens.Lens' StartRelationalDatabase Prelude.Text
startRelationalDatabase_relationalDatabaseName = Lens.lens (\StartRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@StartRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: StartRelationalDatabase)

instance Core.AWSRequest StartRelationalDatabase where
  type
    AWSResponse StartRelationalDatabase =
      StartRelationalDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRelationalDatabaseResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRelationalDatabase where
  hashWithSalt _salt StartRelationalDatabase' {..} =
    _salt `Prelude.hashWithSalt` relationalDatabaseName

instance Prelude.NFData StartRelationalDatabase where
  rnf StartRelationalDatabase' {..} =
    Prelude.rnf relationalDatabaseName

instance Core.ToHeaders StartRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.StartRelationalDatabase" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartRelationalDatabase where
  toJSON StartRelationalDatabase' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath StartRelationalDatabase where
  toPath = Prelude.const "/"

instance Core.ToQuery StartRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRelationalDatabaseResponse' smart constructor.
data StartRelationalDatabaseResponse = StartRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartRelationalDatabaseResponse
newStartRelationalDatabaseResponse pHttpStatus_ =
  StartRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
startRelationalDatabaseResponse_operations :: Lens.Lens' StartRelationalDatabaseResponse (Prelude.Maybe [Operation])
startRelationalDatabaseResponse_operations = Lens.lens (\StartRelationalDatabaseResponse' {operations} -> operations) (\s@StartRelationalDatabaseResponse' {} a -> s {operations = a} :: StartRelationalDatabaseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startRelationalDatabaseResponse_httpStatus :: Lens.Lens' StartRelationalDatabaseResponse Prelude.Int
startRelationalDatabaseResponse_httpStatus = Lens.lens (\StartRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@StartRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: StartRelationalDatabaseResponse)

instance
  Prelude.NFData
    StartRelationalDatabaseResponse
  where
  rnf StartRelationalDatabaseResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus

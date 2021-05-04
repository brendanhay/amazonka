{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.RebootRelationalDatabase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific database in Amazon Lightsail.
--
-- The @reboot relational database@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.RebootRelationalDatabase
  ( -- * Creating a Request
    RebootRelationalDatabase (..),
    newRebootRelationalDatabase,

    -- * Request Lenses
    rebootRelationalDatabase_relationalDatabaseName,

    -- * Destructuring the Response
    RebootRelationalDatabaseResponse (..),
    newRebootRelationalDatabaseResponse,

    -- * Response Lenses
    rebootRelationalDatabaseResponse_operations,
    rebootRelationalDatabaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootRelationalDatabase' smart constructor.
data RebootRelationalDatabase = RebootRelationalDatabase'
  { -- | The name of your database to reboot.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootRelationalDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseName', 'rebootRelationalDatabase_relationalDatabaseName' - The name of your database to reboot.
newRebootRelationalDatabase ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  RebootRelationalDatabase
newRebootRelationalDatabase pRelationalDatabaseName_ =
  RebootRelationalDatabase'
    { relationalDatabaseName =
        pRelationalDatabaseName_
    }

-- | The name of your database to reboot.
rebootRelationalDatabase_relationalDatabaseName :: Lens.Lens' RebootRelationalDatabase Prelude.Text
rebootRelationalDatabase_relationalDatabaseName = Lens.lens (\RebootRelationalDatabase' {relationalDatabaseName} -> relationalDatabaseName) (\s@RebootRelationalDatabase' {} a -> s {relationalDatabaseName = a} :: RebootRelationalDatabase)

instance Prelude.AWSRequest RebootRelationalDatabase where
  type
    Rs RebootRelationalDatabase =
      RebootRelationalDatabaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootRelationalDatabaseResponse'
            Prelude.<$> ( x Prelude..?> "operations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootRelationalDatabase

instance Prelude.NFData RebootRelationalDatabase

instance Prelude.ToHeaders RebootRelationalDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.RebootRelationalDatabase" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RebootRelationalDatabase where
  toJSON RebootRelationalDatabase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseName"
                  Prelude..= relationalDatabaseName
              )
          ]
      )

instance Prelude.ToPath RebootRelationalDatabase where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RebootRelationalDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootRelationalDatabaseResponse' smart constructor.
data RebootRelationalDatabaseResponse = RebootRelationalDatabaseResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RebootRelationalDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'rebootRelationalDatabaseResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'rebootRelationalDatabaseResponse_httpStatus' - The response's http status code.
newRebootRelationalDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootRelationalDatabaseResponse
newRebootRelationalDatabaseResponse pHttpStatus_ =
  RebootRelationalDatabaseResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
rebootRelationalDatabaseResponse_operations :: Lens.Lens' RebootRelationalDatabaseResponse (Prelude.Maybe [Operation])
rebootRelationalDatabaseResponse_operations = Lens.lens (\RebootRelationalDatabaseResponse' {operations} -> operations) (\s@RebootRelationalDatabaseResponse' {} a -> s {operations = a} :: RebootRelationalDatabaseResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
rebootRelationalDatabaseResponse_httpStatus :: Lens.Lens' RebootRelationalDatabaseResponse Prelude.Int
rebootRelationalDatabaseResponse_httpStatus = Lens.lens (\RebootRelationalDatabaseResponse' {httpStatus} -> httpStatus) (\s@RebootRelationalDatabaseResponse' {} a -> s {httpStatus = a} :: RebootRelationalDatabaseResponse)

instance
  Prelude.NFData
    RebootRelationalDatabaseResponse

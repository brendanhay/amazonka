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
-- Module      : Amazonka.SecurityLake.DeleteDataLakeExceptionSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification subscription in Amazon Security Lake
-- for the organization you specify.
module Amazonka.SecurityLake.DeleteDataLakeExceptionSubscription
  ( -- * Creating a Request
    DeleteDataLakeExceptionSubscription (..),
    newDeleteDataLakeExceptionSubscription,

    -- * Destructuring the Response
    DeleteDataLakeExceptionSubscriptionResponse (..),
    newDeleteDataLakeExceptionSubscriptionResponse,

    -- * Response Lenses
    deleteDataLakeExceptionSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteDataLakeExceptionSubscription' smart constructor.
data DeleteDataLakeExceptionSubscription = DeleteDataLakeExceptionSubscription'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLakeExceptionSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDataLakeExceptionSubscription ::
  DeleteDataLakeExceptionSubscription
newDeleteDataLakeExceptionSubscription =
  DeleteDataLakeExceptionSubscription'

instance
  Core.AWSRequest
    DeleteDataLakeExceptionSubscription
  where
  type
    AWSResponse DeleteDataLakeExceptionSubscription =
      DeleteDataLakeExceptionSubscriptionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataLakeExceptionSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDataLakeExceptionSubscription
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeleteDataLakeExceptionSubscription
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeleteDataLakeExceptionSubscription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteDataLakeExceptionSubscription
  where
  toPath =
    Prelude.const
      "/v1/datalake/exceptions/subscription"

instance
  Data.ToQuery
    DeleteDataLakeExceptionSubscription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataLakeExceptionSubscriptionResponse' smart constructor.
data DeleteDataLakeExceptionSubscriptionResponse = DeleteDataLakeExceptionSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataLakeExceptionSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataLakeExceptionSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteDataLakeExceptionSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataLakeExceptionSubscriptionResponse
newDeleteDataLakeExceptionSubscriptionResponse
  pHttpStatus_ =
    DeleteDataLakeExceptionSubscriptionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteDataLakeExceptionSubscriptionResponse_httpStatus :: Lens.Lens' DeleteDataLakeExceptionSubscriptionResponse Prelude.Int
deleteDataLakeExceptionSubscriptionResponse_httpStatus = Lens.lens (\DeleteDataLakeExceptionSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteDataLakeExceptionSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteDataLakeExceptionSubscriptionResponse)

instance
  Prelude.NFData
    DeleteDataLakeExceptionSubscriptionResponse
  where
  rnf DeleteDataLakeExceptionSubscriptionResponse' {..} =
    Prelude.rnf httpStatus

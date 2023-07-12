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
-- Module      : Amazonka.IoT.DeleteAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an authorizer.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteAuthorizer>
-- action.
module Amazonka.IoT.DeleteAuthorizer
  ( -- * Creating a Request
    DeleteAuthorizer (..),
    newDeleteAuthorizer,

    -- * Request Lenses
    deleteAuthorizer_authorizerName,

    -- * Destructuring the Response
    DeleteAuthorizerResponse (..),
    newDeleteAuthorizerResponse,

    -- * Response Lenses
    deleteAuthorizerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { -- | The name of the authorizer to delete.
    authorizerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerName', 'deleteAuthorizer_authorizerName' - The name of the authorizer to delete.
newDeleteAuthorizer ::
  -- | 'authorizerName'
  Prelude.Text ->
  DeleteAuthorizer
newDeleteAuthorizer pAuthorizerName_ =
  DeleteAuthorizer'
    { authorizerName =
        pAuthorizerName_
    }

-- | The name of the authorizer to delete.
deleteAuthorizer_authorizerName :: Lens.Lens' DeleteAuthorizer Prelude.Text
deleteAuthorizer_authorizerName = Lens.lens (\DeleteAuthorizer' {authorizerName} -> authorizerName) (\s@DeleteAuthorizer' {} a -> s {authorizerName = a} :: DeleteAuthorizer)

instance Core.AWSRequest DeleteAuthorizer where
  type
    AWSResponse DeleteAuthorizer =
      DeleteAuthorizerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAuthorizerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAuthorizer where
  hashWithSalt _salt DeleteAuthorizer' {..} =
    _salt `Prelude.hashWithSalt` authorizerName

instance Prelude.NFData DeleteAuthorizer where
  rnf DeleteAuthorizer' {..} =
    Prelude.rnf authorizerName

instance Data.ToHeaders DeleteAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Prelude.mconcat
      ["/authorizer/", Data.toBS authorizerName]

instance Data.ToQuery DeleteAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAuthorizerResponse_httpStatus' - The response's http status code.
newDeleteAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAuthorizerResponse
newDeleteAuthorizerResponse pHttpStatus_ =
  DeleteAuthorizerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAuthorizerResponse_httpStatus :: Lens.Lens' DeleteAuthorizerResponse Prelude.Int
deleteAuthorizerResponse_httpStatus = Lens.lens (\DeleteAuthorizerResponse' {httpStatus} -> httpStatus) (\s@DeleteAuthorizerResponse' {} a -> s {httpStatus = a} :: DeleteAuthorizerResponse)

instance Prelude.NFData DeleteAuthorizerResponse where
  rnf DeleteAuthorizerResponse' {..} =
    Prelude.rnf httpStatus

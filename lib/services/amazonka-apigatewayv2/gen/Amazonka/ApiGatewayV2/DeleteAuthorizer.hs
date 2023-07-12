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
-- Module      : Amazonka.ApiGatewayV2.DeleteAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Authorizer.
module Amazonka.ApiGatewayV2.DeleteAuthorizer
  ( -- * Creating a Request
    DeleteAuthorizer (..),
    newDeleteAuthorizer,

    -- * Request Lenses
    deleteAuthorizer_authorizerId,
    deleteAuthorizer_apiId,

    -- * Destructuring the Response
    DeleteAuthorizerResponse (..),
    newDeleteAuthorizerResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { -- | The authorizer identifier.
    authorizerId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
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
-- 'authorizerId', 'deleteAuthorizer_authorizerId' - The authorizer identifier.
--
-- 'apiId', 'deleteAuthorizer_apiId' - The API identifier.
newDeleteAuthorizer ::
  -- | 'authorizerId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  DeleteAuthorizer
newDeleteAuthorizer pAuthorizerId_ pApiId_ =
  DeleteAuthorizer'
    { authorizerId = pAuthorizerId_,
      apiId = pApiId_
    }

-- | The authorizer identifier.
deleteAuthorizer_authorizerId :: Lens.Lens' DeleteAuthorizer Prelude.Text
deleteAuthorizer_authorizerId = Lens.lens (\DeleteAuthorizer' {authorizerId} -> authorizerId) (\s@DeleteAuthorizer' {} a -> s {authorizerId = a} :: DeleteAuthorizer)

-- | The API identifier.
deleteAuthorizer_apiId :: Lens.Lens' DeleteAuthorizer Prelude.Text
deleteAuthorizer_apiId = Lens.lens (\DeleteAuthorizer' {apiId} -> apiId) (\s@DeleteAuthorizer' {} a -> s {apiId = a} :: DeleteAuthorizer)

instance Core.AWSRequest DeleteAuthorizer where
  type
    AWSResponse DeleteAuthorizer =
      DeleteAuthorizerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteAuthorizerResponse'

instance Prelude.Hashable DeleteAuthorizer where
  hashWithSalt _salt DeleteAuthorizer' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteAuthorizer where
  rnf DeleteAuthorizer' {..} =
    Prelude.rnf authorizerId
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders DeleteAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/authorizers/",
        Data.toBS authorizerId
      ]

instance Data.ToQuery DeleteAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAuthorizerResponse ::
  DeleteAuthorizerResponse
newDeleteAuthorizerResponse =
  DeleteAuthorizerResponse'

instance Prelude.NFData DeleteAuthorizerResponse where
  rnf _ = ()

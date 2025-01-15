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
-- Module      : Amazonka.APIGateway.DeleteAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Authorizer resource.
module Amazonka.APIGateway.DeleteAuthorizer
  ( -- * Creating a Request
    DeleteAuthorizer (..),
    newDeleteAuthorizer,

    -- * Request Lenses
    deleteAuthorizer_restApiId,
    deleteAuthorizer_authorizerId,

    -- * Destructuring the Response
    DeleteAuthorizerResponse (..),
    newDeleteAuthorizerResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete an existing Authorizer resource.
--
-- /See:/ 'newDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the Authorizer resource.
    authorizerId :: Prelude.Text
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
-- 'restApiId', 'deleteAuthorizer_restApiId' - The string identifier of the associated RestApi.
--
-- 'authorizerId', 'deleteAuthorizer_authorizerId' - The identifier of the Authorizer resource.
newDeleteAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  DeleteAuthorizer
newDeleteAuthorizer pRestApiId_ pAuthorizerId_ =
  DeleteAuthorizer'
    { restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | The string identifier of the associated RestApi.
deleteAuthorizer_restApiId :: Lens.Lens' DeleteAuthorizer Prelude.Text
deleteAuthorizer_restApiId = Lens.lens (\DeleteAuthorizer' {restApiId} -> restApiId) (\s@DeleteAuthorizer' {} a -> s {restApiId = a} :: DeleteAuthorizer)

-- | The identifier of the Authorizer resource.
deleteAuthorizer_authorizerId :: Lens.Lens' DeleteAuthorizer Prelude.Text
deleteAuthorizer_authorizerId = Lens.lens (\DeleteAuthorizer' {authorizerId} -> authorizerId) (\s@DeleteAuthorizer' {} a -> s {authorizerId = a} :: DeleteAuthorizer)

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
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` authorizerId

instance Prelude.NFData DeleteAuthorizer where
  rnf DeleteAuthorizer' {..} =
    Prelude.rnf restApiId `Prelude.seq`
      Prelude.rnf authorizerId

instance Data.ToHeaders DeleteAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
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

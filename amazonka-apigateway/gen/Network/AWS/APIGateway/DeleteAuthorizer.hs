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
-- Module      : Network.AWS.APIGateway.DeleteAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Authorizer resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html AWS CLI>
module Network.AWS.APIGateway.DeleteAuthorizer
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing Authorizer resource.
--
-- /See:/ 'newDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The identifier of the Authorizer resource.
    authorizerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'authorizerId', 'deleteAuthorizer_authorizerId' - [Required] The identifier of the Authorizer resource.
newDeleteAuthorizer ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'authorizerId'
  Core.Text ->
  DeleteAuthorizer
newDeleteAuthorizer pRestApiId_ pAuthorizerId_ =
  DeleteAuthorizer'
    { restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | [Required] The string identifier of the associated RestApi.
deleteAuthorizer_restApiId :: Lens.Lens' DeleteAuthorizer Core.Text
deleteAuthorizer_restApiId = Lens.lens (\DeleteAuthorizer' {restApiId} -> restApiId) (\s@DeleteAuthorizer' {} a -> s {restApiId = a} :: DeleteAuthorizer)

-- | [Required] The identifier of the Authorizer resource.
deleteAuthorizer_authorizerId :: Lens.Lens' DeleteAuthorizer Core.Text
deleteAuthorizer_authorizerId = Lens.lens (\DeleteAuthorizer' {authorizerId} -> authorizerId) (\s@DeleteAuthorizer' {} a -> s {authorizerId = a} :: DeleteAuthorizer)

instance Core.AWSRequest DeleteAuthorizer where
  type
    AWSResponse DeleteAuthorizer =
      DeleteAuthorizerResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteAuthorizerResponse'

instance Core.Hashable DeleteAuthorizer

instance Core.NFData DeleteAuthorizer

instance Core.ToHeaders DeleteAuthorizer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteAuthorizer where
  toPath DeleteAuthorizer' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/authorizers/",
        Core.toBS authorizerId
      ]

instance Core.ToQuery DeleteAuthorizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAuthorizerResponse ::
  DeleteAuthorizerResponse
newDeleteAuthorizerResponse =
  DeleteAuthorizerResponse'

instance Core.NFData DeleteAuthorizerResponse

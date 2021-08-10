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
-- Module      : Network.AWS.APIGateway.UpdateAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Authorizer resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/update-authorizer.html AWS CLI>
module Network.AWS.APIGateway.UpdateAuthorizer
  ( -- * Creating a Request
    UpdateAuthorizer (..),
    newUpdateAuthorizer,

    -- * Request Lenses
    updateAuthorizer_patchOperations,
    updateAuthorizer_restApiId,
    updateAuthorizer_authorizerId,

    -- * Destructuring the Response
    Authorizer (..),
    newAuthorizer,

    -- * Response Lenses
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing Authorizer resource.
--
-- /See:/ 'newUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the Authorizer resource.
    authorizerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateAuthorizer_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'authorizerId', 'updateAuthorizer_authorizerId' - [Required] The identifier of the Authorizer resource.
newUpdateAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'authorizerId'
  Prelude.Text ->
  UpdateAuthorizer
newUpdateAuthorizer pRestApiId_ pAuthorizerId_ =
  UpdateAuthorizer'
    { patchOperations =
        Prelude.Nothing,
      restApiId = pRestApiId_,
      authorizerId = pAuthorizerId_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateAuthorizer_patchOperations :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe [PatchOperation])
updateAuthorizer_patchOperations = Lens.lens (\UpdateAuthorizer' {patchOperations} -> patchOperations) (\s@UpdateAuthorizer' {} a -> s {patchOperations = a} :: UpdateAuthorizer) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateAuthorizer_restApiId :: Lens.Lens' UpdateAuthorizer Prelude.Text
updateAuthorizer_restApiId = Lens.lens (\UpdateAuthorizer' {restApiId} -> restApiId) (\s@UpdateAuthorizer' {} a -> s {restApiId = a} :: UpdateAuthorizer)

-- | [Required] The identifier of the Authorizer resource.
updateAuthorizer_authorizerId :: Lens.Lens' UpdateAuthorizer Prelude.Text
updateAuthorizer_authorizerId = Lens.lens (\UpdateAuthorizer' {authorizerId} -> authorizerId) (\s@UpdateAuthorizer' {} a -> s {authorizerId = a} :: UpdateAuthorizer)

instance Core.AWSRequest UpdateAuthorizer where
  type AWSResponse UpdateAuthorizer = Authorizer
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateAuthorizer

instance Prelude.NFData UpdateAuthorizer

instance Core.ToHeaders UpdateAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateAuthorizer where
  toPath UpdateAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/authorizers/",
        Core.toBS authorizerId
      ]

instance Core.ToQuery UpdateAuthorizer where
  toQuery = Prelude.const Prelude.mempty

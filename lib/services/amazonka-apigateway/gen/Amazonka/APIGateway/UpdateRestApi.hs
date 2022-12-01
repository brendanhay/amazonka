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
-- Module      : Amazonka.APIGateway.UpdateRestApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the specified API.
module Amazonka.APIGateway.UpdateRestApi
  ( -- * Creating a Request
    UpdateRestApi (..),
    newUpdateRestApi,

    -- * Request Lenses
    updateRestApi_patchOperations,
    updateRestApi_restApiId,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_tags,
    restApi_policy,
    restApi_name,
    restApi_description,
    restApi_id,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_createdDate,
    restApi_apiKeySource,
    restApi_minimumCompressionSize,
    restApi_version,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to update an existing RestApi resource in your collection.
--
-- /See:/ 'newUpdateRestApi' smart constructor.
data UpdateRestApi = UpdateRestApi'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateRestApi_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateRestApi_restApiId' - The string identifier of the associated RestApi.
newUpdateRestApi ::
  -- | 'restApiId'
  Prelude.Text ->
  UpdateRestApi
newUpdateRestApi pRestApiId_ =
  UpdateRestApi'
    { patchOperations = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateRestApi_patchOperations :: Lens.Lens' UpdateRestApi (Prelude.Maybe [PatchOperation])
updateRestApi_patchOperations = Lens.lens (\UpdateRestApi' {patchOperations} -> patchOperations) (\s@UpdateRestApi' {} a -> s {patchOperations = a} :: UpdateRestApi) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateRestApi_restApiId :: Lens.Lens' UpdateRestApi Prelude.Text
updateRestApi_restApiId = Lens.lens (\UpdateRestApi' {restApiId} -> restApiId) (\s@UpdateRestApi' {} a -> s {restApiId = a} :: UpdateRestApi)

instance Core.AWSRequest UpdateRestApi where
  type AWSResponse UpdateRestApi = RestApi
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateRestApi where
  hashWithSalt _salt UpdateRestApi' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData UpdateRestApi where
  rnf UpdateRestApi' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId

instance Core.ToHeaders UpdateRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateRestApi where
  toJSON UpdateRestApi' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateRestApi where
  toPath UpdateRestApi' {..} =
    Prelude.mconcat ["/restapis/", Core.toBS restApiId]

instance Core.ToQuery UpdateRestApi where
  toQuery = Prelude.const Prelude.mempty

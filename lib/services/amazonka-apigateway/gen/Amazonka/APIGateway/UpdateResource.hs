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
-- Module      : Amazonka.APIGateway.UpdateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a Resource resource.
module Amazonka.APIGateway.UpdateResource
  ( -- * Creating a Request
    UpdateResource (..),
    newUpdateResource,

    -- * Request Lenses
    updateResource_patchOperations,
    updateResource_restApiId,
    updateResource_resourceId,

    -- * Destructuring the Response
    Resource (..),
    newResource,

    -- * Response Lenses
    resource_id,
    resource_parentId,
    resource_path,
    resource_pathPart,
    resource_resourceMethods,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to change information about a Resource resource.
--
-- /See:/ 'newUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the Resource resource.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateResource_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateResource_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateResource_resourceId' - The identifier of the Resource resource.
newUpdateResource ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  UpdateResource
newUpdateResource pRestApiId_ pResourceId_ =
  UpdateResource'
    { patchOperations = Prelude.Nothing,
      restApiId = pRestApiId_,
      resourceId = pResourceId_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateResource_patchOperations :: Lens.Lens' UpdateResource (Prelude.Maybe [PatchOperation])
updateResource_patchOperations = Lens.lens (\UpdateResource' {patchOperations} -> patchOperations) (\s@UpdateResource' {} a -> s {patchOperations = a} :: UpdateResource) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateResource_restApiId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_restApiId = Lens.lens (\UpdateResource' {restApiId} -> restApiId) (\s@UpdateResource' {} a -> s {restApiId = a} :: UpdateResource)

-- | The identifier of the Resource resource.
updateResource_resourceId :: Lens.Lens' UpdateResource Prelude.Text
updateResource_resourceId = Lens.lens (\UpdateResource' {resourceId} -> resourceId) (\s@UpdateResource' {} a -> s {resourceId = a} :: UpdateResource)

instance Core.AWSRequest UpdateResource where
  type AWSResponse UpdateResource = Resource
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateResource where
  hashWithSalt _salt UpdateResource' {..} =
    _salt
      `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData UpdateResource where
  rnf UpdateResource' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders UpdateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateResource where
  toPath UpdateResource' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId
      ]

instance Data.ToQuery UpdateResource where
  toQuery = Prelude.const Prelude.mempty

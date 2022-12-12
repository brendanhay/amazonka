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
-- Module      : Amazonka.APIGateway.CreateResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resource resource.
module Amazonka.APIGateway.CreateResource
  ( -- * Creating a Request
    CreateResource (..),
    newCreateResource,

    -- * Request Lenses
    createResource_restApiId,
    createResource_parentId,
    createResource_pathPart,

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

-- | Requests API Gateway to create a Resource resource.
--
-- /See:/ 'newCreateResource' smart constructor.
data CreateResource = CreateResource'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The parent resource\'s identifier.
    parentId :: Prelude.Text,
    -- | The last path segment for this resource.
    pathPart :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'createResource_restApiId' - The string identifier of the associated RestApi.
--
-- 'parentId', 'createResource_parentId' - The parent resource\'s identifier.
--
-- 'pathPart', 'createResource_pathPart' - The last path segment for this resource.
newCreateResource ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'parentId'
  Prelude.Text ->
  -- | 'pathPart'
  Prelude.Text ->
  CreateResource
newCreateResource pRestApiId_ pParentId_ pPathPart_ =
  CreateResource'
    { restApiId = pRestApiId_,
      parentId = pParentId_,
      pathPart = pPathPart_
    }

-- | The string identifier of the associated RestApi.
createResource_restApiId :: Lens.Lens' CreateResource Prelude.Text
createResource_restApiId = Lens.lens (\CreateResource' {restApiId} -> restApiId) (\s@CreateResource' {} a -> s {restApiId = a} :: CreateResource)

-- | The parent resource\'s identifier.
createResource_parentId :: Lens.Lens' CreateResource Prelude.Text
createResource_parentId = Lens.lens (\CreateResource' {parentId} -> parentId) (\s@CreateResource' {} a -> s {parentId = a} :: CreateResource)

-- | The last path segment for this resource.
createResource_pathPart :: Lens.Lens' CreateResource Prelude.Text
createResource_pathPart = Lens.lens (\CreateResource' {pathPart} -> pathPart) (\s@CreateResource' {} a -> s {pathPart = a} :: CreateResource)

instance Core.AWSRequest CreateResource where
  type AWSResponse CreateResource = Resource
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateResource where
  hashWithSalt _salt CreateResource' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` pathPart

instance Prelude.NFData CreateResource where
  rnf CreateResource' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf pathPart

instance Data.ToHeaders CreateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("pathPart" Data..= pathPart)]
      )

instance Data.ToPath CreateResource where
  toPath CreateResource' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS parentId
      ]

instance Data.ToQuery CreateResource where
  toQuery = Prelude.const Prelude.mempty

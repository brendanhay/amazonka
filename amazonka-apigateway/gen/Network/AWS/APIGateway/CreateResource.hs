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
-- Module      : Network.AWS.APIGateway.CreateResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resource resource.
module Network.AWS.APIGateway.CreateResource
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
    resource_pathPart,
    resource_parentId,
    resource_resourceMethods,
    resource_path,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to create a Resource resource.
--
-- /See:/ 'newCreateResource' smart constructor.
data CreateResource = CreateResource'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The parent resource\'s identifier.
    parentId :: Core.Text,
    -- | The last path segment for this resource.
    pathPart :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'createResource_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'parentId', 'createResource_parentId' - [Required] The parent resource\'s identifier.
--
-- 'pathPart', 'createResource_pathPart' - The last path segment for this resource.
newCreateResource ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'parentId'
  Core.Text ->
  -- | 'pathPart'
  Core.Text ->
  CreateResource
newCreateResource pRestApiId_ pParentId_ pPathPart_ =
  CreateResource'
    { restApiId = pRestApiId_,
      parentId = pParentId_,
      pathPart = pPathPart_
    }

-- | [Required] The string identifier of the associated RestApi.
createResource_restApiId :: Lens.Lens' CreateResource Core.Text
createResource_restApiId = Lens.lens (\CreateResource' {restApiId} -> restApiId) (\s@CreateResource' {} a -> s {restApiId = a} :: CreateResource)

-- | [Required] The parent resource\'s identifier.
createResource_parentId :: Lens.Lens' CreateResource Core.Text
createResource_parentId = Lens.lens (\CreateResource' {parentId} -> parentId) (\s@CreateResource' {} a -> s {parentId = a} :: CreateResource)

-- | The last path segment for this resource.
createResource_pathPart :: Lens.Lens' CreateResource Core.Text
createResource_pathPart = Lens.lens (\CreateResource' {pathPart} -> pathPart) (\s@CreateResource' {} a -> s {pathPart = a} :: CreateResource)

instance Core.AWSRequest CreateResource where
  type AWSResponse CreateResource = Resource
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateResource

instance Core.NFData CreateResource

instance Core.ToHeaders CreateResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("pathPart" Core..= pathPart)]
      )

instance Core.ToPath CreateResource where
  toPath CreateResource' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS parentId
      ]

instance Core.ToQuery CreateResource where
  toQuery = Core.const Core.mempty

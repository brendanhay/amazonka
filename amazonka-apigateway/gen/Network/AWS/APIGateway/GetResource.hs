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
-- Module      : Network.AWS.APIGateway.GetResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a resource.
module Network.AWS.APIGateway.GetResource
  ( -- * Creating a Request
    GetResource (..),
    newGetResource,

    -- * Request Lenses
    getResource_embed,
    getResource_restApiId,
    getResource_resourceId,

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

-- | Request to list information about a resource.
--
-- /See:/ 'newGetResource' smart constructor.
data GetResource = GetResource'
  { -- | A query parameter to retrieve the specified resources embedded in the
    -- returned Resource representation in the response. This @embed@ parameter
    -- value is a list of comma-separated strings. Currently, the request
    -- supports only retrieval of the embedded Method resources this way. The
    -- query parameter value must be a single-valued list and contain the
    -- @\"methods\"@ string. For example,
    -- @GET \/restapis\/{restapi_id}\/resources\/{resource_id}?embed=methods@.
    embed :: Core.Maybe [Core.Text],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The identifier for the Resource resource.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'embed', 'getResource_embed' - A query parameter to retrieve the specified resources embedded in the
-- returned Resource representation in the response. This @embed@ parameter
-- value is a list of comma-separated strings. Currently, the request
-- supports only retrieval of the embedded Method resources this way. The
-- query parameter value must be a single-valued list and contain the
-- @\"methods\"@ string. For example,
-- @GET \/restapis\/{restapi_id}\/resources\/{resource_id}?embed=methods@.
--
-- 'restApiId', 'getResource_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getResource_resourceId' - [Required] The identifier for the Resource resource.
newGetResource ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  GetResource
newGetResource pRestApiId_ pResourceId_ =
  GetResource'
    { embed = Core.Nothing,
      restApiId = pRestApiId_,
      resourceId = pResourceId_
    }

-- | A query parameter to retrieve the specified resources embedded in the
-- returned Resource representation in the response. This @embed@ parameter
-- value is a list of comma-separated strings. Currently, the request
-- supports only retrieval of the embedded Method resources this way. The
-- query parameter value must be a single-valued list and contain the
-- @\"methods\"@ string. For example,
-- @GET \/restapis\/{restapi_id}\/resources\/{resource_id}?embed=methods@.
getResource_embed :: Lens.Lens' GetResource (Core.Maybe [Core.Text])
getResource_embed = Lens.lens (\GetResource' {embed} -> embed) (\s@GetResource' {} a -> s {embed = a} :: GetResource) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
getResource_restApiId :: Lens.Lens' GetResource Core.Text
getResource_restApiId = Lens.lens (\GetResource' {restApiId} -> restApiId) (\s@GetResource' {} a -> s {restApiId = a} :: GetResource)

-- | [Required] The identifier for the Resource resource.
getResource_resourceId :: Lens.Lens' GetResource Core.Text
getResource_resourceId = Lens.lens (\GetResource' {resourceId} -> resourceId) (\s@GetResource' {} a -> s {resourceId = a} :: GetResource)

instance Core.AWSRequest GetResource where
  type AWSResponse GetResource = Resource
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetResource

instance Core.NFData GetResource

instance Core.ToHeaders GetResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetResource where
  toPath GetResource' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId
      ]

instance Core.ToQuery GetResource where
  toQuery GetResource' {..} =
    Core.mconcat
      [ "embed"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> embed)
      ]

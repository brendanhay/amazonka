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
-- Module      : Network.AWS.APIGateway.GetRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RestApi resource in the collection.
module Network.AWS.APIGateway.GetRestApi
  ( -- * Creating a Request
    GetRestApi (..),
    newGetRestApi,

    -- * Request Lenses
    getRestApi_restApiId,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to list an existing RestApi defined for your collection.
--
-- /See:/ 'newGetRestApi' smart constructor.
data GetRestApi = GetRestApi'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getRestApi_restApiId' - [Required] The string identifier of the associated RestApi.
newGetRestApi ::
  -- | 'restApiId'
  Core.Text ->
  GetRestApi
newGetRestApi pRestApiId_ =
  GetRestApi' {restApiId = pRestApiId_}

-- | [Required] The string identifier of the associated RestApi.
getRestApi_restApiId :: Lens.Lens' GetRestApi Core.Text
getRestApi_restApiId = Lens.lens (\GetRestApi' {restApiId} -> restApiId) (\s@GetRestApi' {} a -> s {restApiId = a} :: GetRestApi)

instance Core.AWSRequest GetRestApi where
  type AWSResponse GetRestApi = RestApi
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetRestApi

instance Core.NFData GetRestApi

instance Core.ToHeaders GetRestApi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetRestApi where
  toPath GetRestApi' {..} =
    Core.mconcat ["/restapis/", Core.toBS restApiId]

instance Core.ToQuery GetRestApi where
  toQuery = Core.const Core.mempty

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
-- Module      : Amazonka.APIGateway.GetRestApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RestApi resource in the collection.
module Amazonka.APIGateway.GetRestApi
  ( -- * Creating a Request
    GetRestApi (..),
    newGetRestApi,

    -- * Request Lenses
    getRestApi_restApiId,

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

-- | The GET request to list an existing RestApi defined for your collection.
--
-- /See:/ 'newGetRestApi' smart constructor.
data GetRestApi = GetRestApi'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getRestApi_restApiId' - The string identifier of the associated RestApi.
newGetRestApi ::
  -- | 'restApiId'
  Prelude.Text ->
  GetRestApi
newGetRestApi pRestApiId_ =
  GetRestApi' {restApiId = pRestApiId_}

-- | The string identifier of the associated RestApi.
getRestApi_restApiId :: Lens.Lens' GetRestApi Prelude.Text
getRestApi_restApiId = Lens.lens (\GetRestApi' {restApiId} -> restApiId) (\s@GetRestApi' {} a -> s {restApiId = a} :: GetRestApi)

instance Core.AWSRequest GetRestApi where
  type AWSResponse GetRestApi = RestApi
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetRestApi where
  hashWithSalt _salt GetRestApi' {..} =
    _salt `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetRestApi where
  rnf GetRestApi' {..} = Prelude.rnf restApiId

instance Core.ToHeaders GetRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetRestApi where
  toPath GetRestApi' {..} =
    Prelude.mconcat ["/restapis/", Core.toBS restApiId]

instance Core.ToQuery GetRestApi where
  toQuery = Prelude.const Prelude.mempty

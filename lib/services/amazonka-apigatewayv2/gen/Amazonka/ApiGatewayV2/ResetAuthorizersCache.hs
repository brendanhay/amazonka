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
-- Module      : Amazonka.ApiGatewayV2.ResetAuthorizersCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all authorizer cache entries on a stage. Supported only for HTTP
-- APIs.
module Amazonka.ApiGatewayV2.ResetAuthorizersCache
  ( -- * Creating a Request
    ResetAuthorizersCache (..),
    newResetAuthorizersCache,

    -- * Request Lenses
    resetAuthorizersCache_stageName,
    resetAuthorizersCache_apiId,

    -- * Destructuring the Response
    ResetAuthorizersCacheResponse (..),
    newResetAuthorizersCacheResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetAuthorizersCache' smart constructor.
data ResetAuthorizersCache = ResetAuthorizersCache'
  { -- | The stage name. Stage names can contain only alphanumeric characters,
    -- hyphens, and underscores, or be $default. Maximum length is 128
    -- characters.
    stageName :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAuthorizersCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'resetAuthorizersCache_stageName' - The stage name. Stage names can contain only alphanumeric characters,
-- hyphens, and underscores, or be $default. Maximum length is 128
-- characters.
--
-- 'apiId', 'resetAuthorizersCache_apiId' - The API identifier.
newResetAuthorizersCache ::
  -- | 'stageName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  ResetAuthorizersCache
newResetAuthorizersCache pStageName_ pApiId_ =
  ResetAuthorizersCache'
    { stageName = pStageName_,
      apiId = pApiId_
    }

-- | The stage name. Stage names can contain only alphanumeric characters,
-- hyphens, and underscores, or be $default. Maximum length is 128
-- characters.
resetAuthorizersCache_stageName :: Lens.Lens' ResetAuthorizersCache Prelude.Text
resetAuthorizersCache_stageName = Lens.lens (\ResetAuthorizersCache' {stageName} -> stageName) (\s@ResetAuthorizersCache' {} a -> s {stageName = a} :: ResetAuthorizersCache)

-- | The API identifier.
resetAuthorizersCache_apiId :: Lens.Lens' ResetAuthorizersCache Prelude.Text
resetAuthorizersCache_apiId = Lens.lens (\ResetAuthorizersCache' {apiId} -> apiId) (\s@ResetAuthorizersCache' {} a -> s {apiId = a} :: ResetAuthorizersCache)

instance Core.AWSRequest ResetAuthorizersCache where
  type
    AWSResponse ResetAuthorizersCache =
      ResetAuthorizersCacheResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull ResetAuthorizersCacheResponse'

instance Prelude.Hashable ResetAuthorizersCache where
  hashWithSalt _salt ResetAuthorizersCache' {..} =
    _salt
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData ResetAuthorizersCache where
  rnf ResetAuthorizersCache' {..} =
    Prelude.rnf stageName `Prelude.seq`
      Prelude.rnf apiId

instance Data.ToHeaders ResetAuthorizersCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ResetAuthorizersCache where
  toPath ResetAuthorizersCache' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/stages/",
        Data.toBS stageName,
        "/cache/authorizers"
      ]

instance Data.ToQuery ResetAuthorizersCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetAuthorizersCacheResponse' smart constructor.
data ResetAuthorizersCacheResponse = ResetAuthorizersCacheResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetAuthorizersCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetAuthorizersCacheResponse ::
  ResetAuthorizersCacheResponse
newResetAuthorizersCacheResponse =
  ResetAuthorizersCacheResponse'

instance Prelude.NFData ResetAuthorizersCacheResponse where
  rnf _ = ()

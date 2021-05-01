{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.FlushStageAuthorizersCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes all authorizer cache entries on a stage.
module Network.AWS.APIGateway.FlushStageAuthorizersCache
  ( -- * Creating a Request
    FlushStageAuthorizersCache (..),
    newFlushStageAuthorizersCache,

    -- * Request Lenses
    flushStageAuthorizersCache_restApiId,
    flushStageAuthorizersCache_stageName,

    -- * Destructuring the Response
    FlushStageAuthorizersCacheResponse (..),
    newFlushStageAuthorizersCacheResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to flush authorizer cache entries on a specified stage.
--
-- /See:/ 'newFlushStageAuthorizersCache' smart constructor.
data FlushStageAuthorizersCache = FlushStageAuthorizersCache'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the stage to flush.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushStageAuthorizersCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'flushStageAuthorizersCache_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'flushStageAuthorizersCache_stageName' - The name of the stage to flush.
newFlushStageAuthorizersCache ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  FlushStageAuthorizersCache
newFlushStageAuthorizersCache pRestApiId_ pStageName_ =
  FlushStageAuthorizersCache'
    { restApiId =
        pRestApiId_,
      stageName = pStageName_
    }

-- | The string identifier of the associated RestApi.
flushStageAuthorizersCache_restApiId :: Lens.Lens' FlushStageAuthorizersCache Prelude.Text
flushStageAuthorizersCache_restApiId = Lens.lens (\FlushStageAuthorizersCache' {restApiId} -> restApiId) (\s@FlushStageAuthorizersCache' {} a -> s {restApiId = a} :: FlushStageAuthorizersCache)

-- | The name of the stage to flush.
flushStageAuthorizersCache_stageName :: Lens.Lens' FlushStageAuthorizersCache Prelude.Text
flushStageAuthorizersCache_stageName = Lens.lens (\FlushStageAuthorizersCache' {stageName} -> stageName) (\s@FlushStageAuthorizersCache' {} a -> s {stageName = a} :: FlushStageAuthorizersCache)

instance
  Prelude.AWSRequest
    FlushStageAuthorizersCache
  where
  type
    Rs FlushStageAuthorizersCache =
      FlushStageAuthorizersCacheResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      FlushStageAuthorizersCacheResponse'

instance Prelude.Hashable FlushStageAuthorizersCache

instance Prelude.NFData FlushStageAuthorizersCache

instance Prelude.ToHeaders FlushStageAuthorizersCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath FlushStageAuthorizersCache where
  toPath FlushStageAuthorizersCache' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/stages/",
        Prelude.toBS stageName,
        "/cache/authorizers"
      ]

instance Prelude.ToQuery FlushStageAuthorizersCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFlushStageAuthorizersCacheResponse' smart constructor.
data FlushStageAuthorizersCacheResponse = FlushStageAuthorizersCacheResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushStageAuthorizersCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newFlushStageAuthorizersCacheResponse ::
  FlushStageAuthorizersCacheResponse
newFlushStageAuthorizersCacheResponse =
  FlushStageAuthorizersCacheResponse'

instance
  Prelude.NFData
    FlushStageAuthorizersCacheResponse

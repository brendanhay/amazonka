{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.FlushStageAuthorizersCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes all authorizer cache entries on a stage.
module Network.AWS.APIGateway.FlushStageAuthorizersCache
  ( -- * Creating a request
    FlushStageAuthorizersCache (..),
    mkFlushStageAuthorizersCache,

    -- ** Request lenses
    fsacRestAPIId,
    fsacStageName,

    -- * Destructuring the response
    FlushStageAuthorizersCacheResponse (..),
    mkFlushStageAuthorizersCacheResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to flush authorizer cache entries on a specified stage.
--
-- /See:/ 'mkFlushStageAuthorizersCache' smart constructor.
data FlushStageAuthorizersCache = FlushStageAuthorizersCache'
  { -- | The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The name of the stage to flush.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlushStageAuthorizersCache' with the minimum fields required to make a request.
--
-- * 'restAPIId' - The string identifier of the associated 'RestApi' .
-- * 'stageName' - The name of the stage to flush.
mkFlushStageAuthorizersCache ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  FlushStageAuthorizersCache
mkFlushStageAuthorizersCache pRestAPIId_ pStageName_ =
  FlushStageAuthorizersCache'
    { restAPIId = pRestAPIId_,
      stageName = pStageName_
    }

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsacRestAPIId :: Lens.Lens' FlushStageAuthorizersCache Lude.Text
fsacRestAPIId = Lens.lens (restAPIId :: FlushStageAuthorizersCache -> Lude.Text) (\s a -> s {restAPIId = a} :: FlushStageAuthorizersCache)
{-# DEPRECATED fsacRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The name of the stage to flush.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsacStageName :: Lens.Lens' FlushStageAuthorizersCache Lude.Text
fsacStageName = Lens.lens (stageName :: FlushStageAuthorizersCache -> Lude.Text) (\s a -> s {stageName = a} :: FlushStageAuthorizersCache)
{-# DEPRECATED fsacStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest FlushStageAuthorizersCache where
  type
    Rs FlushStageAuthorizersCache =
      FlushStageAuthorizersCacheResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull FlushStageAuthorizersCacheResponse'

instance Lude.ToHeaders FlushStageAuthorizersCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath FlushStageAuthorizersCache where
  toPath FlushStageAuthorizersCache' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName,
        "/cache/authorizers"
      ]

instance Lude.ToQuery FlushStageAuthorizersCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkFlushStageAuthorizersCacheResponse' smart constructor.
data FlushStageAuthorizersCacheResponse = FlushStageAuthorizersCacheResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlushStageAuthorizersCacheResponse' with the minimum fields required to make a request.
mkFlushStageAuthorizersCacheResponse ::
  FlushStageAuthorizersCacheResponse
mkFlushStageAuthorizersCacheResponse =
  FlushStageAuthorizersCacheResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.FlushStageCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes a stage's cache.
module Network.AWS.APIGateway.FlushStageCache
  ( -- * Creating a request
    FlushStageCache (..),
    mkFlushStageCache,

    -- ** Request lenses
    fscRestAPIId,
    fscStageName,

    -- * Destructuring the response
    FlushStageCacheResponse (..),
    mkFlushStageCacheResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to flush a stage's cache.
--
-- /See:/ 'mkFlushStageCache' smart constructor.
data FlushStageCache = FlushStageCache'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The name of the stage to flush its cache.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlushStageCache' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageName' - [Required] The name of the stage to flush its cache.
mkFlushStageCache ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  FlushStageCache
mkFlushStageCache pRestAPIId_ pStageName_ =
  FlushStageCache'
    { restAPIId = pRestAPIId_,
      stageName = pStageName_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscRestAPIId :: Lens.Lens' FlushStageCache Lude.Text
fscRestAPIId = Lens.lens (restAPIId :: FlushStageCache -> Lude.Text) (\s a -> s {restAPIId = a} :: FlushStageCache)
{-# DEPRECATED fscRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The name of the stage to flush its cache.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscStageName :: Lens.Lens' FlushStageCache Lude.Text
fscStageName = Lens.lens (stageName :: FlushStageCache -> Lude.Text) (\s a -> s {stageName = a} :: FlushStageCache)
{-# DEPRECATED fscStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest FlushStageCache where
  type Rs FlushStageCache = FlushStageCacheResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull FlushStageCacheResponse'

instance Lude.ToHeaders FlushStageCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath FlushStageCache where
  toPath FlushStageCache' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName,
        "/cache/data"
      ]

instance Lude.ToQuery FlushStageCache where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkFlushStageCacheResponse' smart constructor.
data FlushStageCacheResponse = FlushStageCacheResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlushStageCacheResponse' with the minimum fields required to make a request.
mkFlushStageCacheResponse ::
  FlushStageCacheResponse
mkFlushStageCacheResponse = FlushStageCacheResponse'

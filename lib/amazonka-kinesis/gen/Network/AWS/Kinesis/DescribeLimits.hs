{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the shard limits and usage for the account.
--
-- If you update your account limits, the old limits might be returned for a few minutes.
-- This operation has a limit of one transaction per second per account.
module Network.AWS.Kinesis.DescribeLimits
  ( -- * Creating a request
    DescribeLimits (..),
    mkDescribeLimits,

    -- * Destructuring the response
    DescribeLimitsResponse (..),
    mkDescribeLimitsResponse,

    -- ** Response lenses
    dlrsOpenShardCount,
    dlrsShardLimit,
    dlrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLimits' with the minimum fields required to make a request.
mkDescribeLimits ::
  DescribeLimits
mkDescribeLimits = DescribeLimits'

instance Lude.AWSRequest DescribeLimits where
  type Rs DescribeLimits = DescribeLimitsResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Lude.<$> (x Lude..:> "OpenShardCount")
            Lude.<*> (x Lude..:> "ShardLimit")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLimits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DescribeLimits" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLimits where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLimits where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { -- | The number of open shards.
    openShardCount :: Lude.Natural,
    -- | The maximum number of shards.
    shardLimit :: Lude.Natural,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLimitsResponse' with the minimum fields required to make a request.
--
-- * 'openShardCount' - The number of open shards.
-- * 'shardLimit' - The maximum number of shards.
-- * 'responseStatus' - The response status code.
mkDescribeLimitsResponse ::
  -- | 'openShardCount'
  Lude.Natural ->
  -- | 'shardLimit'
  Lude.Natural ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLimitsResponse
mkDescribeLimitsResponse
  pOpenShardCount_
  pShardLimit_
  pResponseStatus_ =
    DescribeLimitsResponse'
      { openShardCount = pOpenShardCount_,
        shardLimit = pShardLimit_,
        responseStatus = pResponseStatus_
      }

-- | The number of open shards.
--
-- /Note:/ Consider using 'openShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsOpenShardCount :: Lens.Lens' DescribeLimitsResponse Lude.Natural
dlrsOpenShardCount = Lens.lens (openShardCount :: DescribeLimitsResponse -> Lude.Natural) (\s a -> s {openShardCount = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsOpenShardCount "Use generic-lens or generic-optics with 'openShardCount' instead." #-}

-- | The maximum number of shards.
--
-- /Note:/ Consider using 'shardLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsShardLimit :: Lens.Lens' DescribeLimitsResponse Lude.Natural
dlrsShardLimit = Lens.lens (shardLimit :: DescribeLimitsResponse -> Lude.Natural) (\s a -> s {shardLimit = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsShardLimit "Use generic-lens or generic-optics with 'shardLimit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DescribeLimitsResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DescribeLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

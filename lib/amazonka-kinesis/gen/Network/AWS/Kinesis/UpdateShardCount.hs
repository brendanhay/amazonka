{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.UpdateShardCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shard count of the specified stream to the specified number of shards.
--
-- Updating the shard count is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Depending on the size of the stream, the scaling action could take a few minutes to complete. You can continue to read and write data to your stream while its status is @UPDATING@ .
-- To update the shard count, Kinesis Data Streams performs splits or merges on individual shards. This can cause short-lived shards to be created, in addition to the final shards. These short-lived shards count towards your total shard limit for your account in the Region.
-- When using this operation, we recommend that you specify a target shard count that is a multiple of 25% (25%, 50%, 75%, 100%). You can specify any target value within your shard limit. However, if you specify a target that isn't a multiple of 25%, the scaling action might take longer to complete.
-- This operation has the following default limits. By default, you cannot do the following:
--
--     * Scale more than ten times per rolling 24-hour period per stream
--
--
--     * Scale up to more than double your current shard count for a stream
--
--
--     * Scale down below half your current shard count for a stream
--
--
--     * Scale up to more than 500 shards in a stream
--
--
--     * Scale a stream with more than 500 shards down unless the result is less than 500 shards
--
--
--     * Scale up to more than the shard limit for your account
--
--
-- For the default limits for an AWS account, see <https://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To request an increase in the call rate limit, the shard limit for this API, or your overall shard limit, use the <https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase&limitType=service-code-kinesis limits form> .
module Network.AWS.Kinesis.UpdateShardCount
  ( -- * Creating a request
    UpdateShardCount (..),
    mkUpdateShardCount,

    -- ** Request lenses
    uscScalingType,
    uscTargetShardCount,
    uscStreamName,

    -- * Destructuring the response
    UpdateShardCountResponse (..),
    mkUpdateShardCountResponse,

    -- ** Response lenses
    uscrsTargetShardCount,
    uscrsStreamName,
    uscrsCurrentShardCount,
    uscrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateShardCount' smart constructor.
data UpdateShardCount = UpdateShardCount'
  { -- | The scaling type. Uniform scaling creates shards of equal size.
    scalingType :: ScalingType,
    -- | The new number of shards. This value has the following default limits. By default, you cannot do the following:
    --
    --
    --     * Set this value to more than double your current shard count for a stream.
    --
    --
    --     * Set this value below half your current shard count for a stream.
    --
    --
    --     * Set this value to more than 500 shards in a stream (the default limit for shard count per stream is 500 per account per region), unless you request a limit increase.
    --
    --
    --     * Scale a stream with more than 500 shards down unless you set this value to less than 500 shards.
    targetShardCount :: Lude.Natural,
    -- | The name of the stream.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateShardCount' with the minimum fields required to make a request.
--
-- * 'scalingType' - The scaling type. Uniform scaling creates shards of equal size.
-- * 'targetShardCount' - The new number of shards. This value has the following default limits. By default, you cannot do the following:
--
--
--     * Set this value to more than double your current shard count for a stream.
--
--
--     * Set this value below half your current shard count for a stream.
--
--
--     * Set this value to more than 500 shards in a stream (the default limit for shard count per stream is 500 per account per region), unless you request a limit increase.
--
--
--     * Scale a stream with more than 500 shards down unless you set this value to less than 500 shards.
--
--
-- * 'streamName' - The name of the stream.
mkUpdateShardCount ::
  -- | 'scalingType'
  ScalingType ->
  -- | 'targetShardCount'
  Lude.Natural ->
  -- | 'streamName'
  Lude.Text ->
  UpdateShardCount
mkUpdateShardCount pScalingType_ pTargetShardCount_ pStreamName_ =
  UpdateShardCount'
    { scalingType = pScalingType_,
      targetShardCount = pTargetShardCount_,
      streamName = pStreamName_
    }

-- | The scaling type. Uniform scaling creates shards of equal size.
--
-- /Note:/ Consider using 'scalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscScalingType :: Lens.Lens' UpdateShardCount ScalingType
uscScalingType = Lens.lens (scalingType :: UpdateShardCount -> ScalingType) (\s a -> s {scalingType = a} :: UpdateShardCount)
{-# DEPRECATED uscScalingType "Use generic-lens or generic-optics with 'scalingType' instead." #-}

-- | The new number of shards. This value has the following default limits. By default, you cannot do the following:
--
--
--     * Set this value to more than double your current shard count for a stream.
--
--
--     * Set this value below half your current shard count for a stream.
--
--
--     * Set this value to more than 500 shards in a stream (the default limit for shard count per stream is 500 per account per region), unless you request a limit increase.
--
--
--     * Scale a stream with more than 500 shards down unless you set this value to less than 500 shards.
--
--
--
-- /Note:/ Consider using 'targetShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscTargetShardCount :: Lens.Lens' UpdateShardCount Lude.Natural
uscTargetShardCount = Lens.lens (targetShardCount :: UpdateShardCount -> Lude.Natural) (\s a -> s {targetShardCount = a} :: UpdateShardCount)
{-# DEPRECATED uscTargetShardCount "Use generic-lens or generic-optics with 'targetShardCount' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscStreamName :: Lens.Lens' UpdateShardCount Lude.Text
uscStreamName = Lens.lens (streamName :: UpdateShardCount -> Lude.Text) (\s a -> s {streamName = a} :: UpdateShardCount)
{-# DEPRECATED uscStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest UpdateShardCount where
  type Rs UpdateShardCount = UpdateShardCountResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateShardCountResponse'
            Lude.<$> (x Lude..?> "TargetShardCount")
            Lude.<*> (x Lude..?> "StreamName")
            Lude.<*> (x Lude..?> "CurrentShardCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateShardCount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.UpdateShardCount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateShardCount where
  toJSON UpdateShardCount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ScalingType" Lude..= scalingType),
            Lude.Just ("TargetShardCount" Lude..= targetShardCount),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath UpdateShardCount where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateShardCount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateShardCountResponse' smart constructor.
data UpdateShardCountResponse = UpdateShardCountResponse'
  { -- | The updated number of shards.
    targetShardCount :: Lude.Maybe Lude.Natural,
    -- | The name of the stream.
    streamName :: Lude.Maybe Lude.Text,
    -- | The current number of shards.
    currentShardCount :: Lude.Maybe Lude.Natural,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateShardCountResponse' with the minimum fields required to make a request.
--
-- * 'targetShardCount' - The updated number of shards.
-- * 'streamName' - The name of the stream.
-- * 'currentShardCount' - The current number of shards.
-- * 'responseStatus' - The response status code.
mkUpdateShardCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateShardCountResponse
mkUpdateShardCountResponse pResponseStatus_ =
  UpdateShardCountResponse'
    { targetShardCount = Lude.Nothing,
      streamName = Lude.Nothing,
      currentShardCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated number of shards.
--
-- /Note:/ Consider using 'targetShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsTargetShardCount :: Lens.Lens' UpdateShardCountResponse (Lude.Maybe Lude.Natural)
uscrsTargetShardCount = Lens.lens (targetShardCount :: UpdateShardCountResponse -> Lude.Maybe Lude.Natural) (\s a -> s {targetShardCount = a} :: UpdateShardCountResponse)
{-# DEPRECATED uscrsTargetShardCount "Use generic-lens or generic-optics with 'targetShardCount' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsStreamName :: Lens.Lens' UpdateShardCountResponse (Lude.Maybe Lude.Text)
uscrsStreamName = Lens.lens (streamName :: UpdateShardCountResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: UpdateShardCountResponse)
{-# DEPRECATED uscrsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The current number of shards.
--
-- /Note:/ Consider using 'currentShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsCurrentShardCount :: Lens.Lens' UpdateShardCountResponse (Lude.Maybe Lude.Natural)
uscrsCurrentShardCount = Lens.lens (currentShardCount :: UpdateShardCountResponse -> Lude.Maybe Lude.Natural) (\s a -> s {currentShardCount = a} :: UpdateShardCountResponse)
{-# DEPRECATED uscrsCurrentShardCount "Use generic-lens or generic-optics with 'currentShardCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsResponseStatus :: Lens.Lens' UpdateShardCountResponse Lude.Int
uscrsResponseStatus = Lens.lens (responseStatus :: UpdateShardCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateShardCountResponse)
{-# DEPRECATED uscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

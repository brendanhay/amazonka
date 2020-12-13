{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UpdateDataRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases or decreases the stream's data retention period by the value that you specify. To indicate whether you want to increase or decrease the data retention period, specify the @Operation@ parameter in the request body. In the request, you must specify either the @StreamName@ or the @StreamARN@ .
--
-- This operation requires permission for the @KinesisVideo:UpdateDataRetention@ action.
-- Changing the data retention period affects the data in the stream as follows:
--
--     * If the data retention period is increased, existing data is retained for the new retention period. For example, if the data retention period is increased from one hour to seven hours, all existing data is retained for seven hours.
--
--
--     * If the data retention period is decreased, existing data is retained for the new retention period. For example, if the data retention period is decreased from seven hours to one hour, all existing data is retained for one hour, and any data older than one hour is deleted immediately.
module Network.AWS.KinesisVideo.UpdateDataRetention
  ( -- * Creating a request
    UpdateDataRetention (..),
    mkUpdateDataRetention,

    -- ** Request lenses
    udrOperation,
    udrCurrentVersion,
    udrDataRetentionChangeInHours,
    udrStreamARN,
    udrStreamName,

    -- * Destructuring the response
    UpdateDataRetentionResponse (..),
    mkUpdateDataRetentionResponse,

    -- ** Response lenses
    udrrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDataRetention' smart constructor.
data UpdateDataRetention = UpdateDataRetention'
  { -- | Indicates whether you want to increase or decrease the retention period.
    operation :: UpdateDataRetentionOperation,
    -- | The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
    currentVersion :: Lude.Text,
    -- | The retention period, in hours. The value you specify replaces the current value. The maximum value for this parameter is 87600 (ten years).
    dataRetentionChangeInHours :: Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The name of the stream whose retention period you want to change.
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataRetention' with the minimum fields required to make a request.
--
-- * 'operation' - Indicates whether you want to increase or decrease the retention period.
-- * 'currentVersion' - The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
-- * 'dataRetentionChangeInHours' - The retention period, in hours. The value you specify replaces the current value. The maximum value for this parameter is 87600 (ten years).
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
-- * 'streamName' - The name of the stream whose retention period you want to change.
mkUpdateDataRetention ::
  -- | 'operation'
  UpdateDataRetentionOperation ->
  -- | 'currentVersion'
  Lude.Text ->
  -- | 'dataRetentionChangeInHours'
  Lude.Natural ->
  UpdateDataRetention
mkUpdateDataRetention
  pOperation_
  pCurrentVersion_
  pDataRetentionChangeInHours_ =
    UpdateDataRetention'
      { operation = pOperation_,
        currentVersion = pCurrentVersion_,
        dataRetentionChangeInHours = pDataRetentionChangeInHours_,
        streamARN = Lude.Nothing,
        streamName = Lude.Nothing
      }

-- | Indicates whether you want to increase or decrease the retention period.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrOperation :: Lens.Lens' UpdateDataRetention UpdateDataRetentionOperation
udrOperation = Lens.lens (operation :: UpdateDataRetention -> UpdateDataRetentionOperation) (\s a -> s {operation = a} :: UpdateDataRetention)
{-# DEPRECATED udrOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrCurrentVersion :: Lens.Lens' UpdateDataRetention Lude.Text
udrCurrentVersion = Lens.lens (currentVersion :: UpdateDataRetention -> Lude.Text) (\s a -> s {currentVersion = a} :: UpdateDataRetention)
{-# DEPRECATED udrCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The retention period, in hours. The value you specify replaces the current value. The maximum value for this parameter is 87600 (ten years).
--
-- /Note:/ Consider using 'dataRetentionChangeInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrDataRetentionChangeInHours :: Lens.Lens' UpdateDataRetention Lude.Natural
udrDataRetentionChangeInHours = Lens.lens (dataRetentionChangeInHours :: UpdateDataRetention -> Lude.Natural) (\s a -> s {dataRetentionChangeInHours = a} :: UpdateDataRetention)
{-# DEPRECATED udrDataRetentionChangeInHours "Use generic-lens or generic-optics with 'dataRetentionChangeInHours' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrStreamARN :: Lens.Lens' UpdateDataRetention (Lude.Maybe Lude.Text)
udrStreamARN = Lens.lens (streamARN :: UpdateDataRetention -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: UpdateDataRetention)
{-# DEPRECATED udrStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream whose retention period you want to change.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrStreamName :: Lens.Lens' UpdateDataRetention (Lude.Maybe Lude.Text)
udrStreamName = Lens.lens (streamName :: UpdateDataRetention -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: UpdateDataRetention)
{-# DEPRECATED udrStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest UpdateDataRetention where
  type Rs UpdateDataRetention = UpdateDataRetentionResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDataRetentionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDataRetention where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateDataRetention where
  toJSON UpdateDataRetention' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Operation" Lude..= operation),
            Lude.Just ("CurrentVersion" Lude..= currentVersion),
            Lude.Just
              ("DataRetentionChangeInHours" Lude..= dataRetentionChangeInHours),
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath UpdateDataRetention where
  toPath = Lude.const "/updateDataRetention"

instance Lude.ToQuery UpdateDataRetention where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDataRetentionResponse' smart constructor.
newtype UpdateDataRetentionResponse = UpdateDataRetentionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataRetentionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDataRetentionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDataRetentionResponse
mkUpdateDataRetentionResponse pResponseStatus_ =
  UpdateDataRetentionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDataRetentionResponse Lude.Int
udrrsResponseStatus = Lens.lens (responseStatus :: UpdateDataRetentionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDataRetentionResponse)
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

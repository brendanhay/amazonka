{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a real-time log configuration.
--
-- When you update a real-time log configuration, all the parameters are updated with the values provided in the request. You cannot update some parameters independent of others. To update a real-time log configuration:
--
--     * Call @GetRealtimeLogConfig@ to get the current real-time log configuration.
--
--
--     * Locally modify the parameters in the real-time log configuration that you want to update.
--
--
--     * Call this API (@UpdateRealtimeLogConfig@ ) by providing the entire real-time log configuration, including the parameters that you modified and those that you didn’t.
--
--
-- You cannot update a real-time log configuration’s @Name@ or @ARN@ .
module Network.AWS.CloudFront.UpdateRealtimeLogConfig
  ( -- * Creating a request
    UpdateRealtimeLogConfig (..),
    mkUpdateRealtimeLogConfig,

    -- ** Request lenses
    urlcARN,
    urlcSamplingRate,
    urlcName,
    urlcEndPoints,
    urlcFields,

    -- * Destructuring the response
    UpdateRealtimeLogConfigResponse (..),
    mkUpdateRealtimeLogConfigResponse,

    -- ** Response lenses
    urlcrsRealtimeLogConfig,
    urlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRealtimeLogConfig' smart constructor.
data UpdateRealtimeLogConfig = UpdateRealtimeLogConfig'
  { arn ::
      Lude.Maybe Lude.Text,
    samplingRate :: Lude.Maybe Lude.Integer,
    name :: Lude.Maybe Lude.Text,
    endPoints :: Lude.Maybe [EndPoint],
    fields :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for this real-time log configuration.
-- * 'endPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
-- * 'fields' - A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
-- * 'name' - The name for this real-time log configuration.
-- * 'samplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
mkUpdateRealtimeLogConfig ::
  UpdateRealtimeLogConfig
mkUpdateRealtimeLogConfig =
  UpdateRealtimeLogConfig'
    { arn = Lude.Nothing,
      samplingRate = Lude.Nothing,
      name = Lude.Nothing,
      endPoints = Lude.Nothing,
      fields = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for this real-time log configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcARN :: Lens.Lens' UpdateRealtimeLogConfig (Lude.Maybe Lude.Text)
urlcARN = Lens.lens (arn :: UpdateRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateRealtimeLogConfig)
{-# DEPRECATED urlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcSamplingRate :: Lens.Lens' UpdateRealtimeLogConfig (Lude.Maybe Lude.Integer)
urlcSamplingRate = Lens.lens (samplingRate :: UpdateRealtimeLogConfig -> Lude.Maybe Lude.Integer) (\s a -> s {samplingRate = a} :: UpdateRealtimeLogConfig)
{-# DEPRECATED urlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

-- | The name for this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcName :: Lens.Lens' UpdateRealtimeLogConfig (Lude.Maybe Lude.Text)
urlcName = Lens.lens (name :: UpdateRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateRealtimeLogConfig)
{-# DEPRECATED urlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcEndPoints :: Lens.Lens' UpdateRealtimeLogConfig (Lude.Maybe [EndPoint])
urlcEndPoints = Lens.lens (endPoints :: UpdateRealtimeLogConfig -> Lude.Maybe [EndPoint]) (\s a -> s {endPoints = a} :: UpdateRealtimeLogConfig)
{-# DEPRECATED urlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcFields :: Lens.Lens' UpdateRealtimeLogConfig (Lude.Maybe [Lude.Text])
urlcFields = Lens.lens (fields :: UpdateRealtimeLogConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {fields = a} :: UpdateRealtimeLogConfig)
{-# DEPRECATED urlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.AWSRequest UpdateRealtimeLogConfig where
  type Rs UpdateRealtimeLogConfig = UpdateRealtimeLogConfigResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateRealtimeLogConfigResponse'
            Lude.<$> (x Lude..@? "RealtimeLogConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateRealtimeLogConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}UpdateRealtimeLogConfigRequest"

instance Lude.ToHeaders UpdateRealtimeLogConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateRealtimeLogConfig where
  toPath = Lude.const "/2020-05-31/realtime-log-config/"

instance Lude.ToQuery UpdateRealtimeLogConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML UpdateRealtimeLogConfig where
  toXML UpdateRealtimeLogConfig' {..} =
    Lude.mconcat
      [ "ARN" Lude.@= arn,
        "SamplingRate" Lude.@= samplingRate,
        "Name" Lude.@= name,
        "EndPoints"
          Lude.@= Lude.toXML (Lude.toXMLList "member" Lude.<$> endPoints),
        "Fields"
          Lude.@= Lude.toXML (Lude.toXMLList "Field" Lude.<$> fields)
      ]

-- | /See:/ 'mkUpdateRealtimeLogConfigResponse' smart constructor.
data UpdateRealtimeLogConfigResponse = UpdateRealtimeLogConfigResponse'
  { realtimeLogConfig ::
      Lude.Maybe
        RealtimeLogConfig,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- * 'realtimeLogConfig' - A real-time log configuration.
-- * 'responseStatus' - The response status code.
mkUpdateRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRealtimeLogConfigResponse
mkUpdateRealtimeLogConfigResponse pResponseStatus_ =
  UpdateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcrsRealtimeLogConfig :: Lens.Lens' UpdateRealtimeLogConfigResponse (Lude.Maybe RealtimeLogConfig)
urlcrsRealtimeLogConfig = Lens.lens (realtimeLogConfig :: UpdateRealtimeLogConfigResponse -> Lude.Maybe RealtimeLogConfig) (\s a -> s {realtimeLogConfig = a} :: UpdateRealtimeLogConfigResponse)
{-# DEPRECATED urlcrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcrsResponseStatus :: Lens.Lens' UpdateRealtimeLogConfigResponse Lude.Int
urlcrsResponseStatus = Lens.lens (responseStatus :: UpdateRealtimeLogConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRealtimeLogConfigResponse)
{-# DEPRECATED urlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

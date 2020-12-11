{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time log configuration.
--
-- After you create a real-time log configuration, you can attach it to one or more cache behaviors to send real-time log data to the specified Amazon Kinesis data stream.
-- For more information about real-time log configurations, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateRealtimeLogConfig
  ( -- * Creating a request
    CreateRealtimeLogConfig (..),
    mkCreateRealtimeLogConfig,

    -- ** Request lenses
    crlcEndPoints,
    crlcFields,
    crlcName,
    crlcSamplingRate,

    -- * Destructuring the response
    CreateRealtimeLogConfigResponse (..),
    mkCreateRealtimeLogConfigResponse,

    -- ** Response lenses
    crlcrsRealtimeLogConfig,
    crlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRealtimeLogConfig' smart constructor.
data CreateRealtimeLogConfig = CreateRealtimeLogConfig'
  { endPoints ::
      [EndPoint],
    fields :: [Lude.Text],
    name :: Lude.Text,
    samplingRate :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'endPoints' - Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
-- * 'fields' - A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
-- * 'name' - A unique name to identify this real-time log configuration.
-- * 'samplingRate' - The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
mkCreateRealtimeLogConfig ::
  -- | 'name'
  Lude.Text ->
  -- | 'samplingRate'
  Lude.Integer ->
  CreateRealtimeLogConfig
mkCreateRealtimeLogConfig pName_ pSamplingRate_ =
  CreateRealtimeLogConfig'
    { endPoints = Lude.mempty,
      fields = Lude.mempty,
      name = pName_,
      samplingRate = pSamplingRate_
    }

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcEndPoints :: Lens.Lens' CreateRealtimeLogConfig [EndPoint]
crlcEndPoints = Lens.lens (endPoints :: CreateRealtimeLogConfig -> [EndPoint]) (\s a -> s {endPoints = a} :: CreateRealtimeLogConfig)
{-# DEPRECATED crlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcFields :: Lens.Lens' CreateRealtimeLogConfig [Lude.Text]
crlcFields = Lens.lens (fields :: CreateRealtimeLogConfig -> [Lude.Text]) (\s a -> s {fields = a} :: CreateRealtimeLogConfig)
{-# DEPRECATED crlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | A unique name to identify this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcName :: Lens.Lens' CreateRealtimeLogConfig Lude.Text
crlcName = Lens.lens (name :: CreateRealtimeLogConfig -> Lude.Text) (\s a -> s {name = a} :: CreateRealtimeLogConfig)
{-# DEPRECATED crlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcSamplingRate :: Lens.Lens' CreateRealtimeLogConfig Lude.Integer
crlcSamplingRate = Lens.lens (samplingRate :: CreateRealtimeLogConfig -> Lude.Integer) (\s a -> s {samplingRate = a} :: CreateRealtimeLogConfig)
{-# DEPRECATED crlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

instance Lude.AWSRequest CreateRealtimeLogConfig where
  type Rs CreateRealtimeLogConfig = CreateRealtimeLogConfigResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateRealtimeLogConfigResponse'
            Lude.<$> (x Lude..@? "RealtimeLogConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateRealtimeLogConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CreateRealtimeLogConfigRequest"

instance Lude.ToHeaders CreateRealtimeLogConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateRealtimeLogConfig where
  toPath = Lude.const "/2020-05-31/realtime-log-config"

instance Lude.ToQuery CreateRealtimeLogConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateRealtimeLogConfig where
  toXML CreateRealtimeLogConfig' {..} =
    Lude.mconcat
      [ "EndPoints" Lude.@= Lude.toXMLList "member" endPoints,
        "Fields" Lude.@= Lude.toXMLList "Field" fields,
        "Name" Lude.@= name,
        "SamplingRate" Lude.@= samplingRate
      ]

-- | /See:/ 'mkCreateRealtimeLogConfigResponse' smart constructor.
data CreateRealtimeLogConfigResponse = CreateRealtimeLogConfigResponse'
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

-- | Creates a value of 'CreateRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- * 'realtimeLogConfig' - A real-time log configuration.
-- * 'responseStatus' - The response status code.
mkCreateRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRealtimeLogConfigResponse
mkCreateRealtimeLogConfigResponse pResponseStatus_ =
  CreateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcrsRealtimeLogConfig :: Lens.Lens' CreateRealtimeLogConfigResponse (Lude.Maybe RealtimeLogConfig)
crlcrsRealtimeLogConfig = Lens.lens (realtimeLogConfig :: CreateRealtimeLogConfigResponse -> Lude.Maybe RealtimeLogConfig) (\s a -> s {realtimeLogConfig = a} :: CreateRealtimeLogConfigResponse)
{-# DEPRECATED crlcrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcrsResponseStatus :: Lens.Lens' CreateRealtimeLogConfigResponse Lude.Int
crlcrsResponseStatus = Lens.lens (responseStatus :: CreateRealtimeLogConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRealtimeLogConfigResponse)
{-# DEPRECATED crlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

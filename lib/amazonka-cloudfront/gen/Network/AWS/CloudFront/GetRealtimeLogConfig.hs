{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a real-time log configuration.
--
-- To get a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to get.
module Network.AWS.CloudFront.GetRealtimeLogConfig
  ( -- * Creating a request
    GetRealtimeLogConfig (..),
    mkGetRealtimeLogConfig,

    -- ** Request lenses
    grlcARN,
    grlcName,

    -- * Destructuring the response
    GetRealtimeLogConfigResponse (..),
    mkGetRealtimeLogConfigResponse,

    -- ** Response lenses
    grlcrsRealtimeLogConfig,
    grlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRealtimeLogConfig' smart constructor.
data GetRealtimeLogConfig = GetRealtimeLogConfig'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the real-time log configuration to get.
-- * 'name' - The name of the real-time log configuration to get.
mkGetRealtimeLogConfig ::
  GetRealtimeLogConfig
mkGetRealtimeLogConfig =
  GetRealtimeLogConfig' {arn = Lude.Nothing, name = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to get.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcARN :: Lens.Lens' GetRealtimeLogConfig (Lude.Maybe Lude.Text)
grlcARN = Lens.lens (arn :: GetRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetRealtimeLogConfig)
{-# DEPRECATED grlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the real-time log configuration to get.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcName :: Lens.Lens' GetRealtimeLogConfig (Lude.Maybe Lude.Text)
grlcName = Lens.lens (name :: GetRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetRealtimeLogConfig)
{-# DEPRECATED grlcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetRealtimeLogConfig where
  type Rs GetRealtimeLogConfig = GetRealtimeLogConfigResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetRealtimeLogConfigResponse'
            Lude.<$> (x Lude..@? "RealtimeLogConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement GetRealtimeLogConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}GetRealtimeLogConfigRequest"

instance Lude.ToHeaders GetRealtimeLogConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetRealtimeLogConfig where
  toPath = Lude.const "/2020-05-31/get-realtime-log-config/"

instance Lude.ToQuery GetRealtimeLogConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML GetRealtimeLogConfig where
  toXML GetRealtimeLogConfig' {..} =
    Lude.mconcat ["ARN" Lude.@= arn, "Name" Lude.@= name]

-- | /See:/ 'mkGetRealtimeLogConfigResponse' smart constructor.
data GetRealtimeLogConfigResponse = GetRealtimeLogConfigResponse'
  { realtimeLogConfig ::
      Lude.Maybe RealtimeLogConfig,
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

-- | Creates a value of 'GetRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- * 'realtimeLogConfig' - A real-time log configuration.
-- * 'responseStatus' - The response status code.
mkGetRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRealtimeLogConfigResponse
mkGetRealtimeLogConfigResponse pResponseStatus_ =
  GetRealtimeLogConfigResponse'
    { realtimeLogConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcrsRealtimeLogConfig :: Lens.Lens' GetRealtimeLogConfigResponse (Lude.Maybe RealtimeLogConfig)
grlcrsRealtimeLogConfig = Lens.lens (realtimeLogConfig :: GetRealtimeLogConfigResponse -> Lude.Maybe RealtimeLogConfig) (\s a -> s {realtimeLogConfig = a} :: GetRealtimeLogConfigResponse)
{-# DEPRECATED grlcrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcrsResponseStatus :: Lens.Lens' GetRealtimeLogConfigResponse Lude.Int
grlcrsResponseStatus = Lens.lens (responseStatus :: GetRealtimeLogConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRealtimeLogConfigResponse)
{-# DEPRECATED grlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

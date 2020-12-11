{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetV2LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the fine grained logging options.
module Network.AWS.IoT.GetV2LoggingOptions
  ( -- * Creating a request
    GetV2LoggingOptions (..),
    mkGetV2LoggingOptions,

    -- * Destructuring the response
    GetV2LoggingOptionsResponse (..),
    mkGetV2LoggingOptionsResponse,

    -- ** Response lenses
    gvlorsDisableAllLogs,
    gvlorsDefaultLogLevel,
    gvlorsRoleARN,
    gvlorsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetV2LoggingOptions' smart constructor.
data GetV2LoggingOptions = GetV2LoggingOptions'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetV2LoggingOptions' with the minimum fields required to make a request.
mkGetV2LoggingOptions ::
  GetV2LoggingOptions
mkGetV2LoggingOptions = GetV2LoggingOptions'

instance Lude.AWSRequest GetV2LoggingOptions where
  type Rs GetV2LoggingOptions = GetV2LoggingOptionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetV2LoggingOptionsResponse'
            Lude.<$> (x Lude..?> "disableAllLogs")
            Lude.<*> (x Lude..?> "defaultLogLevel")
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetV2LoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetV2LoggingOptions where
  toPath = Lude.const "/v2LoggingOptions"

instance Lude.ToQuery GetV2LoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetV2LoggingOptionsResponse' smart constructor.
data GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse'
  { disableAllLogs ::
      Lude.Maybe Lude.Bool,
    defaultLogLevel ::
      Lude.Maybe LogLevel,
    roleARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetV2LoggingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'defaultLogLevel' - The default log level.
-- * 'disableAllLogs' - Disables all logs.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
mkGetV2LoggingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetV2LoggingOptionsResponse
mkGetV2LoggingOptionsResponse pResponseStatus_ =
  GetV2LoggingOptionsResponse'
    { disableAllLogs = Lude.Nothing,
      defaultLogLevel = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Disables all logs.
--
-- /Note:/ Consider using 'disableAllLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorsDisableAllLogs :: Lens.Lens' GetV2LoggingOptionsResponse (Lude.Maybe Lude.Bool)
gvlorsDisableAllLogs = Lens.lens (disableAllLogs :: GetV2LoggingOptionsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {disableAllLogs = a} :: GetV2LoggingOptionsResponse)
{-# DEPRECATED gvlorsDisableAllLogs "Use generic-lens or generic-optics with 'disableAllLogs' instead." #-}

-- | The default log level.
--
-- /Note:/ Consider using 'defaultLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorsDefaultLogLevel :: Lens.Lens' GetV2LoggingOptionsResponse (Lude.Maybe LogLevel)
gvlorsDefaultLogLevel = Lens.lens (defaultLogLevel :: GetV2LoggingOptionsResponse -> Lude.Maybe LogLevel) (\s a -> s {defaultLogLevel = a} :: GetV2LoggingOptionsResponse)
{-# DEPRECATED gvlorsDefaultLogLevel "Use generic-lens or generic-optics with 'defaultLogLevel' instead." #-}

-- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorsRoleARN :: Lens.Lens' GetV2LoggingOptionsResponse (Lude.Maybe Lude.Text)
gvlorsRoleARN = Lens.lens (roleARN :: GetV2LoggingOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GetV2LoggingOptionsResponse)
{-# DEPRECATED gvlorsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorsResponseStatus :: Lens.Lens' GetV2LoggingOptionsResponse Lude.Int
gvlorsResponseStatus = Lens.lens (responseStatus :: GetV2LoggingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetV2LoggingOptionsResponse)
{-# DEPRECATED gvlorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

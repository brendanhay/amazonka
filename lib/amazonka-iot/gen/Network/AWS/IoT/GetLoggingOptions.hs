{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the logging options.
--
-- NOTE: use of this command is not recommended. Use @GetV2LoggingOptions@ instead.
module Network.AWS.IoT.GetLoggingOptions
  ( -- * Creating a request
    GetLoggingOptions (..),
    mkGetLoggingOptions,

    -- * Destructuring the response
    GetLoggingOptionsResponse (..),
    mkGetLoggingOptionsResponse,

    -- ** Response lenses
    glorsLogLevel,
    glorsRoleARN,
    glorsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetLoggingOptions operation.
--
-- /See:/ 'mkGetLoggingOptions' smart constructor.
data GetLoggingOptions = GetLoggingOptions'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoggingOptions' with the minimum fields required to make a request.
mkGetLoggingOptions ::
  GetLoggingOptions
mkGetLoggingOptions = GetLoggingOptions'

instance Lude.AWSRequest GetLoggingOptions where
  type Rs GetLoggingOptions = GetLoggingOptionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoggingOptionsResponse'
            Lude.<$> (x Lude..?> "logLevel")
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLoggingOptions where
  toPath = Lude.const "/loggingOptions"

instance Lude.ToQuery GetLoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | The output from the GetLoggingOptions operation.
--
-- /See:/ 'mkGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { -- | The logging level.
    logLevel :: Lude.Maybe LogLevel,
    -- | The ARN of the IAM role that grants access.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoggingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'logLevel' - The logging level.
-- * 'roleARN' - The ARN of the IAM role that grants access.
-- * 'responseStatus' - The response status code.
mkGetLoggingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoggingOptionsResponse
mkGetLoggingOptionsResponse pResponseStatus_ =
  GetLoggingOptionsResponse'
    { logLevel = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The logging level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorsLogLevel :: Lens.Lens' GetLoggingOptionsResponse (Lude.Maybe LogLevel)
glorsLogLevel = Lens.lens (logLevel :: GetLoggingOptionsResponse -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: GetLoggingOptionsResponse)
{-# DEPRECATED glorsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorsRoleARN :: Lens.Lens' GetLoggingOptionsResponse (Lude.Maybe Lude.Text)
glorsRoleARN = Lens.lens (roleARN :: GetLoggingOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GetLoggingOptionsResponse)
{-# DEPRECATED glorsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorsResponseStatus :: Lens.Lens' GetLoggingOptionsResponse Lude.Int
glorsResponseStatus = Lens.lens (responseStatus :: GetLoggingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoggingOptionsResponse)
{-# DEPRECATED glorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current settings of the AWS IoT Analytics logging options.
module Network.AWS.IoTAnalytics.DescribeLoggingOptions
  ( -- * Creating a request
    DescribeLoggingOptions (..),
    mkDescribeLoggingOptions,

    -- * Destructuring the response
    DescribeLoggingOptionsResponse (..),
    mkDescribeLoggingOptionsResponse,

    -- ** Response lenses
    dlorsLoggingOptions,
    dlorsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoggingOptions' smart constructor.
data DescribeLoggingOptions = DescribeLoggingOptions'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoggingOptions' with the minimum fields required to make a request.
mkDescribeLoggingOptions ::
  DescribeLoggingOptions
mkDescribeLoggingOptions = DescribeLoggingOptions'

instance Lude.AWSRequest DescribeLoggingOptions where
  type Rs DescribeLoggingOptions = DescribeLoggingOptionsResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLoggingOptionsResponse'
            Lude.<$> (x Lude..?> "loggingOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoggingOptions where
  toPath = Lude.const "/logging"

instance Lude.ToQuery DescribeLoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLoggingOptionsResponse' smart constructor.
data DescribeLoggingOptionsResponse = DescribeLoggingOptionsResponse'
  { -- | The current settings of the AWS IoT Analytics logging options.
    loggingOptions :: Lude.Maybe LoggingOptions,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoggingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'loggingOptions' - The current settings of the AWS IoT Analytics logging options.
-- * 'responseStatus' - The response status code.
mkDescribeLoggingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoggingOptionsResponse
mkDescribeLoggingOptionsResponse pResponseStatus_ =
  DescribeLoggingOptionsResponse'
    { loggingOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current settings of the AWS IoT Analytics logging options.
--
-- /Note:/ Consider using 'loggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlorsLoggingOptions :: Lens.Lens' DescribeLoggingOptionsResponse (Lude.Maybe LoggingOptions)
dlorsLoggingOptions = Lens.lens (loggingOptions :: DescribeLoggingOptionsResponse -> Lude.Maybe LoggingOptions) (\s a -> s {loggingOptions = a} :: DescribeLoggingOptionsResponse)
{-# DEPRECATED dlorsLoggingOptions "Use generic-lens or generic-optics with 'loggingOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlorsResponseStatus :: Lens.Lens' DescribeLoggingOptionsResponse Lude.Int
dlorsResponseStatus = Lens.lens (responseStatus :: DescribeLoggingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoggingOptionsResponse)
{-# DEPRECATED dlorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

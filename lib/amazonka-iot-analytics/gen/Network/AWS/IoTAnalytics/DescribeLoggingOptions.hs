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
    dlorrsLoggingOptions,
    dlorrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoggingOptions' smart constructor.
data DescribeLoggingOptions = DescribeLoggingOptions'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoggingOptions' value with any optional fields omitted.
mkDescribeLoggingOptions ::
  DescribeLoggingOptions
mkDescribeLoggingOptions = DescribeLoggingOptions'

instance Core.AWSRequest DescribeLoggingOptions where
  type Rs DescribeLoggingOptions = DescribeLoggingOptionsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/logging",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoggingOptionsResponse'
            Core.<$> (x Core..:? "loggingOptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLoggingOptionsResponse' smart constructor.
data DescribeLoggingOptionsResponse = DescribeLoggingOptionsResponse'
  { -- | The current settings of the AWS IoT Analytics logging options.
    loggingOptions :: Core.Maybe Types.LoggingOptions,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoggingOptionsResponse' value with any optional fields omitted.
mkDescribeLoggingOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoggingOptionsResponse
mkDescribeLoggingOptionsResponse responseStatus =
  DescribeLoggingOptionsResponse'
    { loggingOptions = Core.Nothing,
      responseStatus
    }

-- | The current settings of the AWS IoT Analytics logging options.
--
-- /Note:/ Consider using 'loggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlorrsLoggingOptions :: Lens.Lens' DescribeLoggingOptionsResponse (Core.Maybe Types.LoggingOptions)
dlorrsLoggingOptions = Lens.field @"loggingOptions"
{-# DEPRECATED dlorrsLoggingOptions "Use generic-lens or generic-optics with 'loggingOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlorrsResponseStatus :: Lens.Lens' DescribeLoggingOptionsResponse Core.Int
dlorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

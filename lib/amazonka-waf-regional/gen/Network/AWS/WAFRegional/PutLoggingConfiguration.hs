{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.PutLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a 'LoggingConfiguration' with a specified web ACL.
--
-- You can access information about all traffic that AWS WAF inspects using the following steps:
--
--     * Create an Amazon Kinesis Data Firehose.
-- Create the data firehose with a PUT source and in the region that you are operating. However, if you are capturing logs for Amazon CloudFront, always create the firehose in US East (N. Virginia).
--
--
--     * Associate that firehose to your web ACL using a @PutLoggingConfiguration@ request.
--
--
-- When you successfully enable logging using a @PutLoggingConfiguration@ request, AWS WAF will create a service linked role with the necessary permissions to write logs to the Amazon Kinesis Data Firehose. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information> in the /AWS WAF Developer Guide/ .
module Network.AWS.WAFRegional.PutLoggingConfiguration
  ( -- * Creating a request
    PutLoggingConfiguration (..),
    mkPutLoggingConfiguration,

    -- ** Request lenses
    plcLoggingConfiguration,

    -- * Destructuring the response
    PutLoggingConfigurationResponse (..),
    mkPutLoggingConfigurationResponse,

    -- ** Response lenses
    plcrrsLoggingConfiguration,
    plcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkPutLoggingConfiguration' smart constructor.
newtype PutLoggingConfiguration = PutLoggingConfiguration'
  { -- | The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
    loggingConfiguration :: Types.LoggingConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutLoggingConfiguration' value with any optional fields omitted.
mkPutLoggingConfiguration ::
  -- | 'loggingConfiguration'
  Types.LoggingConfiguration ->
  PutLoggingConfiguration
mkPutLoggingConfiguration loggingConfiguration =
  PutLoggingConfiguration' {loggingConfiguration}

-- | The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcLoggingConfiguration :: Lens.Lens' PutLoggingConfiguration Types.LoggingConfiguration
plcLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# DEPRECATED plcLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

instance Core.FromJSON PutLoggingConfiguration where
  toJSON PutLoggingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("LoggingConfiguration" Core..= loggingConfiguration)]
      )

instance Core.AWSRequest PutLoggingConfiguration where
  type Rs PutLoggingConfiguration = PutLoggingConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSWAF_Regional_20161128.PutLoggingConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLoggingConfigurationResponse'
            Core.<$> (x Core..:? "LoggingConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutLoggingConfigurationResponse' smart constructor.
data PutLoggingConfigurationResponse = PutLoggingConfigurationResponse'
  { -- | The 'LoggingConfiguration' that you submitted in the request.
    loggingConfiguration :: Core.Maybe Types.LoggingConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLoggingConfigurationResponse' value with any optional fields omitted.
mkPutLoggingConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutLoggingConfigurationResponse
mkPutLoggingConfigurationResponse responseStatus =
  PutLoggingConfigurationResponse'
    { loggingConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | The 'LoggingConfiguration' that you submitted in the request.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcrrsLoggingConfiguration :: Lens.Lens' PutLoggingConfigurationResponse (Core.Maybe Types.LoggingConfiguration)
plcrrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# DEPRECATED plcrrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcrrsResponseStatus :: Lens.Lens' PutLoggingConfigurationResponse Core.Int
plcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED plcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

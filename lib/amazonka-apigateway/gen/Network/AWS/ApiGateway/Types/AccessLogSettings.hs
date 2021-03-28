{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.AccessLogSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.AccessLogSettings
  ( AccessLogSettings (..)
  -- * Smart constructor
  , mkAccessLogSettings
  -- * Lenses
  , alsDestinationArn
  , alsFormat
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Access log settings, including the access log format and access log destination ARN.
--
-- /See:/ 'mkAccessLogSettings' smart constructor.
data AccessLogSettings = AccessLogSettings'
  { destinationArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
  , format :: Core.Maybe Core.Text
    -- ^ A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessLogSettings' value with any optional fields omitted.
mkAccessLogSettings
    :: AccessLogSettings
mkAccessLogSettings
  = AccessLogSettings'{destinationArn = Core.Nothing,
                       format = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
--
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsDestinationArn :: Lens.Lens' AccessLogSettings (Core.Maybe Core.Text)
alsDestinationArn = Lens.field @"destinationArn"
{-# INLINEABLE alsDestinationArn #-}
{-# DEPRECATED destinationArn "Use generic-lens or generic-optics with 'destinationArn' instead"  #-}

-- | A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsFormat :: Lens.Lens' AccessLogSettings (Core.Maybe Core.Text)
alsFormat = Lens.field @"format"
{-# INLINEABLE alsFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

instance Core.FromJSON AccessLogSettings where
        parseJSON
          = Core.withObject "AccessLogSettings" Core.$
              \ x ->
                AccessLogSettings' Core.<$>
                  (x Core..:? "destinationArn") Core.<*> x Core..:? "format"

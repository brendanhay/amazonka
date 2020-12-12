{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AccessLogSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AccessLogSettings
  ( AccessLogSettings (..),

    -- * Smart constructor
    mkAccessLogSettings,

    -- * Lenses
    alsFormat,
    alsDestinationARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Access log settings, including the access log format and access log destination ARN.
--
-- /See:/ 'mkAccessLogSettings' smart constructor.
data AccessLogSettings = AccessLogSettings'
  { format ::
      Lude.Maybe Lude.Text,
    destinationARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessLogSettings' with the minimum fields required to make a request.
--
-- * 'destinationARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
-- * 'format' - A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
mkAccessLogSettings ::
  AccessLogSettings
mkAccessLogSettings =
  AccessLogSettings'
    { format = Lude.Nothing,
      destinationARN = Lude.Nothing
    }

-- | A single line format of the access logs of data, as specified by selected <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#context-variable-reference > context variables> . The format must include at least @> context.requestId@ .
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsFormat :: Lens.Lens' AccessLogSettings (Lude.Maybe Lude.Text)
alsFormat = Lens.lens (format :: AccessLogSettings -> Lude.Maybe Lude.Text) (\s a -> s {format = a} :: AccessLogSettings)
{-# DEPRECATED alsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or Kinesis Data Firehose delivery stream to receive access logs. If you specify a Kinesis Data Firehose delivery stream, the stream name must begin with @amazon-apigateway-@ .
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alsDestinationARN :: Lens.Lens' AccessLogSettings (Lude.Maybe Lude.Text)
alsDestinationARN = Lens.lens (destinationARN :: AccessLogSettings -> Lude.Maybe Lude.Text) (\s a -> s {destinationARN = a} :: AccessLogSettings)
{-# DEPRECATED alsDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

instance Lude.FromJSON AccessLogSettings where
  parseJSON =
    Lude.withObject
      "AccessLogSettings"
      ( \x ->
          AccessLogSettings'
            Lude.<$> (x Lude..:? "format") Lude.<*> (x Lude..:? "destinationArn")
      )

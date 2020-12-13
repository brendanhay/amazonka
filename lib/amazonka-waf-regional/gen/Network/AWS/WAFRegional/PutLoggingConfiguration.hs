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
    plcrsLoggingConfiguration,
    plcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkPutLoggingConfiguration' smart constructor.
newtype PutLoggingConfiguration = PutLoggingConfiguration'
  { -- | The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
    loggingConfiguration :: LoggingConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLoggingConfiguration' with the minimum fields required to make a request.
--
-- * 'loggingConfiguration' - The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
mkPutLoggingConfiguration ::
  -- | 'loggingConfiguration'
  LoggingConfiguration ->
  PutLoggingConfiguration
mkPutLoggingConfiguration pLoggingConfiguration_ =
  PutLoggingConfiguration'
    { loggingConfiguration =
        pLoggingConfiguration_
    }

-- | The Amazon Kinesis Data Firehose that contains the inspected traffic information, the redacted fields details, and the Amazon Resource Name (ARN) of the web ACL to monitor.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcLoggingConfiguration :: Lens.Lens' PutLoggingConfiguration LoggingConfiguration
plcLoggingConfiguration = Lens.lens (loggingConfiguration :: PutLoggingConfiguration -> LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: PutLoggingConfiguration)
{-# DEPRECATED plcLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

instance Lude.AWSRequest PutLoggingConfiguration where
  type Rs PutLoggingConfiguration = PutLoggingConfigurationResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutLoggingConfigurationResponse'
            Lude.<$> (x Lude..?> "LoggingConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLoggingConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.PutLoggingConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutLoggingConfiguration where
  toJSON PutLoggingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("LoggingConfiguration" Lude..= loggingConfiguration)]
      )

instance Lude.ToPath PutLoggingConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLoggingConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLoggingConfigurationResponse' smart constructor.
data PutLoggingConfigurationResponse = PutLoggingConfigurationResponse'
  { -- | The 'LoggingConfiguration' that you submitted in the request.
    loggingConfiguration :: Lude.Maybe LoggingConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLoggingConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'loggingConfiguration' - The 'LoggingConfiguration' that you submitted in the request.
-- * 'responseStatus' - The response status code.
mkPutLoggingConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLoggingConfigurationResponse
mkPutLoggingConfigurationResponse pResponseStatus_ =
  PutLoggingConfigurationResponse'
    { loggingConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'LoggingConfiguration' that you submitted in the request.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcrsLoggingConfiguration :: Lens.Lens' PutLoggingConfigurationResponse (Lude.Maybe LoggingConfiguration)
plcrsLoggingConfiguration = Lens.lens (loggingConfiguration :: PutLoggingConfigurationResponse -> Lude.Maybe LoggingConfiguration) (\s a -> s {loggingConfiguration = a} :: PutLoggingConfigurationResponse)
{-# DEPRECATED plcrsLoggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcrsResponseStatus :: Lens.Lens' PutLoggingConfigurationResponse Lude.Int
plcrsResponseStatus = Lens.lens (responseStatus :: PutLoggingConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLoggingConfigurationResponse)
{-# DEPRECATED plcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.UpdateGatewayInformation
  ( -- * Creating a request
    UpdateGatewayInformation (..),
    mkUpdateGatewayInformation,

    -- ** Request lenses
    ugiGatewayARN,
    ugiGatewayName,
    ugiGatewayTimezone,
    ugiCloudWatchLogGroupARN,

    -- * Destructuring the response
    UpdateGatewayInformationResponse (..),
    mkUpdateGatewayInformationResponse,

    -- ** Response lenses
    ugirsGatewayARN,
    ugirsGatewayName,
    ugirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { gatewayARN :: Lude.Text,
    gatewayName :: Lude.Maybe Lude.Text,
    -- | A value that indicates the time zone of the gateway.
    gatewayTimezone :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
    cloudWatchLogGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewayInformation' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'gatewayName' -
-- * 'gatewayTimezone' - A value that indicates the time zone of the gateway.
-- * 'cloudWatchLogGroupARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
mkUpdateGatewayInformation ::
  -- | 'gatewayARN'
  Lude.Text ->
  UpdateGatewayInformation
mkUpdateGatewayInformation pGatewayARN_ =
  UpdateGatewayInformation'
    { gatewayARN = pGatewayARN_,
      gatewayName = Lude.Nothing,
      gatewayTimezone = Lude.Nothing,
      cloudWatchLogGroupARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayARN :: Lens.Lens' UpdateGatewayInformation Lude.Text
ugiGatewayARN = Lens.lens (gatewayARN :: UpdateGatewayInformation -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateGatewayInformation)
{-# DEPRECATED ugiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayName :: Lens.Lens' UpdateGatewayInformation (Lude.Maybe Lude.Text)
ugiGatewayName = Lens.lens (gatewayName :: UpdateGatewayInformation -> Lude.Maybe Lude.Text) (\s a -> s {gatewayName = a} :: UpdateGatewayInformation)
{-# DEPRECATED ugiGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiGatewayTimezone :: Lens.Lens' UpdateGatewayInformation (Lude.Maybe Lude.Text)
ugiGatewayTimezone = Lens.lens (gatewayTimezone :: UpdateGatewayInformation -> Lude.Maybe Lude.Text) (\s a -> s {gatewayTimezone = a} :: UpdateGatewayInformation)
{-# DEPRECATED ugiGatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that you want to use to monitor and log events in the gateway.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/WhatIsCloudWatchLogs.html What is Amazon CloudWatch Logs?>
--
-- /Note:/ Consider using 'cloudWatchLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugiCloudWatchLogGroupARN :: Lens.Lens' UpdateGatewayInformation (Lude.Maybe Lude.Text)
ugiCloudWatchLogGroupARN = Lens.lens (cloudWatchLogGroupARN :: UpdateGatewayInformation -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchLogGroupARN = a} :: UpdateGatewayInformation)
{-# DEPRECATED ugiCloudWatchLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogGroupARN' instead." #-}

instance Lude.AWSRequest UpdateGatewayInformation where
  type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGatewayInformationResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "GatewayName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGatewayInformation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateGatewayInformation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGatewayInformation where
  toJSON UpdateGatewayInformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            ("GatewayName" Lude..=) Lude.<$> gatewayName,
            ("GatewayTimezone" Lude..=) Lude.<$> gatewayTimezone,
            ("CloudWatchLogGroupARN" Lude..=) Lude.<$> cloudWatchLogGroupARN
          ]
      )

instance Lude.ToPath UpdateGatewayInformation where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGatewayInformation where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was updated.
--
-- /See:/ 'mkUpdateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The name you configured for your gateway.
    gatewayName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewayInformationResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'gatewayName' - The name you configured for your gateway.
-- * 'responseStatus' - The response status code.
mkUpdateGatewayInformationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGatewayInformationResponse
mkUpdateGatewayInformationResponse pResponseStatus_ =
  UpdateGatewayInformationResponse'
    { gatewayARN = Lude.Nothing,
      gatewayName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirsGatewayARN :: Lens.Lens' UpdateGatewayInformationResponse (Lude.Maybe Lude.Text)
ugirsGatewayARN = Lens.lens (gatewayARN :: UpdateGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateGatewayInformationResponse)
{-# DEPRECATED ugirsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirsGatewayName :: Lens.Lens' UpdateGatewayInformationResponse (Lude.Maybe Lude.Text)
ugirsGatewayName = Lens.lens (gatewayName :: UpdateGatewayInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayName = a} :: UpdateGatewayInformationResponse)
{-# DEPRECATED ugirsGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugirsResponseStatus :: Lens.Lens' UpdateGatewayInformationResponse Lude.Int
ugirsResponseStatus = Lens.lens (responseStatus :: UpdateGatewayInformationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGatewayInformationResponse)
{-# DEPRECATED ugirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

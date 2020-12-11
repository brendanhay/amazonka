{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the gateway you previously deployed on your host. In the activation process, you specify information such as the AWS Region that you want to use for storing snapshots or tapes, the time zone for scheduled snapshots the gateway snapshot schedule window, an activation key, and a name for your gateway. The activation process also associates your gateway with your account. For more information, see 'UpdateGatewayInformation' .
module Network.AWS.StorageGateway.ActivateGateway
  ( -- * Creating a request
    ActivateGateway (..),
    mkActivateGateway,

    -- ** Request lenses
    agMediumChangerType,
    agTapeDriveType,
    agGatewayType,
    agTags,
    agActivationKey,
    agGatewayName,
    agGatewayTimezone,
    agGatewayRegion,

    -- * Destructuring the response
    ActivateGatewayResponse (..),
    mkActivateGatewayResponse,

    -- ** Response lenses
    agrsGatewayARN,
    agrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'ActivateGatewayInput$ActivationKey'
--
--
--     * 'ActivateGatewayInput$GatewayName'
--
--
--     * 'ActivateGatewayInput$GatewayRegion'
--
--
--     * 'ActivateGatewayInput$GatewayTimezone'
--
--
--     * 'ActivateGatewayInput$GatewayType'
--
--
--     * 'ActivateGatewayInput$MediumChangerType'
--
--
--     * 'ActivateGatewayInput$TapeDriveType'
--
--
--
-- /See:/ 'mkActivateGateway' smart constructor.
data ActivateGateway = ActivateGateway'
  { mediumChangerType ::
      Lude.Maybe Lude.Text,
    tapeDriveType :: Lude.Maybe Lude.Text,
    gatewayType :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    activationKey :: Lude.Text,
    gatewayName :: Lude.Text,
    gatewayTimezone :: Lude.Text,
    gatewayRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivateGateway' with the minimum fields required to make a request.
--
-- * 'activationKey' - Your gateway activation key. You can obtain the activation key by sending an HTTP GET request with redirects enabled to the gateway IP address (port 80). The redirect URL returned in the response provides you the activation key for your gateway in the query string parameter @activationKey@ . It may also include other activation-related parameters, however, these are merely defaults -- the arguments you pass to the @ActivateGateway@ API call determine the actual configuration of your gateway.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key> in the /AWS Storage Gateway User Guide/ .
-- * 'gatewayName' - The name you configured for your gateway.
-- * 'gatewayRegion' - A value that indicates the AWS Region where you want to store your data. The gateway AWS Region specified must be the same AWS Region as the AWS Region in your @Host@ header in the request. For more information about available AWS Regions and endpoints for AWS Storage Gateway, see <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
--
-- Valid Values: See <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
-- * 'gatewayTimezone' - A value that indicates the time zone you want to set for the gateway. The time zone is of the format "GMT-hr:mm" or "GMT+hr:mm". For example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00 indicates the time is 2 hours ahead of GMT. The time zone is used, for example, for scheduling snapshots and your gateway's maintenance schedule.
-- * 'gatewayType' - A value that defines the type of gateway to activate. The type specified is critical to all later functions of the gateway and cannot be changed after activation. The default value is @CACHED@ .
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@
-- * 'mediumChangerType' - The value that indicates the type of medium changer to use for tape gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
-- * 'tags' - A list of up to 50 tags that you can assign to the gateway. Each tag is a key-value pair.
-- * 'tapeDriveType' - The value that indicates the type of tape drive to use for tape gateway. This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
mkActivateGateway ::
  -- | 'activationKey'
  Lude.Text ->
  -- | 'gatewayName'
  Lude.Text ->
  -- | 'gatewayTimezone'
  Lude.Text ->
  -- | 'gatewayRegion'
  Lude.Text ->
  ActivateGateway
mkActivateGateway
  pActivationKey_
  pGatewayName_
  pGatewayTimezone_
  pGatewayRegion_ =
    ActivateGateway'
      { mediumChangerType = Lude.Nothing,
        tapeDriveType = Lude.Nothing,
        gatewayType = Lude.Nothing,
        tags = Lude.Nothing,
        activationKey = pActivationKey_,
        gatewayName = pGatewayName_,
        gatewayTimezone = pGatewayTimezone_,
        gatewayRegion = pGatewayRegion_
      }

-- | The value that indicates the type of medium changer to use for tape gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@
--
-- /Note:/ Consider using 'mediumChangerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agMediumChangerType :: Lens.Lens' ActivateGateway (Lude.Maybe Lude.Text)
agMediumChangerType = Lens.lens (mediumChangerType :: ActivateGateway -> Lude.Maybe Lude.Text) (\s a -> s {mediumChangerType = a} :: ActivateGateway)
{-# DEPRECATED agMediumChangerType "Use generic-lens or generic-optics with 'mediumChangerType' instead." #-}

-- | The value that indicates the type of tape drive to use for tape gateway. This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@
--
-- /Note:/ Consider using 'tapeDriveType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agTapeDriveType :: Lens.Lens' ActivateGateway (Lude.Maybe Lude.Text)
agTapeDriveType = Lens.lens (tapeDriveType :: ActivateGateway -> Lude.Maybe Lude.Text) (\s a -> s {tapeDriveType = a} :: ActivateGateway)
{-# DEPRECATED agTapeDriveType "Use generic-lens or generic-optics with 'tapeDriveType' instead." #-}

-- | A value that defines the type of gateway to activate. The type specified is critical to all later functions of the gateway and cannot be changed after activation. The default value is @CACHED@ .
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayType :: Lens.Lens' ActivateGateway (Lude.Maybe Lude.Text)
agGatewayType = Lens.lens (gatewayType :: ActivateGateway -> Lude.Maybe Lude.Text) (\s a -> s {gatewayType = a} :: ActivateGateway)
{-# DEPRECATED agGatewayType "Use generic-lens or generic-optics with 'gatewayType' instead." #-}

-- | A list of up to 50 tags that you can assign to the gateway. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agTags :: Lens.Lens' ActivateGateway (Lude.Maybe [Tag])
agTags = Lens.lens (tags :: ActivateGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ActivateGateway)
{-# DEPRECATED agTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Your gateway activation key. You can obtain the activation key by sending an HTTP GET request with redirects enabled to the gateway IP address (port 80). The redirect URL returned in the response provides you the activation key for your gateway in the query string parameter @activationKey@ . It may also include other activation-related parameters, however, these are merely defaults -- the arguments you pass to the @ActivateGateway@ API call determine the actual configuration of your gateway.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key> in the /AWS Storage Gateway User Guide/ .
--
-- /Note:/ Consider using 'activationKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agActivationKey :: Lens.Lens' ActivateGateway Lude.Text
agActivationKey = Lens.lens (activationKey :: ActivateGateway -> Lude.Text) (\s a -> s {activationKey = a} :: ActivateGateway)
{-# DEPRECATED agActivationKey "Use generic-lens or generic-optics with 'activationKey' instead." #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayName :: Lens.Lens' ActivateGateway Lude.Text
agGatewayName = Lens.lens (gatewayName :: ActivateGateway -> Lude.Text) (\s a -> s {gatewayName = a} :: ActivateGateway)
{-# DEPRECATED agGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | A value that indicates the time zone you want to set for the gateway. The time zone is of the format "GMT-hr:mm" or "GMT+hr:mm". For example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00 indicates the time is 2 hours ahead of GMT. The time zone is used, for example, for scheduling snapshots and your gateway's maintenance schedule.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayTimezone :: Lens.Lens' ActivateGateway Lude.Text
agGatewayTimezone = Lens.lens (gatewayTimezone :: ActivateGateway -> Lude.Text) (\s a -> s {gatewayTimezone = a} :: ActivateGateway)
{-# DEPRECATED agGatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead." #-}

-- | A value that indicates the AWS Region where you want to store your data. The gateway AWS Region specified must be the same AWS Region as the AWS Region in your @Host@ header in the request. For more information about available AWS Regions and endpoints for AWS Storage Gateway, see <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
--
-- Valid Values: See <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'gatewayRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayRegion :: Lens.Lens' ActivateGateway Lude.Text
agGatewayRegion = Lens.lens (gatewayRegion :: ActivateGateway -> Lude.Text) (\s a -> s {gatewayRegion = a} :: ActivateGateway)
{-# DEPRECATED agGatewayRegion "Use generic-lens or generic-optics with 'gatewayRegion' instead." #-}

instance Lude.AWSRequest ActivateGateway where
  type Rs ActivateGateway = ActivateGatewayResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ActivateGatewayResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ActivateGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ActivateGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ActivateGateway where
  toJSON ActivateGateway' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MediumChangerType" Lude..=) Lude.<$> mediumChangerType,
            ("TapeDriveType" Lude..=) Lude.<$> tapeDriveType,
            ("GatewayType" Lude..=) Lude.<$> gatewayType,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ActivationKey" Lude..= activationKey),
            Lude.Just ("GatewayName" Lude..= gatewayName),
            Lude.Just ("GatewayTimezone" Lude..= gatewayTimezone),
            Lude.Just ("GatewayRegion" Lude..= gatewayRegion)
          ]
      )

instance Lude.ToPath ActivateGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery ActivateGateway where
  toQuery = Lude.const Lude.mempty

-- | AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated gateway. It is a string made of information such as your account, gateway name, and AWS Region. This ARN is used to reference the gateway in other API operations as well as resource-based authorization.
--
-- /See:/ 'mkActivateGatewayResponse' smart constructor.
data ActivateGatewayResponse = ActivateGatewayResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ActivateGatewayResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkActivateGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ActivateGatewayResponse
mkActivateGatewayResponse pResponseStatus_ =
  ActivateGatewayResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrsGatewayARN :: Lens.Lens' ActivateGatewayResponse (Lude.Maybe Lude.Text)
agrsGatewayARN = Lens.lens (gatewayARN :: ActivateGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ActivateGatewayResponse)
{-# DEPRECATED agrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrsResponseStatus :: Lens.Lens' ActivateGatewayResponse Lude.Int
agrsResponseStatus = Lens.lens (responseStatus :: ActivateGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ActivateGatewayResponse)
{-# DEPRECATED agrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

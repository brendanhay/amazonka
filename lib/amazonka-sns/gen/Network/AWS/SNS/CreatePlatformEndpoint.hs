{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a device and mobile app on one of the supported push notification services, such as GCM (Firebase Cloud Messaging) and APNS. @CreatePlatformEndpoint@ requires the @PlatformApplicationArn@ that is returned from @CreatePlatformApplication@ . You can use the returned @EndpointArn@ to send a message to a mobile app or by the @Subscribe@ action for subscription to a topic. The @CreatePlatformEndpoint@ action is idempotent, so if the requester already owns an endpoint with the same device token and attributes, that endpoint's ARN is returned without creating a new endpoint. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
-- When using @CreatePlatformEndpoint@ with Baidu, two attributes must be provided: ChannelId and UserId. The token field must also contain the ChannelId. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePushBaiduEndpoint.html Creating an Amazon SNS Endpoint for Baidu> .
module Network.AWS.SNS.CreatePlatformEndpoint
  ( -- * Creating a request
    CreatePlatformEndpoint (..),
    mkCreatePlatformEndpoint,

    -- ** Request lenses
    cpeCustomUserData,
    cpeAttributes,
    cpePlatformApplicationARN,
    cpeToken,

    -- * Destructuring the response
    CreatePlatformEndpointResponse (..),
    mkCreatePlatformEndpointResponse,

    -- ** Response lenses
    cpersEndpointARN,
    cpersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for CreatePlatformEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpoint' smart constructor.
data CreatePlatformEndpoint = CreatePlatformEndpoint'
  { customUserData ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    platformApplicationARN :: Lude.Text,
    token :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlatformEndpoint' with the minimum fields required to make a request.
--
-- * 'attributes' - For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
-- * 'customUserData' - Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
-- * 'platformApplicationARN' - PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
-- * 'token' - Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
mkCreatePlatformEndpoint ::
  -- | 'platformApplicationARN'
  Lude.Text ->
  -- | 'token'
  Lude.Text ->
  CreatePlatformEndpoint
mkCreatePlatformEndpoint pPlatformApplicationARN_ pToken_ =
  CreatePlatformEndpoint'
    { customUserData = Lude.Nothing,
      attributes = Lude.Nothing,
      platformApplicationARN = pPlatformApplicationARN_,
      token = pToken_
    }

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
--
-- /Note:/ Consider using 'customUserData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeCustomUserData :: Lens.Lens' CreatePlatformEndpoint (Lude.Maybe Lude.Text)
cpeCustomUserData = Lens.lens (customUserData :: CreatePlatformEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {customUserData = a} :: CreatePlatformEndpoint)
{-# DEPRECATED cpeCustomUserData "Use generic-lens or generic-optics with 'customUserData' instead." #-}

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeAttributes :: Lens.Lens' CreatePlatformEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cpeAttributes = Lens.lens (attributes :: CreatePlatformEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: CreatePlatformEndpoint)
{-# DEPRECATED cpeAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpePlatformApplicationARN :: Lens.Lens' CreatePlatformEndpoint Lude.Text
cpePlatformApplicationARN = Lens.lens (platformApplicationARN :: CreatePlatformEndpoint -> Lude.Text) (\s a -> s {platformApplicationARN = a} :: CreatePlatformEndpoint)
{-# DEPRECATED cpePlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

-- | Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeToken :: Lens.Lens' CreatePlatformEndpoint Lude.Text
cpeToken = Lens.lens (token :: CreatePlatformEndpoint -> Lude.Text) (\s a -> s {token = a} :: CreatePlatformEndpoint)
{-# DEPRECATED cpeToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Lude.AWSRequest CreatePlatformEndpoint where
  type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "CreatePlatformEndpointResult"
      ( \s h x ->
          CreatePlatformEndpointResponse'
            Lude.<$> (x Lude..@? "EndpointArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlatformEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePlatformEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlatformEndpoint where
  toQuery CreatePlatformEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePlatformEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "CustomUserData" Lude.=: customUserData,
        "Attributes"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> attributes),
        "PlatformApplicationArn" Lude.=: platformApplicationARN,
        "Token" Lude.=: token
      ]

-- | Response from CreateEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpointResponse' smart constructor.
data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse'
  { endpointARN ::
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

-- | Creates a value of 'CreatePlatformEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointARN' - EndpointArn returned from CreateEndpoint action.
-- * 'responseStatus' - The response status code.
mkCreatePlatformEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlatformEndpointResponse
mkCreatePlatformEndpointResponse pResponseStatus_ =
  CreatePlatformEndpointResponse'
    { endpointARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | EndpointArn returned from CreateEndpoint action.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpersEndpointARN :: Lens.Lens' CreatePlatformEndpointResponse (Lude.Maybe Lude.Text)
cpersEndpointARN = Lens.lens (endpointARN :: CreatePlatformEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: CreatePlatformEndpointResponse)
{-# DEPRECATED cpersEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpersResponseStatus :: Lens.Lens' CreatePlatformEndpointResponse Lude.Int
cpersResponseStatus = Lens.lens (responseStatus :: CreatePlatformEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlatformEndpointResponse)
{-# DEPRECATED cpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

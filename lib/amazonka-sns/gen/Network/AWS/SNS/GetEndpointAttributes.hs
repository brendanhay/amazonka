{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the endpoint attributes for a device on one of the supported push notification services, such as GCM (Firebase Cloud Messaging) and APNS. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
module Network.AWS.SNS.GetEndpointAttributes
  ( -- * Creating a request
    GetEndpointAttributes (..),
    mkGetEndpointAttributes,

    -- ** Request lenses
    geaEndpointARN,

    -- * Destructuring the response
    GetEndpointAttributesResponse (..),
    mkGetEndpointAttributesResponse,

    -- ** Response lenses
    gearsAttributes,
    gearsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for GetEndpointAttributes action.
--
-- /See:/ 'mkGetEndpointAttributes' smart constructor.
newtype GetEndpointAttributes = GetEndpointAttributes'
  { endpointARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEndpointAttributes' with the minimum fields required to make a request.
--
-- * 'endpointARN' - EndpointArn for GetEndpointAttributes input.
mkGetEndpointAttributes ::
  -- | 'endpointARN'
  Lude.Text ->
  GetEndpointAttributes
mkGetEndpointAttributes pEndpointARN_ =
  GetEndpointAttributes' {endpointARN = pEndpointARN_}

-- | EndpointArn for GetEndpointAttributes input.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geaEndpointARN :: Lens.Lens' GetEndpointAttributes Lude.Text
geaEndpointARN = Lens.lens (endpointARN :: GetEndpointAttributes -> Lude.Text) (\s a -> s {endpointARN = a} :: GetEndpointAttributes)
{-# DEPRECATED geaEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest GetEndpointAttributes where
  type Rs GetEndpointAttributes = GetEndpointAttributesResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "GetEndpointAttributesResult"
      ( \s h x ->
          GetEndpointAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEndpointAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetEndpointAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetEndpointAttributes where
  toQuery GetEndpointAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetEndpointAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "EndpointArn" Lude.=: endpointARN
      ]

-- | Response from GetEndpointAttributes of the EndpointArn.
--
-- /See:/ 'mkGetEndpointAttributesResponse' smart constructor.
data GetEndpointAttributesResponse = GetEndpointAttributesResponse'
  { attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
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

-- | Creates a value of 'GetEndpointAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Attributes include the following:
--
--
--     * @CustomUserData@ – arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
--
--
--     * @Enabled@ – flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.
--
--
--     * @Token@ – device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
--
--
-- * 'responseStatus' - The response status code.
mkGetEndpointAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEndpointAttributesResponse
mkGetEndpointAttributesResponse pResponseStatus_ =
  GetEndpointAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Attributes include the following:
--
--
--     * @CustomUserData@ – arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
--
--
--     * @Enabled@ – flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.
--
--
--     * @Token@ – device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gearsAttributes :: Lens.Lens' GetEndpointAttributesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gearsAttributes = Lens.lens (attributes :: GetEndpointAttributesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: GetEndpointAttributesResponse)
{-# DEPRECATED gearsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gearsResponseStatus :: Lens.Lens' GetEndpointAttributesResponse Lude.Int
gearsResponseStatus = Lens.lens (responseStatus :: GetEndpointAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEndpointAttributesResponse)
{-# DEPRECATED gearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

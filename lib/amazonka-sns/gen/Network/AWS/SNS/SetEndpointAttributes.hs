{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes for an endpoint for a device on one of the supported push notification services, such as GCM (Firebase Cloud Messaging) and APNS. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
module Network.AWS.SNS.SetEndpointAttributes
  ( -- * Creating a request
    SetEndpointAttributes (..),
    mkSetEndpointAttributes,

    -- ** Request lenses
    seaAttributes,
    seaEndpointARN,

    -- * Destructuring the response
    SetEndpointAttributesResponse (..),
    mkSetEndpointAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for SetEndpointAttributes action.
--
-- /See:/ 'mkSetEndpointAttributes' smart constructor.
data SetEndpointAttributes = SetEndpointAttributes'
  { -- | A map of the endpoint attributes. Attributes in this map include the following:
    --
    --
    --     * @CustomUserData@ – arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
    --
    --
    --     * @Enabled@ – flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.
    --
    --
    --     * @Token@ – device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
    attributes :: Lude.HashMap Lude.Text (Lude.Text),
    -- | EndpointArn used for SetEndpointAttributes action.
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetEndpointAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of the endpoint attributes. Attributes in this map include the following:
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
-- * 'endpointARN' - EndpointArn used for SetEndpointAttributes action.
mkSetEndpointAttributes ::
  -- | 'endpointARN'
  Lude.Text ->
  SetEndpointAttributes
mkSetEndpointAttributes pEndpointARN_ =
  SetEndpointAttributes'
    { attributes = Lude.mempty,
      endpointARN = pEndpointARN_
    }

-- | A map of the endpoint attributes. Attributes in this map include the following:
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
seaAttributes :: Lens.Lens' SetEndpointAttributes (Lude.HashMap Lude.Text (Lude.Text))
seaAttributes = Lens.lens (attributes :: SetEndpointAttributes -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {attributes = a} :: SetEndpointAttributes)
{-# DEPRECATED seaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | EndpointArn used for SetEndpointAttributes action.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seaEndpointARN :: Lens.Lens' SetEndpointAttributes Lude.Text
seaEndpointARN = Lens.lens (endpointARN :: SetEndpointAttributes -> Lude.Text) (\s a -> s {endpointARN = a} :: SetEndpointAttributes)
{-# DEPRECATED seaEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest SetEndpointAttributes where
  type Rs SetEndpointAttributes = SetEndpointAttributesResponse
  request = Req.postQuery snsService
  response = Res.receiveNull SetEndpointAttributesResponse'

instance Lude.ToHeaders SetEndpointAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetEndpointAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery SetEndpointAttributes where
  toQuery SetEndpointAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetEndpointAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "Attributes"
          Lude.=: Lude.toQueryMap "entry" "key" "value" attributes,
        "EndpointArn" Lude.=: endpointARN
      ]

-- | /See:/ 'mkSetEndpointAttributesResponse' smart constructor.
data SetEndpointAttributesResponse = SetEndpointAttributesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetEndpointAttributesResponse' with the minimum fields required to make a request.
mkSetEndpointAttributesResponse ::
  SetEndpointAttributesResponse
mkSetEndpointAttributesResponse = SetEndpointAttributesResponse'

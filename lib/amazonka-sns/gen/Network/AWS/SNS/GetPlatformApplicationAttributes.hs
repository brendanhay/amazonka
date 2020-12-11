{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the attributes of the platform application object for the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
module Network.AWS.SNS.GetPlatformApplicationAttributes
  ( -- * Creating a request
    GetPlatformApplicationAttributes (..),
    mkGetPlatformApplicationAttributes,

    -- ** Request lenses
    gpaaPlatformApplicationARN,

    -- * Destructuring the response
    GetPlatformApplicationAttributesResponse (..),
    mkGetPlatformApplicationAttributesResponse,

    -- ** Response lenses
    gpaarsAttributes,
    gpaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributes' smart constructor.
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
  { platformApplicationARN ::
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

-- | Creates a value of 'GetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn for GetPlatformApplicationAttributesInput.
mkGetPlatformApplicationAttributes ::
  -- | 'platformApplicationARN'
  Lude.Text ->
  GetPlatformApplicationAttributes
mkGetPlatformApplicationAttributes pPlatformApplicationARN_ =
  GetPlatformApplicationAttributes'
    { platformApplicationARN =
        pPlatformApplicationARN_
    }

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaaPlatformApplicationARN :: Lens.Lens' GetPlatformApplicationAttributes Lude.Text
gpaaPlatformApplicationARN = Lens.lens (platformApplicationARN :: GetPlatformApplicationAttributes -> Lude.Text) (\s a -> s {platformApplicationARN = a} :: GetPlatformApplicationAttributes)
{-# DEPRECATED gpaaPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

instance Lude.AWSRequest GetPlatformApplicationAttributes where
  type
    Rs GetPlatformApplicationAttributes =
      GetPlatformApplicationAttributesResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "GetPlatformApplicationAttributesResult"
      ( \s h x ->
          GetPlatformApplicationAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPlatformApplicationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPlatformApplicationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPlatformApplicationAttributes where
  toQuery GetPlatformApplicationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetPlatformApplicationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "PlatformApplicationArn" Lude.=: platformApplicationARN
      ]

-- | Response for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributesResponse' smart constructor.
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
  { attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPlatformApplicationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Attributes include the following:
--
--
--     * @EventEndpointCreated@ – Topic ARN to which EndpointCreated event notifications should be sent.
--
--
--     * @EventEndpointDeleted@ – Topic ARN to which EndpointDeleted event notifications should be sent.
--
--
--     * @EventEndpointUpdated@ – Topic ARN to which EndpointUpdate event notifications should be sent.
--
--
--     * @EventDeliveryFailure@ – Topic ARN to which DeliveryFailure event notifications should be sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.
--
--
-- * 'responseStatus' - The response status code.
mkGetPlatformApplicationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPlatformApplicationAttributesResponse
mkGetPlatformApplicationAttributesResponse pResponseStatus_ =
  GetPlatformApplicationAttributesResponse'
    { attributes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Attributes include the following:
--
--
--     * @EventEndpointCreated@ – Topic ARN to which EndpointCreated event notifications should be sent.
--
--
--     * @EventEndpointDeleted@ – Topic ARN to which EndpointDeleted event notifications should be sent.
--
--
--     * @EventEndpointUpdated@ – Topic ARN to which EndpointUpdate event notifications should be sent.
--
--
--     * @EventDeliveryFailure@ – Topic ARN to which DeliveryFailure event notifications should be sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaarsAttributes :: Lens.Lens' GetPlatformApplicationAttributesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gpaarsAttributes = Lens.lens (attributes :: GetPlatformApplicationAttributesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: GetPlatformApplicationAttributesResponse)
{-# DEPRECATED gpaarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaarsResponseStatus :: Lens.Lens' GetPlatformApplicationAttributesResponse Lude.Int
gpaarsResponseStatus = Lens.lens (responseStatus :: GetPlatformApplicationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPlatformApplicationAttributesResponse)
{-# DEPRECATED gpaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

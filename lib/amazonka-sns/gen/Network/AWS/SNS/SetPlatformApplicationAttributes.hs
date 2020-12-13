{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes of the platform application object for the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> . For information on configuring attributes for message delivery status, see <https://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status> .
module Network.AWS.SNS.SetPlatformApplicationAttributes
  ( -- * Creating a request
    SetPlatformApplicationAttributes (..),
    mkSetPlatformApplicationAttributes,

    -- ** Request lenses
    spaaPlatformApplicationARN,
    spaaAttributes,

    -- * Destructuring the response
    SetPlatformApplicationAttributesResponse (..),
    mkSetPlatformApplicationAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for SetPlatformApplicationAttributes action.
--
-- /See:/ 'mkSetPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { -- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
    platformApplicationARN :: Lude.Text,
    -- | A map of the platform application attributes. Attributes in this map include the following:
    --
    --
    --     * @PlatformCredential@ – The credential received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformCredential@ is @private key@ . For @GCM@ (Firebase Cloud Messaging), @PlatformCredential@ is @API key@ . For @ADM@ , @PlatformCredential@ is @client secret@ .
    --
    --
    --     * @PlatformPrincipal@ – The principal received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ . For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ . For @ADM@ , @PlatformPrincipal@ is @client id@ .
    --
    --
    --     * @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event notifications are sent.
    --
    --
    --     * @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event notifications are sent.
    --
    --
    --     * @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event notifications are sent.
    --
    --
    --     * @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event notifications are sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.
    --
    --
    --     * @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
    --
    --
    --     * @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
    --
    --
    --     * @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of successfully delivered messages.
    attributes :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn for SetPlatformApplicationAttributes action.
-- * 'attributes' - A map of the platform application attributes. Attributes in this map include the following:
--
--
--     * @PlatformCredential@ – The credential received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformCredential@ is @private key@ . For @GCM@ (Firebase Cloud Messaging), @PlatformCredential@ is @API key@ . For @ADM@ , @PlatformCredential@ is @client secret@ .
--
--
--     * @PlatformPrincipal@ – The principal received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ . For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ . For @ADM@ , @PlatformPrincipal@ is @client id@ .
--
--
--     * @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event notifications are sent.
--
--
--     * @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event notifications are sent.
--
--
--     * @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event notifications are sent.
--
--
--     * @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event notifications are sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.
--
--
--     * @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
--
--
--     * @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
--
--
--     * @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of successfully delivered messages.
mkSetPlatformApplicationAttributes ::
  -- | 'platformApplicationARN'
  Lude.Text ->
  SetPlatformApplicationAttributes
mkSetPlatformApplicationAttributes pPlatformApplicationARN_ =
  SetPlatformApplicationAttributes'
    { platformApplicationARN =
        pPlatformApplicationARN_,
      attributes = Lude.mempty
    }

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPlatformApplicationARN :: Lens.Lens' SetPlatformApplicationAttributes Lude.Text
spaaPlatformApplicationARN = Lens.lens (platformApplicationARN :: SetPlatformApplicationAttributes -> Lude.Text) (\s a -> s {platformApplicationARN = a} :: SetPlatformApplicationAttributes)
{-# DEPRECATED spaaPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

-- | A map of the platform application attributes. Attributes in this map include the following:
--
--
--     * @PlatformCredential@ – The credential received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformCredential@ is @private key@ . For @GCM@ (Firebase Cloud Messaging), @PlatformCredential@ is @API key@ . For @ADM@ , @PlatformCredential@ is @client secret@ .
--
--
--     * @PlatformPrincipal@ – The principal received from the notification service. For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ . For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ . For @ADM@ , @PlatformPrincipal@ is @client id@ .
--
--
--     * @EventEndpointCreated@ – Topic ARN to which @EndpointCreated@ event notifications are sent.
--
--
--     * @EventEndpointDeleted@ – Topic ARN to which @EndpointDeleted@ event notifications are sent.
--
--
--     * @EventEndpointUpdated@ – Topic ARN to which @EndpointUpdate@ event notifications are sent.
--
--
--     * @EventDeliveryFailure@ – Topic ARN to which @DeliveryFailure@ event notifications are sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.
--
--
--     * @SuccessFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
--
--
--     * @FailureFeedbackRoleArn@ – IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.
--
--
--     * @SuccessFeedbackSampleRate@ – Sample rate percentage (0-100) of successfully delivered messages.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaAttributes :: Lens.Lens' SetPlatformApplicationAttributes (Lude.HashMap Lude.Text (Lude.Text))
spaaAttributes = Lens.lens (attributes :: SetPlatformApplicationAttributes -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {attributes = a} :: SetPlatformApplicationAttributes)
{-# DEPRECATED spaaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest SetPlatformApplicationAttributes where
  type
    Rs SetPlatformApplicationAttributes =
      SetPlatformApplicationAttributesResponse
  request = Req.postQuery snsService
  response =
    Res.receiveNull SetPlatformApplicationAttributesResponse'

instance Lude.ToHeaders SetPlatformApplicationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetPlatformApplicationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery SetPlatformApplicationAttributes where
  toQuery SetPlatformApplicationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetPlatformApplicationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "PlatformApplicationArn" Lude.=: platformApplicationARN,
        "Attributes"
          Lude.=: Lude.toQueryMap "entry" "key" "value" attributes
      ]

-- | /See:/ 'mkSetPlatformApplicationAttributesResponse' smart constructor.
data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetPlatformApplicationAttributesResponse' with the minimum fields required to make a request.
mkSetPlatformApplicationAttributesResponse ::
  SetPlatformApplicationAttributesResponse
mkSetPlatformApplicationAttributesResponse =
  SetPlatformApplicationAttributesResponse'

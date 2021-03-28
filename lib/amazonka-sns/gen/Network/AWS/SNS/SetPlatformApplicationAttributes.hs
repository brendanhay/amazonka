{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetPlatformApplicationAttributes (..)
    , mkSetPlatformApplicationAttributes
    -- ** Request lenses
    , spaaPlatformApplicationArn
    , spaaAttributes

    -- * Destructuring the response
    , SetPlatformApplicationAttributesResponse (..)
    , mkSetPlatformApplicationAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for SetPlatformApplicationAttributes action.
--
-- /See:/ 'mkSetPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { platformApplicationArn :: Core.Text
    -- ^ PlatformApplicationArn for SetPlatformApplicationAttributes action.
  , attributes :: Core.HashMap Core.Text Core.Text
    -- ^ A map of the platform application attributes. Attributes in this map include the following:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetPlatformApplicationAttributes' value with any optional fields omitted.
mkSetPlatformApplicationAttributes
    :: Core.Text -- ^ 'platformApplicationArn'
    -> SetPlatformApplicationAttributes
mkSetPlatformApplicationAttributes platformApplicationArn
  = SetPlatformApplicationAttributes'{platformApplicationArn,
                                      attributes = Core.mempty}

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPlatformApplicationArn :: Lens.Lens' SetPlatformApplicationAttributes Core.Text
spaaPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# INLINEABLE spaaPlatformApplicationArn #-}
{-# DEPRECATED platformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead"  #-}

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
spaaAttributes :: Lens.Lens' SetPlatformApplicationAttributes (Core.HashMap Core.Text Core.Text)
spaaAttributes = Lens.field @"attributes"
{-# INLINEABLE spaaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery SetPlatformApplicationAttributes where
        toQuery SetPlatformApplicationAttributes{..}
          = Core.toQueryPair "Action"
              ("SetPlatformApplicationAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "PlatformApplicationArn" platformApplicationArn
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.toQueryMap "entry" "key" "value" attributes)

instance Core.ToHeaders SetPlatformApplicationAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetPlatformApplicationAttributes where
        type Rs SetPlatformApplicationAttributes =
             SetPlatformApplicationAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SetPlatformApplicationAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetPlatformApplicationAttributesResponse' smart constructor.
data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetPlatformApplicationAttributesResponse' value with any optional fields omitted.
mkSetPlatformApplicationAttributesResponse
    :: SetPlatformApplicationAttributesResponse
mkSetPlatformApplicationAttributesResponse
  = SetPlatformApplicationAttributesResponse'

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetEndpointAttributes (..)
    , mkSetEndpointAttributes
    -- ** Request lenses
    , seaEndpointArn
    , seaAttributes

    -- * Destructuring the response
    , SetEndpointAttributesResponse (..)
    , mkSetEndpointAttributesResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for SetEndpointAttributes action.
--
-- /See:/ 'mkSetEndpointAttributes' smart constructor.
data SetEndpointAttributes = SetEndpointAttributes'
  { endpointArn :: Core.Text
    -- ^ EndpointArn used for SetEndpointAttributes action.
  , attributes :: Core.HashMap Core.Text Core.Text
    -- ^ A map of the endpoint attributes. Attributes in this map include the following:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetEndpointAttributes' value with any optional fields omitted.
mkSetEndpointAttributes
    :: Core.Text -- ^ 'endpointArn'
    -> SetEndpointAttributes
mkSetEndpointAttributes endpointArn
  = SetEndpointAttributes'{endpointArn, attributes = Core.mempty}

-- | EndpointArn used for SetEndpointAttributes action.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seaEndpointArn :: Lens.Lens' SetEndpointAttributes Core.Text
seaEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE seaEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

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
seaAttributes :: Lens.Lens' SetEndpointAttributes (Core.HashMap Core.Text Core.Text)
seaAttributes = Lens.field @"attributes"
{-# INLINEABLE seaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery SetEndpointAttributes where
        toQuery SetEndpointAttributes{..}
          = Core.toQueryPair "Action" ("SetEndpointAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<> Core.toQueryPair "EndpointArn" endpointArn
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.toQueryMap "entry" "key" "value" attributes)

instance Core.ToHeaders SetEndpointAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetEndpointAttributes where
        type Rs SetEndpointAttributes = SetEndpointAttributesResponse
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
        parseResponse = Response.receiveNull SetEndpointAttributesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetEndpointAttributesResponse' smart constructor.
data SetEndpointAttributesResponse = SetEndpointAttributesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetEndpointAttributesResponse' value with any optional fields omitted.
mkSetEndpointAttributesResponse
    :: SetEndpointAttributesResponse
mkSetEndpointAttributesResponse = SetEndpointAttributesResponse'

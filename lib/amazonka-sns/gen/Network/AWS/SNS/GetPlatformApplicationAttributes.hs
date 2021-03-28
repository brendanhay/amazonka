{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPlatformApplicationAttributes (..)
    , mkGetPlatformApplicationAttributes
    -- ** Request lenses
    , gpaaPlatformApplicationArn

    -- * Destructuring the response
    , GetPlatformApplicationAttributesResponse (..)
    , mkGetPlatformApplicationAttributesResponse
    -- ** Response lenses
    , gpaarrsAttributes
    , gpaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributes' smart constructor.
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
  { platformApplicationArn :: Core.Text
    -- ^ PlatformApplicationArn for GetPlatformApplicationAttributesInput.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPlatformApplicationAttributes' value with any optional fields omitted.
mkGetPlatformApplicationAttributes
    :: Core.Text -- ^ 'platformApplicationArn'
    -> GetPlatformApplicationAttributes
mkGetPlatformApplicationAttributes platformApplicationArn
  = GetPlatformApplicationAttributes'{platformApplicationArn}

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaaPlatformApplicationArn :: Lens.Lens' GetPlatformApplicationAttributes Core.Text
gpaaPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# INLINEABLE gpaaPlatformApplicationArn #-}
{-# DEPRECATED platformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead"  #-}

instance Core.ToQuery GetPlatformApplicationAttributes where
        toQuery GetPlatformApplicationAttributes{..}
          = Core.toQueryPair "Action"
              ("GetPlatformApplicationAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "PlatformApplicationArn" platformApplicationArn

instance Core.ToHeaders GetPlatformApplicationAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetPlatformApplicationAttributes where
        type Rs GetPlatformApplicationAttributes =
             GetPlatformApplicationAttributesResponse
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
          = Response.receiveXMLWrapper
              "GetPlatformApplicationAttributesResult"
              (\ s h x ->
                 GetPlatformApplicationAttributesResponse' Core.<$>
                   (x Core..@? "Attributes" Core..<@>
                      Core.parseXMLMap "entry" "key" "value")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributesResponse' smart constructor.
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
  { attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Attributes include the following:
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPlatformApplicationAttributesResponse' value with any optional fields omitted.
mkGetPlatformApplicationAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPlatformApplicationAttributesResponse
mkGetPlatformApplicationAttributesResponse responseStatus
  = GetPlatformApplicationAttributesResponse'{attributes =
                                                Core.Nothing,
                                              responseStatus}

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
gpaarrsAttributes :: Lens.Lens' GetPlatformApplicationAttributesResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gpaarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE gpaarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaarrsResponseStatus :: Lens.Lens' GetPlatformApplicationAttributesResponse Core.Int
gpaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

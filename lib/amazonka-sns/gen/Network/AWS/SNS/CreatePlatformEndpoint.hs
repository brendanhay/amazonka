{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePlatformEndpoint (..)
    , mkCreatePlatformEndpoint
    -- ** Request lenses
    , cpePlatformApplicationArn
    , cpeToken
    , cpeAttributes
    , cpeCustomUserData

    -- * Destructuring the response
    , CreatePlatformEndpointResponse (..)
    , mkCreatePlatformEndpointResponse
    -- ** Response lenses
    , cperrsEndpointArn
    , cperrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for CreatePlatformEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpoint' smart constructor.
data CreatePlatformEndpoint = CreatePlatformEndpoint'
  { platformApplicationArn :: Core.Text
    -- ^ PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
  , token :: Core.Text
    -- ^ Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
  , attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
  , customUserData :: Core.Maybe Core.Text
    -- ^ Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformEndpoint' value with any optional fields omitted.
mkCreatePlatformEndpoint
    :: Core.Text -- ^ 'platformApplicationArn'
    -> Core.Text -- ^ 'token'
    -> CreatePlatformEndpoint
mkCreatePlatformEndpoint platformApplicationArn token
  = CreatePlatformEndpoint'{platformApplicationArn, token,
                            attributes = Core.Nothing, customUserData = Core.Nothing}

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpePlatformApplicationArn :: Lens.Lens' CreatePlatformEndpoint Core.Text
cpePlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# INLINEABLE cpePlatformApplicationArn #-}
{-# DEPRECATED platformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead"  #-}

-- | Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeToken :: Lens.Lens' CreatePlatformEndpoint Core.Text
cpeToken = Lens.field @"token"
{-# INLINEABLE cpeToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeAttributes :: Lens.Lens' CreatePlatformEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
cpeAttributes = Lens.field @"attributes"
{-# INLINEABLE cpeAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
--
-- /Note:/ Consider using 'customUserData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeCustomUserData :: Lens.Lens' CreatePlatformEndpoint (Core.Maybe Core.Text)
cpeCustomUserData = Lens.field @"customUserData"
{-# INLINEABLE cpeCustomUserData #-}
{-# DEPRECATED customUserData "Use generic-lens or generic-optics with 'customUserData' instead"  #-}

instance Core.ToQuery CreatePlatformEndpoint where
        toQuery CreatePlatformEndpoint{..}
          = Core.toQueryPair "Action" ("CreatePlatformEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "PlatformApplicationArn" platformApplicationArn
              Core.<> Core.toQueryPair "Token" token
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   attributes)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CustomUserData")
                customUserData

instance Core.ToHeaders CreatePlatformEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreatePlatformEndpoint where
        type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse
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
          = Response.receiveXMLWrapper "CreatePlatformEndpointResult"
              (\ s h x ->
                 CreatePlatformEndpointResponse' Core.<$>
                   (x Core..@? "EndpointArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response from CreateEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpointResponse' smart constructor.
data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse'
  { endpointArn :: Core.Maybe Core.Text
    -- ^ EndpointArn returned from CreateEndpoint action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformEndpointResponse' value with any optional fields omitted.
mkCreatePlatformEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePlatformEndpointResponse
mkCreatePlatformEndpointResponse responseStatus
  = CreatePlatformEndpointResponse'{endpointArn = Core.Nothing,
                                    responseStatus}

-- | EndpointArn returned from CreateEndpoint action.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperrsEndpointArn :: Lens.Lens' CreatePlatformEndpointResponse (Core.Maybe Core.Text)
cperrsEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE cperrsEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperrsResponseStatus :: Lens.Lens' CreatePlatformEndpointResponse Core.Int
cperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

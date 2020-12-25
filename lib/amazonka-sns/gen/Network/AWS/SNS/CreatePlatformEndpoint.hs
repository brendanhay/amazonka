{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cpePlatformApplicationArn,
    cpeToken,
    cpeAttributes,
    cpeCustomUserData,

    -- * Destructuring the response
    CreatePlatformEndpointResponse (..),
    mkCreatePlatformEndpointResponse,

    -- ** Response lenses
    cperrsEndpointArn,
    cperrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for CreatePlatformEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpoint' smart constructor.
data CreatePlatformEndpoint = CreatePlatformEndpoint'
  { -- | PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
    platformApplicationArn :: Types.String,
    -- | Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
    token :: Types.String,
    -- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
    attributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
    customUserData :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformEndpoint' value with any optional fields omitted.
mkCreatePlatformEndpoint ::
  -- | 'platformApplicationArn'
  Types.String ->
  -- | 'token'
  Types.String ->
  CreatePlatformEndpoint
mkCreatePlatformEndpoint platformApplicationArn token =
  CreatePlatformEndpoint'
    { platformApplicationArn,
      token,
      attributes = Core.Nothing,
      customUserData = Core.Nothing
    }

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to create a an endpoint.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpePlatformApplicationArn :: Lens.Lens' CreatePlatformEndpoint Types.String
cpePlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# DEPRECATED cpePlatformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead." #-}

-- | Unique identifier created by the notification service for an app on a device. The specific name for Token will vary, depending on which notification service is being used. For example, when using APNS as the notification service, you need the device token. Alternatively, when using GCM (Firebase Cloud Messaging) or ADM, the device token equivalent is called the registration ID.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeToken :: Lens.Lens' CreatePlatformEndpoint Types.String
cpeToken = Lens.field @"token"
{-# DEPRECATED cpeToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes> .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeAttributes :: Lens.Lens' CreatePlatformEndpoint (Core.Maybe (Core.HashMap Types.String Types.String))
cpeAttributes = Lens.field @"attributes"
{-# DEPRECATED cpeAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.
--
-- /Note:/ Consider using 'customUserData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeCustomUserData :: Lens.Lens' CreatePlatformEndpoint (Core.Maybe Types.String)
cpeCustomUserData = Lens.field @"customUserData"
{-# DEPRECATED cpeCustomUserData "Use generic-lens or generic-optics with 'customUserData' instead." #-}

instance Core.AWSRequest CreatePlatformEndpoint where
  type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreatePlatformEndpoint")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "PlatformApplicationArn" platformApplicationArn)
                Core.<> (Core.toQueryValue "Token" token)
                Core.<> ( Core.toQueryValue
                            "Attributes"
                            (Core.toQueryMap "entry" "key" "value" Core.<$> attributes)
                        )
                Core.<> (Core.toQueryValue "CustomUserData" Core.<$> customUserData)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreatePlatformEndpointResult"
      ( \s h x ->
          CreatePlatformEndpointResponse'
            Core.<$> (x Core..@? "EndpointArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Response from CreateEndpoint action.
--
-- /See:/ 'mkCreatePlatformEndpointResponse' smart constructor.
data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse'
  { -- | EndpointArn returned from CreateEndpoint action.
    endpointArn :: Core.Maybe Types.EndpointArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePlatformEndpointResponse' value with any optional fields omitted.
mkCreatePlatformEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePlatformEndpointResponse
mkCreatePlatformEndpointResponse responseStatus =
  CreatePlatformEndpointResponse'
    { endpointArn = Core.Nothing,
      responseStatus
    }

-- | EndpointArn returned from CreateEndpoint action.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperrsEndpointArn :: Lens.Lens' CreatePlatformEndpointResponse (Core.Maybe Types.EndpointArn)
cperrsEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED cperrsEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cperrsResponseStatus :: Lens.Lens' CreatePlatformEndpointResponse Core.Int
cperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

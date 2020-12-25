{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gpaaPlatformApplicationArn,

    -- * Destructuring the response
    GetPlatformApplicationAttributesResponse (..),
    mkGetPlatformApplicationAttributesResponse,

    -- ** Response lenses
    gpaarrsAttributes,
    gpaarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributes' smart constructor.
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
  { -- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
    platformApplicationArn :: Types.PlatformApplicationArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPlatformApplicationAttributes' value with any optional fields omitted.
mkGetPlatformApplicationAttributes ::
  -- | 'platformApplicationArn'
  Types.PlatformApplicationArn ->
  GetPlatformApplicationAttributes
mkGetPlatformApplicationAttributes platformApplicationArn =
  GetPlatformApplicationAttributes' {platformApplicationArn}

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaaPlatformApplicationArn :: Lens.Lens' GetPlatformApplicationAttributes Types.PlatformApplicationArn
gpaaPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# DEPRECATED gpaaPlatformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead." #-}

instance Core.AWSRequest GetPlatformApplicationAttributes where
  type
    Rs GetPlatformApplicationAttributes =
      GetPlatformApplicationAttributesResponse
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
            ( Core.pure ("Action", "GetPlatformApplicationAttributes")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> ( Core.toQueryValue
                            "PlatformApplicationArn"
                            platformApplicationArn
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetPlatformApplicationAttributesResult"
      ( \s h x ->
          GetPlatformApplicationAttributesResponse'
            Core.<$> ( x Core..@? "Attributes"
                         Core..<@> Core.parseXMLMap "entry" "key" "value"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Response for GetPlatformApplicationAttributes action.
--
-- /See:/ 'mkGetPlatformApplicationAttributesResponse' smart constructor.
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
  { -- | Attributes include the following:
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
    attributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPlatformApplicationAttributesResponse' value with any optional fields omitted.
mkGetPlatformApplicationAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPlatformApplicationAttributesResponse
mkGetPlatformApplicationAttributesResponse responseStatus =
  GetPlatformApplicationAttributesResponse'
    { attributes =
        Core.Nothing,
      responseStatus
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
gpaarrsAttributes :: Lens.Lens' GetPlatformApplicationAttributesResponse (Core.Maybe (Core.HashMap Types.String Types.String))
gpaarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED gpaarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpaarrsResponseStatus :: Lens.Lens' GetPlatformApplicationAttributesResponse Core.Int
gpaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

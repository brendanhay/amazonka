{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContactMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured contact methods. Specify a protocol in your request to return information about a specific contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.GetContactMethods
  ( -- * Creating a request
    GetContactMethods (..),
    mkGetContactMethods,

    -- ** Request lenses
    gcmProtocols,

    -- * Destructuring the response
    GetContactMethodsResponse (..),
    mkGetContactMethodsResponse,

    -- ** Response lenses
    gcmrrsContactMethods,
    gcmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContactMethods' smart constructor.
newtype GetContactMethods = GetContactMethods'
  { -- | The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging).
    --
    -- Specify a protocol in your request to return information about a specific contact method protocol.
    protocols :: Core.Maybe [Types.ContactProtocol]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactMethods' value with any optional fields omitted.
mkGetContactMethods ::
  GetContactMethods
mkGetContactMethods = GetContactMethods' {protocols = Core.Nothing}

-- | The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging).
--
-- Specify a protocol in your request to return information about a specific contact method protocol.
--
-- /Note:/ Consider using 'protocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmProtocols :: Lens.Lens' GetContactMethods (Core.Maybe [Types.ContactProtocol])
gcmProtocols = Lens.field @"protocols"
{-# DEPRECATED gcmProtocols "Use generic-lens or generic-optics with 'protocols' instead." #-}

instance Core.FromJSON GetContactMethods where
  toJSON GetContactMethods {..} =
    Core.object
      (Core.catMaybes [("protocols" Core..=) Core.<$> protocols])

instance Core.AWSRequest GetContactMethods where
  type Rs GetContactMethods = GetContactMethodsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetContactMethods")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactMethodsResponse'
            Core.<$> (x Core..:? "contactMethods")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContactMethodsResponse' smart constructor.
data GetContactMethodsResponse = GetContactMethodsResponse'
  { -- | An array of objects that describe the contact methods.
    contactMethods :: Core.Maybe [Types.ContactMethod],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContactMethodsResponse' value with any optional fields omitted.
mkGetContactMethodsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContactMethodsResponse
mkGetContactMethodsResponse responseStatus =
  GetContactMethodsResponse'
    { contactMethods = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the contact methods.
--
-- /Note:/ Consider using 'contactMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsContactMethods :: Lens.Lens' GetContactMethodsResponse (Core.Maybe [Types.ContactMethod])
gcmrrsContactMethods = Lens.field @"contactMethods"
{-# DEPRECATED gcmrrsContactMethods "Use generic-lens or generic-optics with 'contactMethods' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsResponseStatus :: Lens.Lens' GetContactMethodsResponse Core.Int
gcmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

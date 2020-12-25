{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.GetContact
  ( -- * Creating a request
    GetContact (..),
    mkGetContact,

    -- ** Request lenses
    gcContactArn,

    -- * Destructuring the response
    GetContactResponse (..),
    mkGetContactResponse,

    -- ** Response lenses
    gcrrsContact,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContact' smart constructor.
newtype GetContact = GetContact'
  { -- | The ARN of the contact for which to request details.
    contactArn :: Types.ContactArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContact' value with any optional fields omitted.
mkGetContact ::
  -- | 'contactArn'
  Types.ContactArn ->
  GetContact
mkGetContact contactArn = GetContact' {contactArn}

-- | The ARN of the contact for which to request details.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcContactArn :: Lens.Lens' GetContact Types.ContactArn
gcContactArn = Lens.field @"contactArn"
{-# DEPRECATED gcContactArn "Use generic-lens or generic-optics with 'contactArn' instead." #-}

instance Core.FromJSON GetContact where
  toJSON GetContact {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ContactArn" Core..= contactArn)])

instance Core.AWSRequest GetContact where
  type Rs GetContact = GetContactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetContact")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Core.<$> (x Core..:? "Contact") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The details of the requested contact.
    contact :: Core.Maybe Types.Contact,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactResponse' value with any optional fields omitted.
mkGetContactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContactResponse
mkGetContactResponse responseStatus =
  GetContactResponse' {contact = Core.Nothing, responseStatus}

-- | The details of the requested contact.
--
-- /Note:/ Consider using 'contact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsContact :: Lens.Lens' GetContactResponse (Core.Maybe Types.Contact)
gcrrsContact = Lens.field @"contact"
{-# DEPRECATED gcrrsContact "Use generic-lens or generic-optics with 'contact' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetContactResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

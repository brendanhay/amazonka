{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetContact (..)
    , mkGetContact
    -- ** Request lenses
    , gcContactArn

    -- * Destructuring the response
    , GetContactResponse (..)
    , mkGetContactResponse
    -- ** Response lenses
    , gcrrsContact
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContact' smart constructor.
newtype GetContact = GetContact'
  { contactArn :: Types.ContactArn
    -- ^ The ARN of the contact for which to request details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContact' value with any optional fields omitted.
mkGetContact
    :: Types.ContactArn -- ^ 'contactArn'
    -> GetContact
mkGetContact contactArn = GetContact'{contactArn}

-- | The ARN of the contact for which to request details.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcContactArn :: Lens.Lens' GetContact Types.ContactArn
gcContactArn = Lens.field @"contactArn"
{-# INLINEABLE gcContactArn #-}
{-# DEPRECATED contactArn "Use generic-lens or generic-optics with 'contactArn' instead"  #-}

instance Core.ToQuery GetContact where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContact where
        toHeaders GetContact{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.GetContact") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContact where
        toJSON GetContact{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ContactArn" Core..= contactArn)])

instance Core.AWSRequest GetContact where
        type Rs GetContact = GetContactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContactResponse' Core.<$>
                   (x Core..:? "Contact") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { contact :: Core.Maybe Types.Contact
    -- ^ The details of the requested contact.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContactResponse' value with any optional fields omitted.
mkGetContactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContactResponse
mkGetContactResponse responseStatus
  = GetContactResponse'{contact = Core.Nothing, responseStatus}

-- | The details of the requested contact.
--
-- /Note:/ Consider using 'contact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsContact :: Lens.Lens' GetContactResponse (Core.Maybe Types.Contact)
gcrrsContact = Lens.field @"contact"
{-# INLINEABLE gcrrsContact #-}
{-# DEPRECATED contact "Use generic-lens or generic-optics with 'contact' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetContactResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

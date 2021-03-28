{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.StopContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends the specified contact.
module Network.AWS.Connect.StopContact
    (
    -- * Creating a request
      StopContact (..)
    , mkStopContact
    -- ** Request lenses
    , scContactId
    , scInstanceId

    -- * Destructuring the response
    , StopContactResponse (..)
    , mkStopContactResponse
    -- ** Response lenses
    , scrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopContact' smart constructor.
data StopContact = StopContact'
  { contactId :: Types.ContactId
    -- ^ The ID of the contact.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopContact' value with any optional fields omitted.
mkStopContact
    :: Types.ContactId -- ^ 'contactId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> StopContact
mkStopContact contactId instanceId
  = StopContact'{contactId, instanceId}

-- | The ID of the contact.
--
-- /Note:/ Consider using 'contactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scContactId :: Lens.Lens' StopContact Types.ContactId
scContactId = Lens.field @"contactId"
{-# INLINEABLE scContactId #-}
{-# DEPRECATED contactId "Use generic-lens or generic-optics with 'contactId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scInstanceId :: Lens.Lens' StopContact Types.InstanceId
scInstanceId = Lens.field @"instanceId"
{-# INLINEABLE scInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery StopContact where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopContact where
        toHeaders StopContact{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopContact where
        toJSON StopContact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContactId" Core..= contactId),
                  Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest StopContact where
        type Rs StopContact = StopContactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/contact/stop",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopContactResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopContactResponse' smart constructor.
newtype StopContactResponse = StopContactResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopContactResponse' value with any optional fields omitted.
mkStopContactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopContactResponse
mkStopContactResponse responseStatus
  = StopContactResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrrsResponseStatus :: Lens.Lens' StopContactResponse Core.Int
scrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

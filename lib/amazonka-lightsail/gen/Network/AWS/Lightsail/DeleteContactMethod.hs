{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.DeleteContactMethod
    (
    -- * Creating a request
      DeleteContactMethod (..)
    , mkDeleteContactMethod
    -- ** Request lenses
    , dcmProtocol

    -- * Destructuring the response
    , DeleteContactMethodResponse (..)
    , mkDeleteContactMethodResponse
    -- ** Response lenses
    , dcmrrsOperations
    , dcmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContactMethod' smart constructor.
newtype DeleteContactMethod = DeleteContactMethod'
  { protocol :: Types.ContactProtocol
    -- ^ The protocol that will be deleted, such as @Email@ or @SMS@ (text messaging).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContactMethod' value with any optional fields omitted.
mkDeleteContactMethod
    :: Types.ContactProtocol -- ^ 'protocol'
    -> DeleteContactMethod
mkDeleteContactMethod protocol = DeleteContactMethod'{protocol}

-- | The protocol that will be deleted, such as @Email@ or @SMS@ (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmProtocol :: Lens.Lens' DeleteContactMethod Types.ContactProtocol
dcmProtocol = Lens.field @"protocol"
{-# INLINEABLE dcmProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

instance Core.ToQuery DeleteContactMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteContactMethod where
        toHeaders DeleteContactMethod{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteContactMethod")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteContactMethod where
        toJSON DeleteContactMethod{..}
          = Core.object
              (Core.catMaybes [Core.Just ("protocol" Core..= protocol)])

instance Core.AWSRequest DeleteContactMethod where
        type Rs DeleteContactMethod = DeleteContactMethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteContactMethodResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteContactMethodResponse' smart constructor.
data DeleteContactMethodResponse = DeleteContactMethodResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteContactMethodResponse' value with any optional fields omitted.
mkDeleteContactMethodResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteContactMethodResponse
mkDeleteContactMethodResponse responseStatus
  = DeleteContactMethodResponse'{operations = Core.Nothing,
                                 responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrrsOperations :: Lens.Lens' DeleteContactMethodResponse (Core.Maybe [Types.Operation])
dcmrrsOperations = Lens.field @"operations"
{-# INLINEABLE dcmrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrrsResponseStatus :: Lens.Lens' DeleteContactMethodResponse Core.Int
dcmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

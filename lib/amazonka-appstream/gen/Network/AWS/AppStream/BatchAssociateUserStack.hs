{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.BatchAssociateUserStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified users with the specified stacks. Users in a user pool cannot be assigned to stacks with fleets that are joined to an Active Directory domain.
module Network.AWS.AppStream.BatchAssociateUserStack
    (
    -- * Creating a request
      BatchAssociateUserStack (..)
    , mkBatchAssociateUserStack
    -- ** Request lenses
    , bausUserStackAssociations

    -- * Destructuring the response
    , BatchAssociateUserStackResponse (..)
    , mkBatchAssociateUserStackResponse
    -- ** Response lenses
    , bausrrsErrors
    , bausrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchAssociateUserStack' smart constructor.
newtype BatchAssociateUserStack = BatchAssociateUserStack'
  { userStackAssociations :: Core.NonEmpty Types.UserStackAssociation
    -- ^ The list of UserStackAssociation objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAssociateUserStack' value with any optional fields omitted.
mkBatchAssociateUserStack
    :: Core.NonEmpty Types.UserStackAssociation -- ^ 'userStackAssociations'
    -> BatchAssociateUserStack
mkBatchAssociateUserStack userStackAssociations
  = BatchAssociateUserStack'{userStackAssociations}

-- | The list of UserStackAssociation objects.
--
-- /Note:/ Consider using 'userStackAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausUserStackAssociations :: Lens.Lens' BatchAssociateUserStack (Core.NonEmpty Types.UserStackAssociation)
bausUserStackAssociations = Lens.field @"userStackAssociations"
{-# INLINEABLE bausUserStackAssociations #-}
{-# DEPRECATED userStackAssociations "Use generic-lens or generic-optics with 'userStackAssociations' instead"  #-}

instance Core.ToQuery BatchAssociateUserStack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchAssociateUserStack where
        toHeaders BatchAssociateUserStack{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.BatchAssociateUserStack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchAssociateUserStack where
        toJSON BatchAssociateUserStack{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("UserStackAssociations" Core..= userStackAssociations)])

instance Core.AWSRequest BatchAssociateUserStack where
        type Rs BatchAssociateUserStack = BatchAssociateUserStackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchAssociateUserStackResponse' Core.<$>
                   (x Core..:? "errors") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchAssociateUserStackResponse' smart constructor.
data BatchAssociateUserStackResponse = BatchAssociateUserStackResponse'
  { errors :: Core.Maybe [Types.UserStackAssociationError]
    -- ^ The list of UserStackAssociationError objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAssociateUserStackResponse' value with any optional fields omitted.
mkBatchAssociateUserStackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchAssociateUserStackResponse
mkBatchAssociateUserStackResponse responseStatus
  = BatchAssociateUserStackResponse'{errors = Core.Nothing,
                                     responseStatus}

-- | The list of UserStackAssociationError objects.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausrrsErrors :: Lens.Lens' BatchAssociateUserStackResponse (Core.Maybe [Types.UserStackAssociationError])
bausrrsErrors = Lens.field @"errors"
{-# INLINEABLE bausrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausrrsResponseStatus :: Lens.Lens' BatchAssociateUserStackResponse Core.Int
bausrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bausrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified constraint.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteConstraint
    (
    -- * Creating a request
      DeleteConstraint (..)
    , mkDeleteConstraint
    -- ** Request lenses
    , dcId
    , dcAcceptLanguage

    -- * Destructuring the response
    , DeleteConstraintResponse (..)
    , mkDeleteConstraintResponse
    -- ** Response lenses
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteConstraint' smart constructor.
data DeleteConstraint = DeleteConstraint'
  { id :: Types.Id
    -- ^ The identifier of the constraint.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConstraint' value with any optional fields omitted.
mkDeleteConstraint
    :: Types.Id -- ^ 'id'
    -> DeleteConstraint
mkDeleteConstraint id
  = DeleteConstraint'{id, acceptLanguage = Core.Nothing}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcId :: Lens.Lens' DeleteConstraint Types.Id
dcId = Lens.field @"id"
{-# INLINEABLE dcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAcceptLanguage :: Lens.Lens' DeleteConstraint (Core.Maybe Types.AcceptLanguage)
dcAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dcAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DeleteConstraint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConstraint where
        toHeaders DeleteConstraint{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteConstraint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConstraint where
        toJSON DeleteConstraint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DeleteConstraint where
        type Rs DeleteConstraint = DeleteConstraintResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteConstraintResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteConstraintResponse' smart constructor.
newtype DeleteConstraintResponse = DeleteConstraintResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConstraintResponse' value with any optional fields omitted.
mkDeleteConstraintResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConstraintResponse
mkDeleteConstraintResponse responseStatus
  = DeleteConstraintResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteConstraintResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a self-service action.
module Network.AWS.ServiceCatalog.UpdateServiceAction
    (
    -- * Creating a request
      UpdateServiceAction (..)
    , mkUpdateServiceAction
    -- ** Request lenses
    , usaId
    , usaAcceptLanguage
    , usaDefinition
    , usaDescription
    , usaName

    -- * Destructuring the response
    , UpdateServiceActionResponse (..)
    , mkUpdateServiceActionResponse
    -- ** Response lenses
    , usarrsServiceActionDetail
    , usarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateServiceAction' smart constructor.
data UpdateServiceAction = UpdateServiceAction'
  { id :: Types.Id
    -- ^ The self-service action identifier.
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
  , definition :: Core.Maybe (Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue)
    -- ^ A map that defines the self-service action.
  , description :: Core.Maybe Types.Description
    -- ^ The self-service action description.
  , name :: Core.Maybe Types.ServiceActionName
    -- ^ The self-service action name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceAction' value with any optional fields omitted.
mkUpdateServiceAction
    :: Types.Id -- ^ 'id'
    -> UpdateServiceAction
mkUpdateServiceAction id
  = UpdateServiceAction'{id, acceptLanguage = Core.Nothing,
                         definition = Core.Nothing, description = Core.Nothing,
                         name = Core.Nothing}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaId :: Lens.Lens' UpdateServiceAction Types.Id
usaId = Lens.field @"id"
{-# INLINEABLE usaId #-}
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
usaAcceptLanguage :: Lens.Lens' UpdateServiceAction (Core.Maybe Types.AcceptLanguage)
usaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE usaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | A map that defines the self-service action.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDefinition :: Lens.Lens' UpdateServiceAction (Core.Maybe (Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue))
usaDefinition = Lens.field @"definition"
{-# INLINEABLE usaDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDescription :: Lens.Lens' UpdateServiceAction (Core.Maybe Types.Description)
usaDescription = Lens.field @"description"
{-# INLINEABLE usaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaName :: Lens.Lens' UpdateServiceAction (Core.Maybe Types.ServiceActionName)
usaName = Lens.field @"name"
{-# INLINEABLE usaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateServiceAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateServiceAction where
        toHeaders UpdateServiceAction{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.UpdateServiceAction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateServiceAction where
        toJSON UpdateServiceAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("Definition" Core..=) Core.<$> definition,
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateServiceAction where
        type Rs UpdateServiceAction = UpdateServiceActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServiceActionResponse' Core.<$>
                   (x Core..:? "ServiceActionDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServiceActionResponse' smart constructor.
data UpdateServiceActionResponse = UpdateServiceActionResponse'
  { serviceActionDetail :: Core.Maybe Types.ServiceActionDetail
    -- ^ Detailed information about the self-service action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServiceActionResponse' value with any optional fields omitted.
mkUpdateServiceActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServiceActionResponse
mkUpdateServiceActionResponse responseStatus
  = UpdateServiceActionResponse'{serviceActionDetail = Core.Nothing,
                                 responseStatus}

-- | Detailed information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarrsServiceActionDetail :: Lens.Lens' UpdateServiceActionResponse (Core.Maybe Types.ServiceActionDetail)
usarrsServiceActionDetail = Lens.field @"serviceActionDetail"
{-# INLINEABLE usarrsServiceActionDetail #-}
{-# DEPRECATED serviceActionDetail "Use generic-lens or generic-optics with 'serviceActionDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarrsResponseStatus :: Lens.Lens' UpdateServiceActionResponse Core.Int
usarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

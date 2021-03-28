{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Network.AWS.Greengrass.UpdateConnectorDefinition
    (
    -- * Creating a request
      UpdateConnectorDefinition (..)
    , mkUpdateConnectorDefinition
    -- ** Request lenses
    , uConnectorDefinitionId
    , uName

    -- * Destructuring the response
    , UpdateConnectorDefinitionResponse (..)
    , mkUpdateConnectorDefinitionResponse
    -- ** Response lenses
    , ucdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { connectorDefinitionId :: Core.Text
    -- ^ The ID of the connector definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectorDefinition' value with any optional fields omitted.
mkUpdateConnectorDefinition
    :: Core.Text -- ^ 'connectorDefinitionId'
    -> UpdateConnectorDefinition
mkUpdateConnectorDefinition connectorDefinitionId
  = UpdateConnectorDefinition'{connectorDefinitionId,
                               name = Core.Nothing}

-- | The ID of the connector definition.
--
-- /Note:/ Consider using 'connectorDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConnectorDefinitionId :: Lens.Lens' UpdateConnectorDefinition Core.Text
uConnectorDefinitionId = Lens.field @"connectorDefinitionId"
{-# INLINEABLE uConnectorDefinitionId #-}
{-# DEPRECATED connectorDefinitionId "Use generic-lens or generic-optics with 'connectorDefinitionId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateConnectorDefinition (Core.Maybe Core.Text)
uName = Lens.field @"name"
{-# INLINEABLE uName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateConnectorDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateConnectorDefinition where
        toHeaders UpdateConnectorDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateConnectorDefinition where
        toJSON UpdateConnectorDefinition{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateConnectorDefinition where
        type Rs UpdateConnectorDefinition =
             UpdateConnectorDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/definition/connectors/" Core.<>
                             Core.toText connectorDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateConnectorDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateConnectorDefinitionResponse' smart constructor.
newtype UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectorDefinitionResponse' value with any optional fields omitted.
mkUpdateConnectorDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateConnectorDefinitionResponse
mkUpdateConnectorDefinitionResponse responseStatus
  = UpdateConnectorDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucdrrsResponseStatus :: Lens.Lens' UpdateConnectorDefinitionResponse Core.Int
ucdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

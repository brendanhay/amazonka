{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Network.AWS.Greengrass.UpdateFunctionDefinition
    (
    -- * Creating a request
      UpdateFunctionDefinition (..)
    , mkUpdateFunctionDefinition
    -- ** Request lenses
    , ufdFunctionDefinitionId
    , ufdName

    -- * Destructuring the response
    , UpdateFunctionDefinitionResponse (..)
    , mkUpdateFunctionDefinitionResponse
    -- ** Response lenses
    , ufdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { functionDefinitionId :: Core.Text
    -- ^ The ID of the Lambda function definition.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionDefinition' value with any optional fields omitted.
mkUpdateFunctionDefinition
    :: Core.Text -- ^ 'functionDefinitionId'
    -> UpdateFunctionDefinition
mkUpdateFunctionDefinition functionDefinitionId
  = UpdateFunctionDefinition'{functionDefinitionId,
                              name = Core.Nothing}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdFunctionDefinitionId :: Lens.Lens' UpdateFunctionDefinition Core.Text
ufdFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# INLINEABLE ufdFunctionDefinitionId #-}
{-# DEPRECATED functionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead"  #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdName :: Lens.Lens' UpdateFunctionDefinition (Core.Maybe Core.Text)
ufdName = Lens.field @"name"
{-# INLINEABLE ufdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateFunctionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFunctionDefinition where
        toHeaders UpdateFunctionDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateFunctionDefinition where
        toJSON UpdateFunctionDefinition{..}
          = Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateFunctionDefinition where
        type Rs UpdateFunctionDefinition = UpdateFunctionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/greengrass/definition/functions/" Core.<>
                             Core.toText functionDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateFunctionDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFunctionDefinitionResponse' smart constructor.
newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFunctionDefinitionResponse' value with any optional fields omitted.
mkUpdateFunctionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFunctionDefinitionResponse
mkUpdateFunctionDefinitionResponse responseStatus
  = UpdateFunctionDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufdrrsResponseStatus :: Lens.Lens' UpdateFunctionDefinitionResponse Core.Int
ufdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ufdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

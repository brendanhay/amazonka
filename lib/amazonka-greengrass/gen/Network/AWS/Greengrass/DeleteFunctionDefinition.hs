{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function definition.
module Network.AWS.Greengrass.DeleteFunctionDefinition
    (
    -- * Creating a request
      DeleteFunctionDefinition (..)
    , mkDeleteFunctionDefinition
    -- ** Request lenses
    , dfdFunctionDefinitionId

    -- * Destructuring the response
    , DeleteFunctionDefinitionResponse (..)
    , mkDeleteFunctionDefinitionResponse
    -- ** Response lenses
    , dfdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunctionDefinition' smart constructor.
newtype DeleteFunctionDefinition = DeleteFunctionDefinition'
  { functionDefinitionId :: Core.Text
    -- ^ The ID of the Lambda function definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionDefinition' value with any optional fields omitted.
mkDeleteFunctionDefinition
    :: Core.Text -- ^ 'functionDefinitionId'
    -> DeleteFunctionDefinition
mkDeleteFunctionDefinition functionDefinitionId
  = DeleteFunctionDefinition'{functionDefinitionId}

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFunctionDefinitionId :: Lens.Lens' DeleteFunctionDefinition Core.Text
dfdFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# INLINEABLE dfdFunctionDefinitionId #-}
{-# DEPRECATED functionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead"  #-}

instance Core.ToQuery DeleteFunctionDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFunctionDefinition where
        toHeaders DeleteFunctionDefinition{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteFunctionDefinition where
        type Rs DeleteFunctionDefinition = DeleteFunctionDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/greengrass/definition/functions/" Core.<>
                             Core.toText functionDefinitionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteFunctionDefinitionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFunctionDefinitionResponse' smart constructor.
newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionDefinitionResponse' value with any optional fields omitted.
mkDeleteFunctionDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFunctionDefinitionResponse
mkDeleteFunctionDefinitionResponse responseStatus
  = DeleteFunctionDefinitionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrrsResponseStatus :: Lens.Lens' DeleteFunctionDefinitionResponse Core.Int
dfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

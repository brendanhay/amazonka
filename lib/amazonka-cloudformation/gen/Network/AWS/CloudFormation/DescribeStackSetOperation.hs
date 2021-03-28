{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation. 
module Network.AWS.CloudFormation.DescribeStackSetOperation
    (
    -- * Creating a request
      DescribeStackSetOperation (..)
    , mkDescribeStackSetOperation
    -- ** Request lenses
    , dssoStackSetName
    , dssoOperationId

    -- * Destructuring the response
    , DescribeStackSetOperationResponse (..)
    , mkDescribeStackSetOperationResponse
    -- ** Response lenses
    , dssorrsStackSetOperation
    , dssorrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackSetOperation' smart constructor.
data DescribeStackSetOperation = DescribeStackSetOperation'
  { stackSetName :: Types.StackSetName
    -- ^ The name or the unique stack ID of the stack set for the stack operation.
  , operationId :: Types.OperationId
    -- ^ The unique ID of the stack set operation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackSetOperation' value with any optional fields omitted.
mkDescribeStackSetOperation
    :: Types.StackSetName -- ^ 'stackSetName'
    -> Types.OperationId -- ^ 'operationId'
    -> DescribeStackSetOperation
mkDescribeStackSetOperation stackSetName operationId
  = DescribeStackSetOperation'{stackSetName, operationId}

-- | The name or the unique stack ID of the stack set for the stack operation.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoStackSetName :: Lens.Lens' DescribeStackSetOperation Types.StackSetName
dssoStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE dssoStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | The unique ID of the stack set operation. 
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoOperationId :: Lens.Lens' DescribeStackSetOperation Types.OperationId
dssoOperationId = Lens.field @"operationId"
{-# INLINEABLE dssoOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

instance Core.ToQuery DescribeStackSetOperation where
        toQuery DescribeStackSetOperation{..}
          = Core.toQueryPair "Action"
              ("DescribeStackSetOperation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackSetName" stackSetName
              Core.<> Core.toQueryPair "OperationId" operationId

instance Core.ToHeaders DescribeStackSetOperation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeStackSetOperation where
        type Rs DescribeStackSetOperation =
             DescribeStackSetOperationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeStackSetOperationResult"
              (\ s h x ->
                 DescribeStackSetOperationResponse' Core.<$>
                   (x Core..@? "StackSetOperation") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { stackSetOperation :: Core.Maybe Types.StackSetOperation
    -- ^ The specified stack set operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStackSetOperationResponse' value with any optional fields omitted.
mkDescribeStackSetOperationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStackSetOperationResponse
mkDescribeStackSetOperationResponse responseStatus
  = DescribeStackSetOperationResponse'{stackSetOperation =
                                         Core.Nothing,
                                       responseStatus}

-- | The specified stack set operation.
--
-- /Note:/ Consider using 'stackSetOperation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorrsStackSetOperation :: Lens.Lens' DescribeStackSetOperationResponse (Core.Maybe Types.StackSetOperation)
dssorrsStackSetOperation = Lens.field @"stackSetOperation"
{-# INLINEABLE dssorrsStackSetOperation #-}
{-# DEPRECATED stackSetOperation "Use generic-lens or generic-optics with 'stackSetOperation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorrsResponseStatus :: Lens.Lens' DescribeStackSetOperationResponse Core.Int
dssorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dssorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a concurrent execution limit from a function.
module Network.AWS.Lambda.DeleteFunctionConcurrency
    (
    -- * Creating a request
      DeleteFunctionConcurrency (..)
    , mkDeleteFunctionConcurrency
    -- ** Request lenses
    , dfcFunctionName

    -- * Destructuring the response
    , DeleteFunctionConcurrencyResponse (..)
    , mkDeleteFunctionConcurrencyResponse
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunctionConcurrency' smart constructor.
newtype DeleteFunctionConcurrency = DeleteFunctionConcurrency'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionConcurrency' value with any optional fields omitted.
mkDeleteFunctionConcurrency
    :: Types.FunctionName -- ^ 'functionName'
    -> DeleteFunctionConcurrency
mkDeleteFunctionConcurrency functionName
  = DeleteFunctionConcurrency'{functionName}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFunctionName :: Lens.Lens' DeleteFunctionConcurrency Types.FunctionName
dfcFunctionName = Lens.field @"functionName"
{-# INLINEABLE dfcFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

instance Core.ToQuery DeleteFunctionConcurrency where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFunctionConcurrency where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteFunctionConcurrency where
        type Rs DeleteFunctionConcurrency =
             DeleteFunctionConcurrencyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2017-10-31/functions/" Core.<> Core.toText functionName Core.<>
                             "/concurrency",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteFunctionConcurrencyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFunctionConcurrencyResponse' smart constructor.
data DeleteFunctionConcurrencyResponse = DeleteFunctionConcurrencyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionConcurrencyResponse' value with any optional fields omitted.
mkDeleteFunctionConcurrencyResponse
    :: DeleteFunctionConcurrencyResponse
mkDeleteFunctionConcurrencyResponse
  = DeleteFunctionConcurrencyResponse'

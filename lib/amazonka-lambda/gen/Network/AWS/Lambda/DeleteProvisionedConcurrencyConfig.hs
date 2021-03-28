{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the provisioned concurrency configuration for a function.
module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
    (
    -- * Creating a request
      DeleteProvisionedConcurrencyConfig (..)
    , mkDeleteProvisionedConcurrencyConfig
    -- ** Request lenses
    , dpccFunctionName
    , dpccQualifier

    -- * Destructuring the response
    , DeleteProvisionedConcurrencyConfigResponse (..)
    , mkDeleteProvisionedConcurrencyConfigResponse
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProvisionedConcurrencyConfig' smart constructor.
data DeleteProvisionedConcurrencyConfig = DeleteProvisionedConcurrencyConfig'
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
  , qualifier :: Types.Qualifier
    -- ^ The version number or alias name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisionedConcurrencyConfig' value with any optional fields omitted.
mkDeleteProvisionedConcurrencyConfig
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Qualifier -- ^ 'qualifier'
    -> DeleteProvisionedConcurrencyConfig
mkDeleteProvisionedConcurrencyConfig functionName qualifier
  = DeleteProvisionedConcurrencyConfig'{functionName, qualifier}

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
dpccFunctionName :: Lens.Lens' DeleteProvisionedConcurrencyConfig Types.FunctionName
dpccFunctionName = Lens.field @"functionName"
{-# INLINEABLE dpccFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpccQualifier :: Lens.Lens' DeleteProvisionedConcurrencyConfig Types.Qualifier
dpccQualifier = Lens.field @"qualifier"
{-# INLINEABLE dpccQualifier #-}
{-# DEPRECATED qualifier "Use generic-lens or generic-optics with 'qualifier' instead"  #-}

instance Core.ToQuery DeleteProvisionedConcurrencyConfig where
        toQuery DeleteProvisionedConcurrencyConfig{..}
          = Core.toQueryPair "Qualifier" qualifier

instance Core.ToHeaders DeleteProvisionedConcurrencyConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteProvisionedConcurrencyConfig where
        type Rs DeleteProvisionedConcurrencyConfig =
             DeleteProvisionedConcurrencyConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2019-09-30/functions/" Core.<> Core.toText functionName Core.<>
                             "/provisioned-concurrency",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteProvisionedConcurrencyConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProvisionedConcurrencyConfigResponse' smart constructor.
data DeleteProvisionedConcurrencyConfigResponse = DeleteProvisionedConcurrencyConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisionedConcurrencyConfigResponse' value with any optional fields omitted.
mkDeleteProvisionedConcurrencyConfigResponse
    :: DeleteProvisionedConcurrencyConfigResponse
mkDeleteProvisionedConcurrencyConfigResponse
  = DeleteProvisionedConcurrencyConfigResponse'

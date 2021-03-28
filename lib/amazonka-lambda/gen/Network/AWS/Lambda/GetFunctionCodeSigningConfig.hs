{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the code signing configuration for the specified function.
module Network.AWS.Lambda.GetFunctionCodeSigningConfig
    (
    -- * Creating a request
      GetFunctionCodeSigningConfig (..)
    , mkGetFunctionCodeSigningConfig
    -- ** Request lenses
    , gfcscFunctionName

    -- * Destructuring the response
    , GetFunctionCodeSigningConfigResponse (..)
    , mkGetFunctionCodeSigningConfigResponse
    -- ** Response lenses
    , gfcscrrsCodeSigningConfigArn
    , gfcscrrsFunctionName
    , gfcscrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunctionCodeSigningConfig' smart constructor.
newtype GetFunctionCodeSigningConfig = GetFunctionCodeSigningConfig'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionCodeSigningConfig' value with any optional fields omitted.
mkGetFunctionCodeSigningConfig
    :: Types.FunctionName -- ^ 'functionName'
    -> GetFunctionCodeSigningConfig
mkGetFunctionCodeSigningConfig functionName
  = GetFunctionCodeSigningConfig'{functionName}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscFunctionName :: Lens.Lens' GetFunctionCodeSigningConfig Types.FunctionName
gfcscFunctionName = Lens.field @"functionName"
{-# INLINEABLE gfcscFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

instance Core.ToQuery GetFunctionCodeSigningConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFunctionCodeSigningConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetFunctionCodeSigningConfig where
        type Rs GetFunctionCodeSigningConfig =
             GetFunctionCodeSigningConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-06-30/functions/" Core.<> Core.toText functionName Core.<>
                             "/code-signing-config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFunctionCodeSigningConfigResponse' Core.<$>
                   (x Core..: "CodeSigningConfigArn") Core.<*>
                     x Core..: "FunctionName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFunctionCodeSigningConfigResponse' smart constructor.
data GetFunctionCodeSigningConfigResponse = GetFunctionCodeSigningConfigResponse'
  { codeSigningConfigArn :: Types.CodeSigningConfigArn
    -- ^ The The Amazon Resource Name (ARN) of the code signing configuration.
  , functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionCodeSigningConfigResponse' value with any optional fields omitted.
mkGetFunctionCodeSigningConfigResponse
    :: Types.CodeSigningConfigArn -- ^ 'codeSigningConfigArn'
    -> Types.FunctionName -- ^ 'functionName'
    -> Core.Int -- ^ 'responseStatus'
    -> GetFunctionCodeSigningConfigResponse
mkGetFunctionCodeSigningConfigResponse codeSigningConfigArn
  functionName responseStatus
  = GetFunctionCodeSigningConfigResponse'{codeSigningConfigArn,
                                          functionName, responseStatus}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscrrsCodeSigningConfigArn :: Lens.Lens' GetFunctionCodeSigningConfigResponse Types.CodeSigningConfigArn
gfcscrrsCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# INLINEABLE gfcscrrsCodeSigningConfigArn #-}
{-# DEPRECATED codeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead"  #-}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscrrsFunctionName :: Lens.Lens' GetFunctionCodeSigningConfigResponse Types.FunctionName
gfcscrrsFunctionName = Lens.field @"functionName"
{-# INLINEABLE gfcscrrsFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscrrsResponseStatus :: Lens.Lens' GetFunctionCodeSigningConfigResponse Core.Int
gfcscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfcscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

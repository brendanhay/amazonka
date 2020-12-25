{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PutFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the code signing configuration for the function. Changes to the code signing configuration take effect the next time a user tries to deploy a code package to the function.
module Network.AWS.Lambda.PutFunctionCodeSigningConfig
  ( -- * Creating a request
    PutFunctionCodeSigningConfig (..),
    mkPutFunctionCodeSigningConfig,

    -- ** Request lenses
    pfcscCodeSigningConfigArn,
    pfcscFunctionName,

    -- * Destructuring the response
    PutFunctionCodeSigningConfigResponse (..),
    mkPutFunctionCodeSigningConfigResponse,

    -- ** Response lenses
    pfcscrrsCodeSigningConfigArn,
    pfcscrrsFunctionName,
    pfcscrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutFunctionCodeSigningConfig' smart constructor.
data PutFunctionCodeSigningConfig = PutFunctionCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Types.CodeSigningConfigArn,
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
    functionName :: Types.FunctionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFunctionCodeSigningConfig' value with any optional fields omitted.
mkPutFunctionCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Types.CodeSigningConfigArn ->
  -- | 'functionName'
  Types.FunctionName ->
  PutFunctionCodeSigningConfig
mkPutFunctionCodeSigningConfig codeSigningConfigArn functionName =
  PutFunctionCodeSigningConfig' {codeSigningConfigArn, functionName}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscCodeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfig Types.CodeSigningConfigArn
pfcscCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# DEPRECATED pfcscCodeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead." #-}

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
pfcscFunctionName :: Lens.Lens' PutFunctionCodeSigningConfig Types.FunctionName
pfcscFunctionName = Lens.field @"functionName"
{-# DEPRECATED pfcscFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Core.FromJSON PutFunctionCodeSigningConfig where
  toJSON PutFunctionCodeSigningConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CodeSigningConfigArn" Core..= codeSigningConfigArn)]
      )

instance Core.AWSRequest PutFunctionCodeSigningConfig where
  type
    Rs PutFunctionCodeSigningConfig =
      PutFunctionCodeSigningConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2020-06-30/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/code-signing-config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFunctionCodeSigningConfigResponse'
            Core.<$> (x Core..: "CodeSigningConfigArn")
            Core.<*> (x Core..: "FunctionName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutFunctionCodeSigningConfigResponse' smart constructor.
data PutFunctionCodeSigningConfigResponse = PutFunctionCodeSigningConfigResponse'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Types.CodeSigningConfigArn,
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
    functionName :: Types.FunctionName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFunctionCodeSigningConfigResponse' value with any optional fields omitted.
mkPutFunctionCodeSigningConfigResponse ::
  -- | 'codeSigningConfigArn'
  Types.CodeSigningConfigArn ->
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'responseStatus'
  Core.Int ->
  PutFunctionCodeSigningConfigResponse
mkPutFunctionCodeSigningConfigResponse
  codeSigningConfigArn
  functionName
  responseStatus =
    PutFunctionCodeSigningConfigResponse'
      { codeSigningConfigArn,
        functionName,
        responseStatus
      }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscrrsCodeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfigResponse Types.CodeSigningConfigArn
pfcscrrsCodeSigningConfigArn = Lens.field @"codeSigningConfigArn"
{-# DEPRECATED pfcscrrsCodeSigningConfigArn "Use generic-lens or generic-optics with 'codeSigningConfigArn' instead." #-}

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
pfcscrrsFunctionName :: Lens.Lens' PutFunctionCodeSigningConfigResponse Types.FunctionName
pfcscrrsFunctionName = Lens.field @"functionName"
{-# DEPRECATED pfcscrrsFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscrrsResponseStatus :: Lens.Lens' PutFunctionCodeSigningConfigResponse Core.Int
pfcscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pfcscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

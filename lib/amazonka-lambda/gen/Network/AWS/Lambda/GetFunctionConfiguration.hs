{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the version-specific settings of a Lambda function or version. The output includes only options that can vary between versions of a function. To modify these settings, use 'UpdateFunctionConfiguration' .
--
-- To get all of a function's details, including function-level settings, use 'GetFunction' .
module Network.AWS.Lambda.GetFunctionConfiguration
  ( -- * Creating a request
    GetFunctionConfiguration (..),
    mkGetFunctionConfiguration,

    -- ** Request lenses
    gfcFunctionName,
    gfcQualifier,

    -- * Destructuring the response
    Types.FunctionConfiguration (..),
    Types.mkFunctionConfiguration,

    -- ** Response lenses
    Types.fcCodeSha256,
    Types.fcCodeSize,
    Types.fcDeadLetterConfig,
    Types.fcDescription,
    Types.fcEnvironment,
    Types.fcFileSystemConfigs,
    Types.fcFunctionArn,
    Types.fcFunctionName,
    Types.fcHandler,
    Types.fcKMSKeyArn,
    Types.fcLastModified,
    Types.fcLastUpdateStatus,
    Types.fcLastUpdateStatusReason,
    Types.fcLastUpdateStatusReasonCode,
    Types.fcLayers,
    Types.fcMasterArn,
    Types.fcMemorySize,
    Types.fcRevisionId,
    Types.fcRole,
    Types.fcRuntime,
    Types.fcSigningJobArn,
    Types.fcSigningProfileVersionArn,
    Types.fcState,
    Types.fcStateReason,
    Types.fcStateReasonCode,
    Types.fcTimeout,
    Types.fcTracingConfig,
    Types.fcVersion,
    Types.fcVpcConfig,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunctionConfiguration' smart constructor.
data GetFunctionConfiguration = GetFunctionConfiguration'
  { -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.NamespacedFunctionName,
    -- | Specify a version or alias to get details about a published version of the function.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionConfiguration' value with any optional fields omitted.
mkGetFunctionConfiguration ::
  -- | 'functionName'
  Types.NamespacedFunctionName ->
  GetFunctionConfiguration
mkGetFunctionConfiguration functionName =
  GetFunctionConfiguration' {functionName, qualifier = Core.Nothing}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcFunctionName :: Lens.Lens' GetFunctionConfiguration Types.NamespacedFunctionName
gfcFunctionName = Lens.field @"functionName"
{-# DEPRECATED gfcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to get details about a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcQualifier :: Lens.Lens' GetFunctionConfiguration (Core.Maybe Types.Qualifier)
gfcQualifier = Lens.field @"qualifier"
{-# DEPRECATED gfcQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest GetFunctionConfiguration where
  type Rs GetFunctionConfiguration = Types.FunctionConfiguration
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/configuration")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

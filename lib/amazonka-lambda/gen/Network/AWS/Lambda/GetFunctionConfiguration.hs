{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gfcQualifier,
    gfcFunctionName,

    -- * Destructuring the response
    FunctionConfiguration (..),
    mkFunctionConfiguration,

    -- ** Response lenses
    fcMemorySize,
    fcRuntime,
    fcState,
    fcSigningProfileVersionARN,
    fcLastUpdateStatus,
    fcFunctionARN,
    fcKMSKeyARN,
    fcFileSystemConfigs,
    fcEnvironment,
    fcDeadLetterConfig,
    fcSigningJobARN,
    fcRole,
    fcVPCConfig,
    fcVersion,
    fcFunctionName,
    fcLayers,
    fcCodeSize,
    fcHandler,
    fcTimeout,
    fcLastUpdateStatusReason,
    fcStateReason,
    fcLastModified,
    fcCodeSha256,
    fcTracingConfig,
    fcStateReasonCode,
    fcDescription,
    fcLastUpdateStatusReasonCode,
    fcRevisionId,
    fcMasterARN,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionConfiguration' smart constructor.
data GetFunctionConfiguration = GetFunctionConfiguration'
  { qualifier ::
      Lude.Maybe Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function, version, or alias.
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
-- * 'qualifier' - Specify a version or alias to get details about a published version of the function.
mkGetFunctionConfiguration ::
  -- | 'functionName'
  Lude.Text ->
  GetFunctionConfiguration
mkGetFunctionConfiguration pFunctionName_ =
  GetFunctionConfiguration'
    { qualifier = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get details about a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcQualifier :: Lens.Lens' GetFunctionConfiguration (Lude.Maybe Lude.Text)
gfcQualifier = Lens.lens (qualifier :: GetFunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: GetFunctionConfiguration)
{-# DEPRECATED gfcQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

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
gfcFunctionName :: Lens.Lens' GetFunctionConfiguration Lude.Text
gfcFunctionName = Lens.lens (functionName :: GetFunctionConfiguration -> Lude.Text) (\s a -> s {functionName = a} :: GetFunctionConfiguration)
{-# DEPRECATED gfcFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetFunctionConfiguration where
  type Rs GetFunctionConfiguration = FunctionConfiguration
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetFunctionConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFunctionConfiguration where
  toPath GetFunctionConfiguration' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/configuration"
      ]

instance Lude.ToQuery GetFunctionConfiguration where
  toQuery GetFunctionConfiguration' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

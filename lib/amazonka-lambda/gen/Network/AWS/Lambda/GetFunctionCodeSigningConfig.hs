{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetFunctionCodeSigningConfig (..),
    mkGetFunctionCodeSigningConfig,

    -- ** Request lenses
    gfcscFunctionName,

    -- * Destructuring the response
    GetFunctionCodeSigningConfigResponse (..),
    mkGetFunctionCodeSigningConfigResponse,

    -- ** Response lenses
    gfcscrsResponseStatus,
    gfcscrsCodeSigningConfigARN,
    gfcscrsFunctionName,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionCodeSigningConfig' smart constructor.
newtype GetFunctionCodeSigningConfig = GetFunctionCodeSigningConfig'
  { functionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
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
mkGetFunctionCodeSigningConfig ::
  -- | 'functionName'
  Lude.Text ->
  GetFunctionCodeSigningConfig
mkGetFunctionCodeSigningConfig pFunctionName_ =
  GetFunctionCodeSigningConfig' {functionName = pFunctionName_}

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
gfcscFunctionName :: Lens.Lens' GetFunctionCodeSigningConfig Lude.Text
gfcscFunctionName = Lens.lens (functionName :: GetFunctionCodeSigningConfig -> Lude.Text) (\s a -> s {functionName = a} :: GetFunctionCodeSigningConfig)
{-# DEPRECATED gfcscFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetFunctionCodeSigningConfig where
  type
    Rs GetFunctionCodeSigningConfig =
      GetFunctionCodeSigningConfigResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionCodeSigningConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeSigningConfigArn")
            Lude.<*> (x Lude..:> "FunctionName")
      )

instance Lude.ToHeaders GetFunctionCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFunctionCodeSigningConfig where
  toPath GetFunctionCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-06-30/functions/",
        Lude.toBS functionName,
        "/code-signing-config"
      ]

instance Lude.ToQuery GetFunctionCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFunctionCodeSigningConfigResponse' smart constructor.
data GetFunctionCodeSigningConfigResponse = GetFunctionCodeSigningConfigResponse'
  { responseStatus ::
      Lude.Int,
    codeSigningConfigARN ::
      Lude.Text,
    functionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'codeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
-- * 'functionName' - The name of the Lambda function.
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
-- * 'responseStatus' - The response status code.
mkGetFunctionCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  GetFunctionCodeSigningConfigResponse
mkGetFunctionCodeSigningConfigResponse
  pResponseStatus_
  pCodeSigningConfigARN_
  pFunctionName_ =
    GetFunctionCodeSigningConfigResponse'
      { responseStatus =
          pResponseStatus_,
        codeSigningConfigARN = pCodeSigningConfigARN_,
        functionName = pFunctionName_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscrsResponseStatus :: Lens.Lens' GetFunctionCodeSigningConfigResponse Lude.Int
gfcscrsResponseStatus = Lens.lens (responseStatus :: GetFunctionCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionCodeSigningConfigResponse)
{-# DEPRECATED gfcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcscrsCodeSigningConfigARN :: Lens.Lens' GetFunctionCodeSigningConfigResponse Lude.Text
gfcscrsCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: GetFunctionCodeSigningConfigResponse -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: GetFunctionCodeSigningConfigResponse)
{-# DEPRECATED gfcscrsCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

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
gfcscrsFunctionName :: Lens.Lens' GetFunctionCodeSigningConfigResponse Lude.Text
gfcscrsFunctionName = Lens.lens (functionName :: GetFunctionCodeSigningConfigResponse -> Lude.Text) (\s a -> s {functionName = a} :: GetFunctionCodeSigningConfigResponse)
{-# DEPRECATED gfcscrsFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    pfcscCodeSigningConfigARN,
    pfcscFunctionName,

    -- * Destructuring the response
    PutFunctionCodeSigningConfigResponse (..),
    mkPutFunctionCodeSigningConfigResponse,

    -- ** Response lenses
    pfcscrsResponseStatus,
    pfcscrsCodeSigningConfigARN,
    pfcscrsFunctionName,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutFunctionCodeSigningConfig' smart constructor.
data PutFunctionCodeSigningConfig = PutFunctionCodeSigningConfig'
  { codeSigningConfigARN ::
      Lude.Text,
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

-- | Creates a value of 'PutFunctionCodeSigningConfig' with the minimum fields required to make a request.
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
mkPutFunctionCodeSigningConfig ::
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  PutFunctionCodeSigningConfig
mkPutFunctionCodeSigningConfig
  pCodeSigningConfigARN_
  pFunctionName_ =
    PutFunctionCodeSigningConfig'
      { codeSigningConfigARN =
          pCodeSigningConfigARN_,
        functionName = pFunctionName_
      }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscCodeSigningConfigARN :: Lens.Lens' PutFunctionCodeSigningConfig Lude.Text
pfcscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: PutFunctionCodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: PutFunctionCodeSigningConfig)
{-# DEPRECATED pfcscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

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
pfcscFunctionName :: Lens.Lens' PutFunctionCodeSigningConfig Lude.Text
pfcscFunctionName = Lens.lens (functionName :: PutFunctionCodeSigningConfig -> Lude.Text) (\s a -> s {functionName = a} :: PutFunctionCodeSigningConfig)
{-# DEPRECATED pfcscFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest PutFunctionCodeSigningConfig where
  type
    Rs PutFunctionCodeSigningConfig =
      PutFunctionCodeSigningConfigResponse
  request = Req.putJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutFunctionCodeSigningConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeSigningConfigArn")
            Lude.<*> (x Lude..:> "FunctionName")
      )

instance Lude.ToHeaders PutFunctionCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutFunctionCodeSigningConfig where
  toJSON PutFunctionCodeSigningConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CodeSigningConfigArn" Lude..= codeSigningConfigARN)]
      )

instance Lude.ToPath PutFunctionCodeSigningConfig where
  toPath PutFunctionCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-06-30/functions/",
        Lude.toBS functionName,
        "/code-signing-config"
      ]

instance Lude.ToQuery PutFunctionCodeSigningConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutFunctionCodeSigningConfigResponse' smart constructor.
data PutFunctionCodeSigningConfigResponse = PutFunctionCodeSigningConfigResponse'
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

-- | Creates a value of 'PutFunctionCodeSigningConfigResponse' with the minimum fields required to make a request.
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
mkPutFunctionCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  PutFunctionCodeSigningConfigResponse
mkPutFunctionCodeSigningConfigResponse
  pResponseStatus_
  pCodeSigningConfigARN_
  pFunctionName_ =
    PutFunctionCodeSigningConfigResponse'
      { responseStatus =
          pResponseStatus_,
        codeSigningConfigARN = pCodeSigningConfigARN_,
        functionName = pFunctionName_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscrsResponseStatus :: Lens.Lens' PutFunctionCodeSigningConfigResponse Lude.Int
pfcscrsResponseStatus = Lens.lens (responseStatus :: PutFunctionCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutFunctionCodeSigningConfigResponse)
{-# DEPRECATED pfcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfcscrsCodeSigningConfigARN :: Lens.Lens' PutFunctionCodeSigningConfigResponse Lude.Text
pfcscrsCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: PutFunctionCodeSigningConfigResponse -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: PutFunctionCodeSigningConfigResponse)
{-# DEPRECATED pfcscrsCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

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
pfcscrsFunctionName :: Lens.Lens' PutFunctionCodeSigningConfigResponse Lude.Text
pfcscrsFunctionName = Lens.lens (functionName :: PutFunctionCodeSigningConfigResponse -> Lude.Text) (\s a -> s {functionName = a} :: PutFunctionCodeSigningConfigResponse)
{-# DEPRECATED pfcscrsFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the function or function version, with a link to download the deployment package that's valid for 10 minutes. If you specify a function version, only details that are specific to that version are returned.
module Network.AWS.Lambda.GetFunction
  ( -- * Creating a request
    GetFunction (..),
    mkGetFunction,

    -- ** Request lenses
    gfFunctionName,
    gfQualifier,

    -- * Destructuring the response
    GetFunctionResponse (..),
    mkGetFunctionResponse,

    -- ** Response lenses
    gfrsConcurrency,
    gfrsCode,
    gfrsConfiguration,
    gfrsTags,
    gfrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunction' smart constructor.
data GetFunction = GetFunction'
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
    functionName :: Lude.Text,
    -- | Specify a version or alias to get details about a published version of the function.
    qualifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
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
mkGetFunction ::
  -- | 'functionName'
  Lude.Text ->
  GetFunction
mkGetFunction pFunctionName_ =
  GetFunction'
    { functionName = pFunctionName_,
      qualifier = Lude.Nothing
    }

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
gfFunctionName :: Lens.Lens' GetFunction Lude.Text
gfFunctionName = Lens.lens (functionName :: GetFunction -> Lude.Text) (\s a -> s {functionName = a} :: GetFunction)
{-# DEPRECATED gfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to get details about a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfQualifier :: Lens.Lens' GetFunction (Lude.Maybe Lude.Text)
gfQualifier = Lens.lens (qualifier :: GetFunction -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: GetFunction)
{-# DEPRECATED gfQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Lude.AWSRequest GetFunction where
  type Rs GetFunction = GetFunctionResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Lude.<$> (x Lude..?> "Concurrency")
            Lude.<*> (x Lude..?> "Code")
            Lude.<*> (x Lude..?> "Configuration")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFunction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFunction where
  toPath GetFunction' {..} =
    Lude.mconcat ["/2015-03-31/functions/", Lude.toBS functionName]

instance Lude.ToQuery GetFunction where
  toQuery GetFunction' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
    concurrency :: Lude.Maybe Concurrency,
    -- | The deployment package of the function or version.
    code :: Lude.Maybe FunctionCodeLocation,
    -- | The configuration of the function or version.
    configuration :: Lude.Maybe FunctionConfiguration,
    -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionResponse' with the minimum fields required to make a request.
--
-- * 'concurrency' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
-- * 'code' - The deployment package of the function or version.
-- * 'configuration' - The configuration of the function or version.
-- * 'tags' - The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
-- * 'responseStatus' - The response status code.
mkGetFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFunctionResponse
mkGetFunctionResponse pResponseStatus_ =
  GetFunctionResponse'
    { concurrency = Lude.Nothing,
      code = Lude.Nothing,
      configuration = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
--
-- /Note:/ Consider using 'concurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsConcurrency :: Lens.Lens' GetFunctionResponse (Lude.Maybe Concurrency)
gfrsConcurrency = Lens.lens (concurrency :: GetFunctionResponse -> Lude.Maybe Concurrency) (\s a -> s {concurrency = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsConcurrency "Use generic-lens or generic-optics with 'concurrency' instead." #-}

-- | The deployment package of the function or version.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsCode :: Lens.Lens' GetFunctionResponse (Lude.Maybe FunctionCodeLocation)
gfrsCode = Lens.lens (code :: GetFunctionResponse -> Lude.Maybe FunctionCodeLocation) (\s a -> s {code = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The configuration of the function or version.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsConfiguration :: Lens.Lens' GetFunctionResponse (Lude.Maybe FunctionConfiguration)
gfrsConfiguration = Lens.lens (configuration :: GetFunctionResponse -> Lude.Maybe FunctionConfiguration) (\s a -> s {configuration = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsTags :: Lens.Lens' GetFunctionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gfrsTags = Lens.lens (tags :: GetFunctionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFunctionResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFunctionResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

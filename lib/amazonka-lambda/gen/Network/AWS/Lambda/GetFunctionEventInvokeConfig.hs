{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration for asynchronous invocation for a function, version, or alias.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.GetFunctionEventInvokeConfig
  ( -- * Creating a request
    GetFunctionEventInvokeConfig (..),
    mkGetFunctionEventInvokeConfig,

    -- ** Request lenses
    gfeicFunctionName,
    gfeicQualifier,

    -- * Destructuring the response
    FunctionEventInvokeConfig (..),
    mkFunctionEventInvokeConfig,

    -- ** Response lenses
    feicFunctionARN,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
    feicLastModified,
    feicDestinationConfig,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFunctionEventInvokeConfig' smart constructor.
data GetFunctionEventInvokeConfig = GetFunctionEventInvokeConfig'
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
    -- | A version number or alias name.
    qualifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFunctionEventInvokeConfig' with the minimum fields required to make a request.
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
-- * 'qualifier' - A version number or alias name.
mkGetFunctionEventInvokeConfig ::
  -- | 'functionName'
  Lude.Text ->
  GetFunctionEventInvokeConfig
mkGetFunctionEventInvokeConfig pFunctionName_ =
  GetFunctionEventInvokeConfig'
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
gfeicFunctionName :: Lens.Lens' GetFunctionEventInvokeConfig Lude.Text
gfeicFunctionName = Lens.lens (functionName :: GetFunctionEventInvokeConfig -> Lude.Text) (\s a -> s {functionName = a} :: GetFunctionEventInvokeConfig)
{-# DEPRECATED gfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfeicQualifier :: Lens.Lens' GetFunctionEventInvokeConfig (Lude.Maybe Lude.Text)
gfeicQualifier = Lens.lens (qualifier :: GetFunctionEventInvokeConfig -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: GetFunctionEventInvokeConfig)
{-# DEPRECATED gfeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Lude.AWSRequest GetFunctionEventInvokeConfig where
  type Rs GetFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetFunctionEventInvokeConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetFunctionEventInvokeConfig where
  toPath GetFunctionEventInvokeConfig' {..} =
    Lude.mconcat
      [ "/2019-09-25/functions/",
        Lude.toBS functionName,
        "/event-invoke-config"
      ]

instance Lude.ToQuery GetFunctionEventInvokeConfig where
  toQuery GetFunctionEventInvokeConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

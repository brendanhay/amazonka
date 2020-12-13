{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for asynchronous invocation for a function, version, or alias.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
  ( -- * Creating a request
    UpdateFunctionEventInvokeConfig (..),
    mkUpdateFunctionEventInvokeConfig,

    -- ** Request lenses
    ufeicMaximumEventAgeInSeconds,
    ufeicMaximumRetryAttempts,
    ufeicFunctionName,
    ufeicQualifier,
    ufeicDestinationConfig,

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

-- | /See:/ 'mkUpdateFunctionEventInvokeConfig' smart constructor.
data UpdateFunctionEventInvokeConfig = UpdateFunctionEventInvokeConfig'
  { -- | The maximum age of a request that Lambda sends to a function for processing.
    maximumEventAgeInSeconds :: Lude.Maybe Lude.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Lude.Maybe Lude.Natural,
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
    functionName :: Lude.Text,
    -- | A version number or alias name.
    qualifier :: Lude.Maybe Lude.Text,
    -- | A destination for events after they have been sent to a function for processing.
    --
    -- __Destinations__
    --
    --     * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
    --
    --
    --     * __Queue__ - The ARN of an SQS queue.
    --
    --
    --     * __Topic__ - The ARN of an SNS topic.
    --
    --
    --     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
    destinationConfig :: Lude.Maybe DestinationConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- * 'maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for processing.
-- * 'maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
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
-- * 'destinationConfig' - A destination for events after they have been sent to a function for processing.
--
-- __Destinations__
--
--     * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
--
--     * __Queue__ - The ARN of an SQS queue.
--
--
--     * __Topic__ - The ARN of an SNS topic.
--
--
--     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
mkUpdateFunctionEventInvokeConfig ::
  -- | 'functionName'
  Lude.Text ->
  UpdateFunctionEventInvokeConfig
mkUpdateFunctionEventInvokeConfig pFunctionName_ =
  UpdateFunctionEventInvokeConfig'
    { maximumEventAgeInSeconds =
        Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      functionName = pFunctionName_,
      qualifier = Lude.Nothing,
      destinationConfig = Lude.Nothing
    }

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicMaximumEventAgeInSeconds :: Lens.Lens' UpdateFunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
ufeicMaximumEventAgeInSeconds = Lens.lens (maximumEventAgeInSeconds :: UpdateFunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumEventAgeInSeconds = a} :: UpdateFunctionEventInvokeConfig)
{-# DEPRECATED ufeicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicMaximumRetryAttempts :: Lens.Lens' UpdateFunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
ufeicMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: UpdateFunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumRetryAttempts = a} :: UpdateFunctionEventInvokeConfig)
{-# DEPRECATED ufeicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

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
ufeicFunctionName :: Lens.Lens' UpdateFunctionEventInvokeConfig Lude.Text
ufeicFunctionName = Lens.lens (functionName :: UpdateFunctionEventInvokeConfig -> Lude.Text) (\s a -> s {functionName = a} :: UpdateFunctionEventInvokeConfig)
{-# DEPRECATED ufeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicQualifier :: Lens.Lens' UpdateFunctionEventInvokeConfig (Lude.Maybe Lude.Text)
ufeicQualifier = Lens.lens (qualifier :: UpdateFunctionEventInvokeConfig -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: UpdateFunctionEventInvokeConfig)
{-# DEPRECATED ufeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | A destination for events after they have been sent to a function for processing.
--
-- __Destinations__
--
--     * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
--
--     * __Queue__ - The ARN of an SQS queue.
--
--
--     * __Topic__ - The ARN of an SNS topic.
--
--
--     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
--
--
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicDestinationConfig :: Lens.Lens' UpdateFunctionEventInvokeConfig (Lude.Maybe DestinationConfig)
ufeicDestinationConfig = Lens.lens (destinationConfig :: UpdateFunctionEventInvokeConfig -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: UpdateFunctionEventInvokeConfig)
{-# DEPRECATED ufeicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

instance Lude.AWSRequest UpdateFunctionEventInvokeConfig where
  type Rs UpdateFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = Req.postJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateFunctionEventInvokeConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateFunctionEventInvokeConfig where
  toJSON UpdateFunctionEventInvokeConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumEventAgeInSeconds" Lude..=)
              Lude.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Lude..=) Lude.<$> maximumRetryAttempts,
            ("DestinationConfig" Lude..=) Lude.<$> destinationConfig
          ]
      )

instance Lude.ToPath UpdateFunctionEventInvokeConfig where
  toPath UpdateFunctionEventInvokeConfig' {..} =
    Lude.mconcat
      [ "/2019-09-25/functions/",
        Lude.toBS functionName,
        "/event-invoke-config"
      ]

instance Lude.ToQuery UpdateFunctionEventInvokeConfig where
  toQuery UpdateFunctionEventInvokeConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

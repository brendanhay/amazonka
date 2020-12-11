{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PutFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures options for <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html asynchronous invocation> on a function, version, or alias. If a configuration already exists for a function, version, or alias, this operation overwrites it. If you exclude any settings, they are removed. To set one option without affecting existing settings for other options, use 'UpdateFunctionEventInvokeConfig' .
--
-- By default, Lambda retries an asynchronous invocation twice if the function returns an error. It retains events in a queue for up to six hours. When an event fails all processing attempts or stays in the asynchronous invocation queue for too long, Lambda discards it. To retain discarded events, configure a dead-letter queue with 'UpdateFunctionConfiguration' .
-- To send an invocation record to a queue, topic, function, or event bus, specify a <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-async-destinations destination> . You can configure separate destinations for successful invocations (on-success) and events that fail all processing attempts (on-failure). You can configure destinations in addition to or instead of a dead-letter queue.
module Network.AWS.Lambda.PutFunctionEventInvokeConfig
  ( -- * Creating a request
    PutFunctionEventInvokeConfig (..),
    mkPutFunctionEventInvokeConfig,

    -- ** Request lenses
    pfeicMaximumEventAgeInSeconds,
    pfeicMaximumRetryAttempts,
    pfeicQualifier,
    pfeicDestinationConfig,
    pfeicFunctionName,

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

-- | /See:/ 'mkPutFunctionEventInvokeConfig' smart constructor.
data PutFunctionEventInvokeConfig = PutFunctionEventInvokeConfig'
  { maximumEventAgeInSeconds ::
      Lude.Maybe Lude.Natural,
    maximumRetryAttempts ::
      Lude.Maybe Lude.Natural,
    qualifier :: Lude.Maybe Lude.Text,
    destinationConfig ::
      Lude.Maybe DestinationConfig,
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

-- | Creates a value of 'PutFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
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
--
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
-- * 'maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for processing.
-- * 'maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
-- * 'qualifier' - A version number or alias name.
mkPutFunctionEventInvokeConfig ::
  -- | 'functionName'
  Lude.Text ->
  PutFunctionEventInvokeConfig
mkPutFunctionEventInvokeConfig pFunctionName_ =
  PutFunctionEventInvokeConfig'
    { maximumEventAgeInSeconds =
        Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      qualifier = Lude.Nothing,
      destinationConfig = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicMaximumEventAgeInSeconds :: Lens.Lens' PutFunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
pfeicMaximumEventAgeInSeconds = Lens.lens (maximumEventAgeInSeconds :: PutFunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumEventAgeInSeconds = a} :: PutFunctionEventInvokeConfig)
{-# DEPRECATED pfeicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicMaximumRetryAttempts :: Lens.Lens' PutFunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
pfeicMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: PutFunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumRetryAttempts = a} :: PutFunctionEventInvokeConfig)
{-# DEPRECATED pfeicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicQualifier :: Lens.Lens' PutFunctionEventInvokeConfig (Lude.Maybe Lude.Text)
pfeicQualifier = Lens.lens (qualifier :: PutFunctionEventInvokeConfig -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: PutFunctionEventInvokeConfig)
{-# DEPRECATED pfeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

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
pfeicDestinationConfig :: Lens.Lens' PutFunctionEventInvokeConfig (Lude.Maybe DestinationConfig)
pfeicDestinationConfig = Lens.lens (destinationConfig :: PutFunctionEventInvokeConfig -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: PutFunctionEventInvokeConfig)
{-# DEPRECATED pfeicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

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
pfeicFunctionName :: Lens.Lens' PutFunctionEventInvokeConfig Lude.Text
pfeicFunctionName = Lens.lens (functionName :: PutFunctionEventInvokeConfig -> Lude.Text) (\s a -> s {functionName = a} :: PutFunctionEventInvokeConfig)
{-# DEPRECATED pfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest PutFunctionEventInvokeConfig where
  type Rs PutFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutFunctionEventInvokeConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutFunctionEventInvokeConfig where
  toJSON PutFunctionEventInvokeConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumEventAgeInSeconds" Lude..=)
              Lude.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Lude..=) Lude.<$> maximumRetryAttempts,
            ("DestinationConfig" Lude..=) Lude.<$> destinationConfig
          ]
      )

instance Lude.ToPath PutFunctionEventInvokeConfig where
  toPath PutFunctionEventInvokeConfig' {..} =
    Lude.mconcat
      [ "/2019-09-25/functions/",
        Lude.toBS functionName,
        "/event-invoke-config"
      ]

instance Lude.ToQuery PutFunctionEventInvokeConfig where
  toQuery PutFunctionEventInvokeConfig' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

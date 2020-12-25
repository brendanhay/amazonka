{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    pfeicFunctionName,
    pfeicDestinationConfig,
    pfeicMaximumEventAgeInSeconds,
    pfeicMaximumRetryAttempts,
    pfeicQualifier,

    -- * Destructuring the response
    Types.FunctionEventInvokeConfig (..),
    Types.mkFunctionEventInvokeConfig,

    -- ** Response lenses
    Types.feicDestinationConfig,
    Types.feicFunctionArn,
    Types.feicLastModified,
    Types.feicMaximumEventAgeInSeconds,
    Types.feicMaximumRetryAttempts,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutFunctionEventInvokeConfig' smart constructor.
data PutFunctionEventInvokeConfig = PutFunctionEventInvokeConfig'
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
    functionName :: Types.FunctionName,
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
    destinationConfig :: Core.Maybe Types.DestinationConfig,
    -- | The maximum age of a request that Lambda sends to a function for processing.
    maximumEventAgeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Core.Maybe Core.Natural,
    -- | A version number or alias name.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFunctionEventInvokeConfig' value with any optional fields omitted.
mkPutFunctionEventInvokeConfig ::
  -- | 'functionName'
  Types.FunctionName ->
  PutFunctionEventInvokeConfig
mkPutFunctionEventInvokeConfig functionName =
  PutFunctionEventInvokeConfig'
    { functionName,
      destinationConfig = Core.Nothing,
      maximumEventAgeInSeconds = Core.Nothing,
      maximumRetryAttempts = Core.Nothing,
      qualifier = Core.Nothing
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
pfeicFunctionName :: Lens.Lens' PutFunctionEventInvokeConfig Types.FunctionName
pfeicFunctionName = Lens.field @"functionName"
{-# DEPRECATED pfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

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
pfeicDestinationConfig :: Lens.Lens' PutFunctionEventInvokeConfig (Core.Maybe Types.DestinationConfig)
pfeicDestinationConfig = Lens.field @"destinationConfig"
{-# DEPRECATED pfeicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicMaximumEventAgeInSeconds :: Lens.Lens' PutFunctionEventInvokeConfig (Core.Maybe Core.Natural)
pfeicMaximumEventAgeInSeconds = Lens.field @"maximumEventAgeInSeconds"
{-# DEPRECATED pfeicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicMaximumRetryAttempts :: Lens.Lens' PutFunctionEventInvokeConfig (Core.Maybe Core.Natural)
pfeicMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# DEPRECATED pfeicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeicQualifier :: Lens.Lens' PutFunctionEventInvokeConfig (Core.Maybe Types.Qualifier)
pfeicQualifier = Lens.field @"qualifier"
{-# DEPRECATED pfeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.FromJSON PutFunctionEventInvokeConfig where
  toJSON PutFunctionEventInvokeConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("DestinationConfig" Core..=) Core.<$> destinationConfig,
            ("MaximumEventAgeInSeconds" Core..=)
              Core.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Core..=) Core.<$> maximumRetryAttempts
          ]
      )

instance Core.AWSRequest PutFunctionEventInvokeConfig where
  type
    Rs PutFunctionEventInvokeConfig =
      Types.FunctionEventInvokeConfig
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-25/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/event-invoke-config")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)

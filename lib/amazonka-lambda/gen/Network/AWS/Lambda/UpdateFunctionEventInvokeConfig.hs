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
    ufeicFunctionName,
    ufeicDestinationConfig,
    ufeicMaximumEventAgeInSeconds,
    ufeicMaximumRetryAttempts,
    ufeicQualifier,

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

-- | /See:/ 'mkUpdateFunctionEventInvokeConfig' smart constructor.
data UpdateFunctionEventInvokeConfig = UpdateFunctionEventInvokeConfig'
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

-- | Creates a 'UpdateFunctionEventInvokeConfig' value with any optional fields omitted.
mkUpdateFunctionEventInvokeConfig ::
  -- | 'functionName'
  Types.FunctionName ->
  UpdateFunctionEventInvokeConfig
mkUpdateFunctionEventInvokeConfig functionName =
  UpdateFunctionEventInvokeConfig'
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
ufeicFunctionName :: Lens.Lens' UpdateFunctionEventInvokeConfig Types.FunctionName
ufeicFunctionName = Lens.field @"functionName"
{-# DEPRECATED ufeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

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
ufeicDestinationConfig :: Lens.Lens' UpdateFunctionEventInvokeConfig (Core.Maybe Types.DestinationConfig)
ufeicDestinationConfig = Lens.field @"destinationConfig"
{-# DEPRECATED ufeicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicMaximumEventAgeInSeconds :: Lens.Lens' UpdateFunctionEventInvokeConfig (Core.Maybe Core.Natural)
ufeicMaximumEventAgeInSeconds = Lens.field @"maximumEventAgeInSeconds"
{-# DEPRECATED ufeicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicMaximumRetryAttempts :: Lens.Lens' UpdateFunctionEventInvokeConfig (Core.Maybe Core.Natural)
ufeicMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# DEPRECATED ufeicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufeicQualifier :: Lens.Lens' UpdateFunctionEventInvokeConfig (Core.Maybe Types.Qualifier)
ufeicQualifier = Lens.field @"qualifier"
{-# DEPRECATED ufeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.FromJSON UpdateFunctionEventInvokeConfig where
  toJSON UpdateFunctionEventInvokeConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("DestinationConfig" Core..=) Core.<$> destinationConfig,
            ("MaximumEventAgeInSeconds" Core..=)
              Core.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Core..=) Core.<$> maximumRetryAttempts
          ]
      )

instance Core.AWSRequest UpdateFunctionEventInvokeConfig where
  type
    Rs UpdateFunctionEventInvokeConfig =
      Types.FunctionEventInvokeConfig
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionEventInvokeConfig
  ( FunctionEventInvokeConfig (..),

    -- * Smart constructor
    mkFunctionEventInvokeConfig,

    -- * Lenses
    feicDestinationConfig,
    feicFunctionArn,
    feicLastModified,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
  )
where

import qualified Network.AWS.Lambda.Types.DestinationConfig as Types
import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkFunctionEventInvokeConfig' smart constructor.
data FunctionEventInvokeConfig = FunctionEventInvokeConfig'
  { -- | A destination for events after they have been sent to a function for processing.
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
    -- | The Amazon Resource Name (ARN) of the function.
    functionArn :: Core.Maybe Types.FunctionArn,
    -- | The date and time that the configuration was last updated.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum age of a request that Lambda sends to a function for processing.
    maximumEventAgeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FunctionEventInvokeConfig' value with any optional fields omitted.
mkFunctionEventInvokeConfig ::
  FunctionEventInvokeConfig
mkFunctionEventInvokeConfig =
  FunctionEventInvokeConfig'
    { destinationConfig = Core.Nothing,
      functionArn = Core.Nothing,
      lastModified = Core.Nothing,
      maximumEventAgeInSeconds = Core.Nothing,
      maximumRetryAttempts = Core.Nothing
    }

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
feicDestinationConfig :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Types.DestinationConfig)
feicDestinationConfig = Lens.field @"destinationConfig"
{-# DEPRECATED feicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the function.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicFunctionArn :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Types.FunctionArn)
feicFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED feicFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | The date and time that the configuration was last updated.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicLastModified :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.NominalDiffTime)
feicLastModified = Lens.field @"lastModified"
{-# DEPRECATED feicLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicMaximumEventAgeInSeconds :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.Natural)
feicMaximumEventAgeInSeconds = Lens.field @"maximumEventAgeInSeconds"
{-# DEPRECATED feicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicMaximumRetryAttempts :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.Natural)
feicMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# DEPRECATED feicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

instance Core.FromJSON FunctionEventInvokeConfig where
  parseJSON =
    Core.withObject "FunctionEventInvokeConfig" Core.$
      \x ->
        FunctionEventInvokeConfig'
          Core.<$> (x Core..:? "DestinationConfig")
          Core.<*> (x Core..:? "FunctionArn")
          Core.<*> (x Core..:? "LastModified")
          Core.<*> (x Core..:? "MaximumEventAgeInSeconds")
          Core.<*> (x Core..:? "MaximumRetryAttempts")

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
    feicFunctionARN,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
    feicLastModified,
    feicDestinationConfig,
  )
where

import Network.AWS.Lambda.Types.DestinationConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkFunctionEventInvokeConfig' smart constructor.
data FunctionEventInvokeConfig = FunctionEventInvokeConfig'
  { functionARN ::
      Lude.Maybe Lude.Text,
    maximumEventAgeInSeconds ::
      Lude.Maybe Lude.Natural,
    maximumRetryAttempts ::
      Lude.Maybe Lude.Natural,
    lastModified ::
      Lude.Maybe Lude.Timestamp,
    destinationConfig ::
      Lude.Maybe DestinationConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionEventInvokeConfig' with the minimum fields required to make a request.
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
-- * 'functionARN' - The Amazon Resource Name (ARN) of the function.
-- * 'lastModified' - The date and time that the configuration was last updated.
-- * 'maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for processing.
-- * 'maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
mkFunctionEventInvokeConfig ::
  FunctionEventInvokeConfig
mkFunctionEventInvokeConfig =
  FunctionEventInvokeConfig'
    { functionARN = Lude.Nothing,
      maximumEventAgeInSeconds = Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      lastModified = Lude.Nothing,
      destinationConfig = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the function.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicFunctionARN :: Lens.Lens' FunctionEventInvokeConfig (Lude.Maybe Lude.Text)
feicFunctionARN = Lens.lens (functionARN :: FunctionEventInvokeConfig -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: FunctionEventInvokeConfig)
{-# DEPRECATED feicFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | The maximum age of a request that Lambda sends to a function for processing.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicMaximumEventAgeInSeconds :: Lens.Lens' FunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
feicMaximumEventAgeInSeconds = Lens.lens (maximumEventAgeInSeconds :: FunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumEventAgeInSeconds = a} :: FunctionEventInvokeConfig)
{-# DEPRECATED feicMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of times to retry when the function returns an error.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicMaximumRetryAttempts :: Lens.Lens' FunctionEventInvokeConfig (Lude.Maybe Lude.Natural)
feicMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: FunctionEventInvokeConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumRetryAttempts = a} :: FunctionEventInvokeConfig)
{-# DEPRECATED feicMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | The date and time that the configuration was last updated.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feicLastModified :: Lens.Lens' FunctionEventInvokeConfig (Lude.Maybe Lude.Timestamp)
feicLastModified = Lens.lens (lastModified :: FunctionEventInvokeConfig -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: FunctionEventInvokeConfig)
{-# DEPRECATED feicLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

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
feicDestinationConfig :: Lens.Lens' FunctionEventInvokeConfig (Lude.Maybe DestinationConfig)
feicDestinationConfig = Lens.lens (destinationConfig :: FunctionEventInvokeConfig -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: FunctionEventInvokeConfig)
{-# DEPRECATED feicDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

instance Lude.FromJSON FunctionEventInvokeConfig where
  parseJSON =
    Lude.withObject
      "FunctionEventInvokeConfig"
      ( \x ->
          FunctionEventInvokeConfig'
            Lude.<$> (x Lude..:? "FunctionArn")
            Lude.<*> (x Lude..:? "MaximumEventAgeInSeconds")
            Lude.<*> (x Lude..:? "MaximumRetryAttempts")
            Lude.<*> (x Lude..:? "LastModified")
            Lude.<*> (x Lude..:? "DestinationConfig")
      )

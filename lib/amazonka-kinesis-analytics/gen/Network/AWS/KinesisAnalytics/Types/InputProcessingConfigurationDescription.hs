-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
  ( InputProcessingConfigurationDescription (..),

    -- * Smart constructor
    mkInputProcessingConfigurationDescription,

    -- * Lenses
    ipcdInputLambdaProcessorDescription,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration information about an input processor. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
-- /See:/ 'mkInputProcessingConfigurationDescription' smart constructor.
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription'
  { inputLambdaProcessorDescription ::
      Lude.Maybe
        InputLambdaProcessorDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputProcessingConfigurationDescription' with the minimum fields required to make a request.
--
-- * 'inputLambdaProcessorDescription' - Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
mkInputProcessingConfigurationDescription ::
  InputProcessingConfigurationDescription
mkInputProcessingConfigurationDescription =
  InputProcessingConfigurationDescription'
    { inputLambdaProcessorDescription =
        Lude.Nothing
    }

-- | Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
--
-- /Note:/ Consider using 'inputLambdaProcessorDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcdInputLambdaProcessorDescription :: Lens.Lens' InputProcessingConfigurationDescription (Lude.Maybe InputLambdaProcessorDescription)
ipcdInputLambdaProcessorDescription = Lens.lens (inputLambdaProcessorDescription :: InputProcessingConfigurationDescription -> Lude.Maybe InputLambdaProcessorDescription) (\s a -> s {inputLambdaProcessorDescription = a} :: InputProcessingConfigurationDescription)
{-# DEPRECATED ipcdInputLambdaProcessorDescription "Use generic-lens or generic-optics with 'inputLambdaProcessorDescription' instead." #-}

instance Lude.FromJSON InputProcessingConfigurationDescription where
  parseJSON =
    Lude.withObject
      "InputProcessingConfigurationDescription"
      ( \x ->
          InputProcessingConfigurationDescription'
            Lude.<$> (x Lude..:? "InputLambdaProcessorDescription")
      )

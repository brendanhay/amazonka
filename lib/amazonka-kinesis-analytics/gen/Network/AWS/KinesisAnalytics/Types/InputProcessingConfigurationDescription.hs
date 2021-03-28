{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
  ( InputProcessingConfigurationDescription (..)
  -- * Smart constructor
  , mkInputProcessingConfigurationDescription
  -- * Lenses
  , ipcdInputLambdaProcessorDescription
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides configuration information about an input processor. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
-- /See:/ 'mkInputProcessingConfigurationDescription' smart constructor.
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription'
  { inputLambdaProcessorDescription :: Core.Maybe Types.InputLambdaProcessorDescription
    -- ^ Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputProcessingConfigurationDescription' value with any optional fields omitted.
mkInputProcessingConfigurationDescription
    :: InputProcessingConfigurationDescription
mkInputProcessingConfigurationDescription
  = InputProcessingConfigurationDescription'{inputLambdaProcessorDescription
                                               = Core.Nothing}

-- | Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
--
-- /Note:/ Consider using 'inputLambdaProcessorDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcdInputLambdaProcessorDescription :: Lens.Lens' InputProcessingConfigurationDescription (Core.Maybe Types.InputLambdaProcessorDescription)
ipcdInputLambdaProcessorDescription = Lens.field @"inputLambdaProcessorDescription"
{-# INLINEABLE ipcdInputLambdaProcessorDescription #-}
{-# DEPRECATED inputLambdaProcessorDescription "Use generic-lens or generic-optics with 'inputLambdaProcessorDescription' instead"  #-}

instance Core.FromJSON InputProcessingConfigurationDescription
         where
        parseJSON
          = Core.withObject "InputProcessingConfigurationDescription" Core.$
              \ x ->
                InputProcessingConfigurationDescription' Core.<$>
                  (x Core..:? "InputLambdaProcessorDescription")

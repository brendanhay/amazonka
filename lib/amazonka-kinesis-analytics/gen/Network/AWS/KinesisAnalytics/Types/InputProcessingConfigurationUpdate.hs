{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
  ( InputProcessingConfigurationUpdate (..)
  -- * Smart constructor
  , mkInputProcessingConfigurationUpdate
  -- * Lenses
  , ipcuInputLambdaProcessorUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes updates to an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> . 
--
-- /See:/ 'mkInputProcessingConfigurationUpdate' smart constructor.
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate'
  { inputLambdaProcessorUpdate :: Types.InputLambdaProcessorUpdate
    -- ^ Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputProcessingConfigurationUpdate' value with any optional fields omitted.
mkInputProcessingConfigurationUpdate
    :: Types.InputLambdaProcessorUpdate -- ^ 'inputLambdaProcessorUpdate'
    -> InputProcessingConfigurationUpdate
mkInputProcessingConfigurationUpdate inputLambdaProcessorUpdate
  = InputProcessingConfigurationUpdate'{inputLambdaProcessorUpdate}

-- | Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
--
-- /Note:/ Consider using 'inputLambdaProcessorUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcuInputLambdaProcessorUpdate :: Lens.Lens' InputProcessingConfigurationUpdate Types.InputLambdaProcessorUpdate
ipcuInputLambdaProcessorUpdate = Lens.field @"inputLambdaProcessorUpdate"
{-# INLINEABLE ipcuInputLambdaProcessorUpdate #-}
{-# DEPRECATED inputLambdaProcessorUpdate "Use generic-lens or generic-optics with 'inputLambdaProcessorUpdate' instead"  #-}

instance Core.FromJSON InputProcessingConfigurationUpdate where
        toJSON InputProcessingConfigurationUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("InputLambdaProcessorUpdate" Core..= inputLambdaProcessorUpdate)])

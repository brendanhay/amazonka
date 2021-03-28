{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
  ( InputProcessingConfiguration (..)
  -- * Smart constructor
  , mkInputProcessingConfiguration
  -- * Lenses
  , ipcInputLambdaProcessor
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
-- /See:/ 'mkInputProcessingConfiguration' smart constructor.
newtype InputProcessingConfiguration = InputProcessingConfiguration'
  { inputLambdaProcessor :: Types.InputLambdaProcessor
    -- ^ The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputProcessingConfiguration' value with any optional fields omitted.
mkInputProcessingConfiguration
    :: Types.InputLambdaProcessor -- ^ 'inputLambdaProcessor'
    -> InputProcessingConfiguration
mkInputProcessingConfiguration inputLambdaProcessor
  = InputProcessingConfiguration'{inputLambdaProcessor}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
--
-- /Note:/ Consider using 'inputLambdaProcessor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcInputLambdaProcessor :: Lens.Lens' InputProcessingConfiguration Types.InputLambdaProcessor
ipcInputLambdaProcessor = Lens.field @"inputLambdaProcessor"
{-# INLINEABLE ipcInputLambdaProcessor #-}
{-# DEPRECATED inputLambdaProcessor "Use generic-lens or generic-optics with 'inputLambdaProcessor' instead"  #-}

instance Core.FromJSON InputProcessingConfiguration where
        toJSON InputProcessingConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputLambdaProcessor" Core..= inputLambdaProcessor)])

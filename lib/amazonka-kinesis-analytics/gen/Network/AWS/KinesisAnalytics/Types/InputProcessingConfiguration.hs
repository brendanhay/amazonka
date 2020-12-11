-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
  ( InputProcessingConfiguration (..),

    -- * Smart constructor
    mkInputProcessingConfiguration,

    -- * Lenses
    ipcInputLambdaProcessor,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
-- /See:/ 'mkInputProcessingConfiguration' smart constructor.
newtype InputProcessingConfiguration = InputProcessingConfiguration'
  { inputLambdaProcessor ::
      InputLambdaProcessor
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputProcessingConfiguration' with the minimum fields required to make a request.
--
-- * 'inputLambdaProcessor' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
mkInputProcessingConfiguration ::
  -- | 'inputLambdaProcessor'
  InputLambdaProcessor ->
  InputProcessingConfiguration
mkInputProcessingConfiguration pInputLambdaProcessor_ =
  InputProcessingConfiguration'
    { inputLambdaProcessor =
        pInputLambdaProcessor_
    }

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
--
-- /Note:/ Consider using 'inputLambdaProcessor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcInputLambdaProcessor :: Lens.Lens' InputProcessingConfiguration InputLambdaProcessor
ipcInputLambdaProcessor = Lens.lens (inputLambdaProcessor :: InputProcessingConfiguration -> InputLambdaProcessor) (\s a -> s {inputLambdaProcessor = a} :: InputProcessingConfiguration)
{-# DEPRECATED ipcInputLambdaProcessor "Use generic-lens or generic-optics with 'inputLambdaProcessor' instead." #-}

instance Lude.ToJSON InputProcessingConfiguration where
  toJSON InputProcessingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("InputLambdaProcessor" Lude..= inputLambdaProcessor)]
      )

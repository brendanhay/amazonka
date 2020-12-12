{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
  ( InputProcessingConfigurationUpdate (..),

    -- * Smart constructor
    mkInputProcessingConfigurationUpdate,

    -- * Lenses
    ipcuInputLambdaProcessorUpdate,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes updates to an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> .
--
-- /See:/ 'mkInputProcessingConfigurationUpdate' smart constructor.
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate'
  { inputLambdaProcessorUpdate ::
      InputLambdaProcessorUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputProcessingConfigurationUpdate' with the minimum fields required to make a request.
--
-- * 'inputLambdaProcessorUpdate' - Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
mkInputProcessingConfigurationUpdate ::
  -- | 'inputLambdaProcessorUpdate'
  InputLambdaProcessorUpdate ->
  InputProcessingConfigurationUpdate
mkInputProcessingConfigurationUpdate pInputLambdaProcessorUpdate_ =
  InputProcessingConfigurationUpdate'
    { inputLambdaProcessorUpdate =
        pInputLambdaProcessorUpdate_
    }

-- | Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
--
-- /Note:/ Consider using 'inputLambdaProcessorUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipcuInputLambdaProcessorUpdate :: Lens.Lens' InputProcessingConfigurationUpdate InputLambdaProcessorUpdate
ipcuInputLambdaProcessorUpdate = Lens.lens (inputLambdaProcessorUpdate :: InputProcessingConfigurationUpdate -> InputLambdaProcessorUpdate) (\s a -> s {inputLambdaProcessorUpdate = a} :: InputProcessingConfigurationUpdate)
{-# DEPRECATED ipcuInputLambdaProcessorUpdate "Use generic-lens or generic-optics with 'inputLambdaProcessorUpdate' instead." #-}

instance Lude.ToJSON InputProcessingConfigurationUpdate where
  toJSON InputProcessingConfigurationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("InputLambdaProcessorUpdate" Lude..= inputLambdaProcessorUpdate)
          ]
      )

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingInput
  ( ProcessingInput (..),

    -- * Smart constructor
    mkProcessingInput,

    -- * Lenses
    piInputName,
    piS3Input,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3Input

-- | The inputs for a processing job.
--
-- /See:/ 'mkProcessingInput' smart constructor.
data ProcessingInput = ProcessingInput'
  { inputName :: Lude.Text,
    s3Input :: ProcessingS3Input
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingInput' with the minimum fields required to make a request.
--
-- * 'inputName' - The name of the inputs for the processing job.
-- * 's3Input' - The S3 inputs for the processing job.
mkProcessingInput ::
  -- | 'inputName'
  Lude.Text ->
  -- | 's3Input'
  ProcessingS3Input ->
  ProcessingInput
mkProcessingInput pInputName_ pS3Input_ =
  ProcessingInput' {inputName = pInputName_, s3Input = pS3Input_}

-- | The name of the inputs for the processing job.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piInputName :: Lens.Lens' ProcessingInput Lude.Text
piInputName = Lens.lens (inputName :: ProcessingInput -> Lude.Text) (\s a -> s {inputName = a} :: ProcessingInput)
{-# DEPRECATED piInputName "Use generic-lens or generic-optics with 'inputName' instead." #-}

-- | The S3 inputs for the processing job.
--
-- /Note:/ Consider using 's3Input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piS3Input :: Lens.Lens' ProcessingInput ProcessingS3Input
piS3Input = Lens.lens (s3Input :: ProcessingInput -> ProcessingS3Input) (\s a -> s {s3Input = a} :: ProcessingInput)
{-# DEPRECATED piS3Input "Use generic-lens or generic-optics with 's3Input' instead." #-}

instance Lude.FromJSON ProcessingInput where
  parseJSON =
    Lude.withObject
      "ProcessingInput"
      ( \x ->
          ProcessingInput'
            Lude.<$> (x Lude..: "InputName") Lude.<*> (x Lude..: "S3Input")
      )

instance Lude.ToJSON ProcessingInput where
  toJSON ProcessingInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InputName" Lude..= inputName),
            Lude.Just ("S3Input" Lude..= s3Input)
          ]
      )

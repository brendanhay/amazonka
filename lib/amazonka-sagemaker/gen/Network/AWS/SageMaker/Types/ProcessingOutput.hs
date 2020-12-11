-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutput
  ( ProcessingOutput (..),

    -- * Smart constructor
    mkProcessingOutput,

    -- * Lenses
    poOutputName,
    poS3Output,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3Output

-- | Describes the results of a processing job.
--
-- /See:/ 'mkProcessingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { outputName :: Lude.Text,
    s3Output :: ProcessingS3Output
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingOutput' with the minimum fields required to make a request.
--
-- * 'outputName' - The name for the processing job output.
-- * 's3Output' - Configuration for processing job outputs in Amazon S3.
mkProcessingOutput ::
  -- | 'outputName'
  Lude.Text ->
  -- | 's3Output'
  ProcessingS3Output ->
  ProcessingOutput
mkProcessingOutput pOutputName_ pS3Output_ =
  ProcessingOutput'
    { outputName = pOutputName_,
      s3Output = pS3Output_
    }

-- | The name for the processing job output.
--
-- /Note:/ Consider using 'outputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOutputName :: Lens.Lens' ProcessingOutput Lude.Text
poOutputName = Lens.lens (outputName :: ProcessingOutput -> Lude.Text) (\s a -> s {outputName = a} :: ProcessingOutput)
{-# DEPRECATED poOutputName "Use generic-lens or generic-optics with 'outputName' instead." #-}

-- | Configuration for processing job outputs in Amazon S3.
--
-- /Note:/ Consider using 's3Output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poS3Output :: Lens.Lens' ProcessingOutput ProcessingS3Output
poS3Output = Lens.lens (s3Output :: ProcessingOutput -> ProcessingS3Output) (\s a -> s {s3Output = a} :: ProcessingOutput)
{-# DEPRECATED poS3Output "Use generic-lens or generic-optics with 's3Output' instead." #-}

instance Lude.FromJSON ProcessingOutput where
  parseJSON =
    Lude.withObject
      "ProcessingOutput"
      ( \x ->
          ProcessingOutput'
            Lude.<$> (x Lude..: "OutputName") Lude.<*> (x Lude..: "S3Output")
      )

instance Lude.ToJSON ProcessingOutput where
  toJSON ProcessingOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OutputName" Lude..= outputName),
            Lude.Just ("S3Output" Lude..= s3Output)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    poS3Output,
    poOutputName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3Output

-- | Describes the results of a processing job.
--
-- /See:/ 'mkProcessingOutput' smart constructor.
data ProcessingOutput = ProcessingOutput'
  { -- | Configuration for processing job outputs in Amazon S3.
    s3Output :: ProcessingS3Output,
    -- | The name for the processing job output.
    outputName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingOutput' with the minimum fields required to make a request.
--
-- * 's3Output' - Configuration for processing job outputs in Amazon S3.
-- * 'outputName' - The name for the processing job output.
mkProcessingOutput ::
  -- | 's3Output'
  ProcessingS3Output ->
  -- | 'outputName'
  Lude.Text ->
  ProcessingOutput
mkProcessingOutput pS3Output_ pOutputName_ =
  ProcessingOutput'
    { s3Output = pS3Output_,
      outputName = pOutputName_
    }

-- | Configuration for processing job outputs in Amazon S3.
--
-- /Note:/ Consider using 's3Output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poS3Output :: Lens.Lens' ProcessingOutput ProcessingS3Output
poS3Output = Lens.lens (s3Output :: ProcessingOutput -> ProcessingS3Output) (\s a -> s {s3Output = a} :: ProcessingOutput)
{-# DEPRECATED poS3Output "Use generic-lens or generic-optics with 's3Output' instead." #-}

-- | The name for the processing job output.
--
-- /Note:/ Consider using 'outputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOutputName :: Lens.Lens' ProcessingOutput Lude.Text
poOutputName = Lens.lens (outputName :: ProcessingOutput -> Lude.Text) (\s a -> s {outputName = a} :: ProcessingOutput)
{-# DEPRECATED poOutputName "Use generic-lens or generic-optics with 'outputName' instead." #-}

instance Lude.FromJSON ProcessingOutput where
  parseJSON =
    Lude.withObject
      "ProcessingOutput"
      ( \x ->
          ProcessingOutput'
            Lude.<$> (x Lude..: "S3Output") Lude.<*> (x Lude..: "OutputName")
      )

instance Lude.ToJSON ProcessingOutput where
  toJSON ProcessingOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3Output" Lude..= s3Output),
            Lude.Just ("OutputName" Lude..= outputName)
          ]
      )

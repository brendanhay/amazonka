{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingOutputConfig
  ( ProcessingOutputConfig (..),

    -- * Smart constructor
    mkProcessingOutputConfig,

    -- * Lenses
    pocOutputs,
    pocKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingOutput

-- | The output configuration for the processing job.
--
-- /See:/ 'mkProcessingOutputConfig' smart constructor.
data ProcessingOutputConfig = ProcessingOutputConfig'
  { -- | Output configuration information for a processing job.
    outputs :: [ProcessingOutput],
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingOutputConfig' with the minimum fields required to make a request.
--
-- * 'outputs' - Output configuration information for a processing job.
-- * 'kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
mkProcessingOutputConfig ::
  ProcessingOutputConfig
mkProcessingOutputConfig =
  ProcessingOutputConfig'
    { outputs = Lude.mempty,
      kmsKeyId = Lude.Nothing
    }

-- | Output configuration information for a processing job.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocOutputs :: Lens.Lens' ProcessingOutputConfig [ProcessingOutput]
pocOutputs = Lens.lens (outputs :: ProcessingOutputConfig -> [ProcessingOutput]) (\s a -> s {outputs = a} :: ProcessingOutputConfig)
{-# DEPRECATED pocOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocKMSKeyId :: Lens.Lens' ProcessingOutputConfig (Lude.Maybe Lude.Text)
pocKMSKeyId = Lens.lens (kmsKeyId :: ProcessingOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ProcessingOutputConfig)
{-# DEPRECATED pocKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON ProcessingOutputConfig where
  parseJSON =
    Lude.withObject
      "ProcessingOutputConfig"
      ( \x ->
          ProcessingOutputConfig'
            Lude.<$> (x Lude..:? "Outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON ProcessingOutputConfig where
  toJSON ProcessingOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Outputs" Lude..= outputs),
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )

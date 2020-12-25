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
    pocKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.ProcessingOutput as Types

-- | The output configuration for the processing job.
--
-- /See:/ 'mkProcessingOutputConfig' smart constructor.
data ProcessingOutputConfig = ProcessingOutputConfig'
  { -- | Output configuration information for a processing job.
    outputs :: [Types.ProcessingOutput],
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingOutputConfig' value with any optional fields omitted.
mkProcessingOutputConfig ::
  ProcessingOutputConfig
mkProcessingOutputConfig =
  ProcessingOutputConfig'
    { outputs = Core.mempty,
      kmsKeyId = Core.Nothing
    }

-- | Output configuration information for a processing job.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocOutputs :: Lens.Lens' ProcessingOutputConfig [Types.ProcessingOutput]
pocOutputs = Lens.field @"outputs"
{-# DEPRECATED pocOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the processing job output. @KmsKeyId@ can be an ID of a KMS key, ARN of a KMS key, alias of a KMS key, or alias of a KMS key. The @KmsKeyId@ is applied to all outputs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocKmsKeyId :: Lens.Lens' ProcessingOutputConfig (Core.Maybe Types.KmsKeyId)
pocKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED pocKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON ProcessingOutputConfig where
  toJSON ProcessingOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Outputs" Core..= outputs),
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON ProcessingOutputConfig where
  parseJSON =
    Core.withObject "ProcessingOutputConfig" Core.$
      \x ->
        ProcessingOutputConfig'
          Core.<$> (x Core..:? "Outputs" Core..!= Core.mempty)
          Core.<*> (x Core..:? "KmsKeyId")

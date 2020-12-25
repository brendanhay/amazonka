{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
  ( FlowDefinitionOutputConfig (..),

    -- * Smart constructor
    mkFlowDefinitionOutputConfig,

    -- * Lenses
    fdocS3OutputPath,
    fdocKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Contains information about where human output will be stored.
--
-- /See:/ 'mkFlowDefinitionOutputConfig' smart constructor.
data FlowDefinitionOutputConfig = FlowDefinitionOutputConfig'
  { -- | The Amazon S3 path where the object containing human output will be made available.
    s3OutputPath :: Types.S3OutputPath,
    -- | The Amazon Key Management Service (KMS) key ID for server-side encryption.
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FlowDefinitionOutputConfig' value with any optional fields omitted.
mkFlowDefinitionOutputConfig ::
  -- | 's3OutputPath'
  Types.S3OutputPath ->
  FlowDefinitionOutputConfig
mkFlowDefinitionOutputConfig s3OutputPath =
  FlowDefinitionOutputConfig'
    { s3OutputPath,
      kmsKeyId = Core.Nothing
    }

-- | The Amazon S3 path where the object containing human output will be made available.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdocS3OutputPath :: Lens.Lens' FlowDefinitionOutputConfig Types.S3OutputPath
fdocS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED fdocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | The Amazon Key Management Service (KMS) key ID for server-side encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdocKmsKeyId :: Lens.Lens' FlowDefinitionOutputConfig (Core.Maybe Types.KmsKeyId)
fdocKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED fdocKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON FlowDefinitionOutputConfig where
  toJSON FlowDefinitionOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputPath" Core..= s3OutputPath),
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON FlowDefinitionOutputConfig where
  parseJSON =
    Core.withObject "FlowDefinitionOutputConfig" Core.$
      \x ->
        FlowDefinitionOutputConfig'
          Core.<$> (x Core..: "S3OutputPath") Core.<*> (x Core..:? "KmsKeyId")

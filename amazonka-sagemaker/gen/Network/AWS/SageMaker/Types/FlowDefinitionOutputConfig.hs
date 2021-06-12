{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about where human output will be stored.
--
-- /See:/ 'newFlowDefinitionOutputConfig' smart constructor.
data FlowDefinitionOutputConfig = FlowDefinitionOutputConfig'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The Amazon S3 path where the object containing human output will be made
    -- available.
    --
    -- To learn more about the format of Amazon A2I output data, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-output-data.html Amazon A2I Output Data>.
    s3OutputPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FlowDefinitionOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'flowDefinitionOutputConfig_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 's3OutputPath', 'flowDefinitionOutputConfig_s3OutputPath' - The Amazon S3 path where the object containing human output will be made
-- available.
--
-- To learn more about the format of Amazon A2I output data, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-output-data.html Amazon A2I Output Data>.
newFlowDefinitionOutputConfig ::
  -- | 's3OutputPath'
  Core.Text ->
  FlowDefinitionOutputConfig
newFlowDefinitionOutputConfig pS3OutputPath_ =
  FlowDefinitionOutputConfig'
    { kmsKeyId =
        Core.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
flowDefinitionOutputConfig_kmsKeyId :: Lens.Lens' FlowDefinitionOutputConfig (Core.Maybe Core.Text)
flowDefinitionOutputConfig_kmsKeyId = Lens.lens (\FlowDefinitionOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@FlowDefinitionOutputConfig' {} a -> s {kmsKeyId = a} :: FlowDefinitionOutputConfig)

-- | The Amazon S3 path where the object containing human output will be made
-- available.
--
-- To learn more about the format of Amazon A2I output data, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-output-data.html Amazon A2I Output Data>.
flowDefinitionOutputConfig_s3OutputPath :: Lens.Lens' FlowDefinitionOutputConfig Core.Text
flowDefinitionOutputConfig_s3OutputPath = Lens.lens (\FlowDefinitionOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@FlowDefinitionOutputConfig' {} a -> s {s3OutputPath = a} :: FlowDefinitionOutputConfig)

instance Core.FromJSON FlowDefinitionOutputConfig where
  parseJSON =
    Core.withObject
      "FlowDefinitionOutputConfig"
      ( \x ->
          FlowDefinitionOutputConfig'
            Core.<$> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..: "S3OutputPath")
      )

instance Core.Hashable FlowDefinitionOutputConfig

instance Core.NFData FlowDefinitionOutputConfig

instance Core.ToJSON FlowDefinitionOutputConfig where
  toJSON FlowDefinitionOutputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            Core.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )

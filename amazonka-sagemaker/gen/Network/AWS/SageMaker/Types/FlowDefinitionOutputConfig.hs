{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about where human output will be stored.
--
-- /See:/ 'newFlowDefinitionOutputConfig' smart constructor.
data FlowDefinitionOutputConfig = FlowDefinitionOutputConfig'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path where the object containing human output will be made
    -- available.
    --
    -- To learn more about the format of Amazon A2I output data, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-output-data.html Amazon A2I Output Data>.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  FlowDefinitionOutputConfig
newFlowDefinitionOutputConfig pS3OutputPath_ =
  FlowDefinitionOutputConfig'
    { kmsKeyId =
        Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
flowDefinitionOutputConfig_kmsKeyId :: Lens.Lens' FlowDefinitionOutputConfig (Prelude.Maybe Prelude.Text)
flowDefinitionOutputConfig_kmsKeyId = Lens.lens (\FlowDefinitionOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@FlowDefinitionOutputConfig' {} a -> s {kmsKeyId = a} :: FlowDefinitionOutputConfig)

-- | The Amazon S3 path where the object containing human output will be made
-- available.
--
-- To learn more about the format of Amazon A2I output data, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-output-data.html Amazon A2I Output Data>.
flowDefinitionOutputConfig_s3OutputPath :: Lens.Lens' FlowDefinitionOutputConfig Prelude.Text
flowDefinitionOutputConfig_s3OutputPath = Lens.lens (\FlowDefinitionOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@FlowDefinitionOutputConfig' {} a -> s {s3OutputPath = a} :: FlowDefinitionOutputConfig)

instance Prelude.FromJSON FlowDefinitionOutputConfig where
  parseJSON =
    Prelude.withObject
      "FlowDefinitionOutputConfig"
      ( \x ->
          FlowDefinitionOutputConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable FlowDefinitionOutputConfig

instance Prelude.NFData FlowDefinitionOutputConfig

instance Prelude.ToJSON FlowDefinitionOutputConfig where
  toJSON FlowDefinitionOutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )

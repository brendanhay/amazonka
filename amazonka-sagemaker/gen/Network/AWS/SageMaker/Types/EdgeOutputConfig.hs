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
-- Module      : Network.AWS.SageMaker.Types.EdgeOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgeOutputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The output configuration.
--
-- /See:/ 'newEdgeOutputConfig' smart constructor.
data EdgeOutputConfig = EdgeOutputConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data on the storage volume after compilation job. If you
    -- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
    -- for Amazon S3 for your role\'s account.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The Amazon Simple Storage (S3) bucker URI.
    s3OutputLocation :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EdgeOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'edgeOutputConfig_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume after compilation job. If you
-- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
-- for Amazon S3 for your role\'s account.
--
-- 's3OutputLocation', 'edgeOutputConfig_s3OutputLocation' - The Amazon Simple Storage (S3) bucker URI.
newEdgeOutputConfig ::
  -- | 's3OutputLocation'
  Core.Text ->
  EdgeOutputConfig
newEdgeOutputConfig pS3OutputLocation_ =
  EdgeOutputConfig'
    { kmsKeyId = Core.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume after compilation job. If you
-- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
-- for Amazon S3 for your role\'s account.
edgeOutputConfig_kmsKeyId :: Lens.Lens' EdgeOutputConfig (Core.Maybe Core.Text)
edgeOutputConfig_kmsKeyId = Lens.lens (\EdgeOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@EdgeOutputConfig' {} a -> s {kmsKeyId = a} :: EdgeOutputConfig)

-- | The Amazon Simple Storage (S3) bucker URI.
edgeOutputConfig_s3OutputLocation :: Lens.Lens' EdgeOutputConfig Core.Text
edgeOutputConfig_s3OutputLocation = Lens.lens (\EdgeOutputConfig' {s3OutputLocation} -> s3OutputLocation) (\s@EdgeOutputConfig' {} a -> s {s3OutputLocation = a} :: EdgeOutputConfig)

instance Core.FromJSON EdgeOutputConfig where
  parseJSON =
    Core.withObject
      "EdgeOutputConfig"
      ( \x ->
          EdgeOutputConfig'
            Core.<$> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..: "S3OutputLocation")
      )

instance Core.Hashable EdgeOutputConfig

instance Core.NFData EdgeOutputConfig

instance Core.ToJSON EdgeOutputConfig where
  toJSON EdgeOutputConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            Core.Just
              ("S3OutputLocation" Core..= s3OutputLocation)
          ]
      )

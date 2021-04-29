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
-- Module      : Network.AWS.SageMaker.Types.EdgeOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgeOutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The output configuration.
--
-- /See:/ 'newEdgeOutputConfig' smart constructor.
data EdgeOutputConfig = EdgeOutputConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data on the storage volume after compilation job. If you
    -- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
    -- for Amazon S3 for your role\'s account.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Storage (S3) bucker URI.
    s3OutputLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  EdgeOutputConfig
newEdgeOutputConfig pS3OutputLocation_ =
  EdgeOutputConfig'
    { kmsKeyId = Prelude.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume after compilation job. If you
-- don\'t provide a KMS key ID, Amazon SageMaker uses the default KMS key
-- for Amazon S3 for your role\'s account.
edgeOutputConfig_kmsKeyId :: Lens.Lens' EdgeOutputConfig (Prelude.Maybe Prelude.Text)
edgeOutputConfig_kmsKeyId = Lens.lens (\EdgeOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@EdgeOutputConfig' {} a -> s {kmsKeyId = a} :: EdgeOutputConfig)

-- | The Amazon Simple Storage (S3) bucker URI.
edgeOutputConfig_s3OutputLocation :: Lens.Lens' EdgeOutputConfig Prelude.Text
edgeOutputConfig_s3OutputLocation = Lens.lens (\EdgeOutputConfig' {s3OutputLocation} -> s3OutputLocation) (\s@EdgeOutputConfig' {} a -> s {s3OutputLocation = a} :: EdgeOutputConfig)

instance Prelude.FromJSON EdgeOutputConfig where
  parseJSON =
    Prelude.withObject
      "EdgeOutputConfig"
      ( \x ->
          EdgeOutputConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3OutputLocation")
      )

instance Prelude.Hashable EdgeOutputConfig

instance Prelude.NFData EdgeOutputConfig

instance Prelude.ToJSON EdgeOutputConfig where
  toJSON EdgeOutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("S3OutputLocation" Prelude..= s3OutputLocation)
          ]
      )

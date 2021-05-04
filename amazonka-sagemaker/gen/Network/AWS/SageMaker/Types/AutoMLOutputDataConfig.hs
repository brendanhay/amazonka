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
-- Module      : Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLOutputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The output data configuration.
--
-- /See:/ 'newAutoMLOutputDataConfig' smart constructor.
data AutoMLOutputDataConfig = AutoMLOutputDataConfig'
  { -- | The AWS KMS encryption key ID.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 output path. Must be 128 characters or less.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoMLOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'autoMLOutputDataConfig_kmsKeyId' - The AWS KMS encryption key ID.
--
-- 's3OutputPath', 'autoMLOutputDataConfig_s3OutputPath' - The Amazon S3 output path. Must be 128 characters or less.
newAutoMLOutputDataConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  AutoMLOutputDataConfig
newAutoMLOutputDataConfig pS3OutputPath_ =
  AutoMLOutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The AWS KMS encryption key ID.
autoMLOutputDataConfig_kmsKeyId :: Lens.Lens' AutoMLOutputDataConfig (Prelude.Maybe Prelude.Text)
autoMLOutputDataConfig_kmsKeyId = Lens.lens (\AutoMLOutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@AutoMLOutputDataConfig' {} a -> s {kmsKeyId = a} :: AutoMLOutputDataConfig)

-- | The Amazon S3 output path. Must be 128 characters or less.
autoMLOutputDataConfig_s3OutputPath :: Lens.Lens' AutoMLOutputDataConfig Prelude.Text
autoMLOutputDataConfig_s3OutputPath = Lens.lens (\AutoMLOutputDataConfig' {s3OutputPath} -> s3OutputPath) (\s@AutoMLOutputDataConfig' {} a -> s {s3OutputPath = a} :: AutoMLOutputDataConfig)

instance Prelude.FromJSON AutoMLOutputDataConfig where
  parseJSON =
    Prelude.withObject
      "AutoMLOutputDataConfig"
      ( \x ->
          AutoMLOutputDataConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable AutoMLOutputDataConfig

instance Prelude.NFData AutoMLOutputDataConfig

instance Prelude.ToJSON AutoMLOutputDataConfig where
  toJSON AutoMLOutputDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )

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
-- Module      : Amazonka.SageMaker.Types.AutoMLOutputDataConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLOutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The output data configuration.
--
-- /See:/ 'newAutoMLOutputDataConfig' smart constructor.
data AutoMLOutputDataConfig = AutoMLOutputDataConfig'
  { -- | The Amazon Web Services KMS encryption key ID.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 output path. Must be 128 characters or less.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'autoMLOutputDataConfig_kmsKeyId' - The Amazon Web Services KMS encryption key ID.
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

-- | The Amazon Web Services KMS encryption key ID.
autoMLOutputDataConfig_kmsKeyId :: Lens.Lens' AutoMLOutputDataConfig (Prelude.Maybe Prelude.Text)
autoMLOutputDataConfig_kmsKeyId = Lens.lens (\AutoMLOutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@AutoMLOutputDataConfig' {} a -> s {kmsKeyId = a} :: AutoMLOutputDataConfig)

-- | The Amazon S3 output path. Must be 128 characters or less.
autoMLOutputDataConfig_s3OutputPath :: Lens.Lens' AutoMLOutputDataConfig Prelude.Text
autoMLOutputDataConfig_s3OutputPath = Lens.lens (\AutoMLOutputDataConfig' {s3OutputPath} -> s3OutputPath) (\s@AutoMLOutputDataConfig' {} a -> s {s3OutputPath = a} :: AutoMLOutputDataConfig)

instance Core.FromJSON AutoMLOutputDataConfig where
  parseJSON =
    Core.withObject
      "AutoMLOutputDataConfig"
      ( \x ->
          AutoMLOutputDataConfig'
            Prelude.<$> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..: "S3OutputPath")
      )

instance Prelude.Hashable AutoMLOutputDataConfig where
  hashWithSalt _salt AutoMLOutputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData AutoMLOutputDataConfig where
  rnf AutoMLOutputDataConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Core.ToJSON AutoMLOutputDataConfig where
  toJSON AutoMLOutputDataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )

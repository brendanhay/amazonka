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
-- Module      : Amazonka.Braket.Types.JobOutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobOutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the path to the S3 location where you want to store job
-- artifacts and the encryption key used to store them.
--
-- /See:/ 'newJobOutputDataConfig' smart constructor.
data JobOutputDataConfig = JobOutputDataConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon Braket uses to
    -- encrypt the job training artifacts at rest using Amazon S3 server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 path where you want Amazon Braket to store the job
    -- training artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3Path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'jobOutputDataConfig_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon Braket uses to
-- encrypt the job training artifacts at rest using Amazon S3 server-side
-- encryption.
--
-- 's3Path', 'jobOutputDataConfig_s3Path' - Identifies the S3 path where you want Amazon Braket to store the job
-- training artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newJobOutputDataConfig ::
  -- | 's3Path'
  Prelude.Text ->
  JobOutputDataConfig
newJobOutputDataConfig pS3Path_ =
  JobOutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3Path = pS3Path_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon Braket uses to
-- encrypt the job training artifacts at rest using Amazon S3 server-side
-- encryption.
jobOutputDataConfig_kmsKeyId :: Lens.Lens' JobOutputDataConfig (Prelude.Maybe Prelude.Text)
jobOutputDataConfig_kmsKeyId = Lens.lens (\JobOutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@JobOutputDataConfig' {} a -> s {kmsKeyId = a} :: JobOutputDataConfig)

-- | Identifies the S3 path where you want Amazon Braket to store the job
-- training artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
jobOutputDataConfig_s3Path :: Lens.Lens' JobOutputDataConfig Prelude.Text
jobOutputDataConfig_s3Path = Lens.lens (\JobOutputDataConfig' {s3Path} -> s3Path) (\s@JobOutputDataConfig' {} a -> s {s3Path = a} :: JobOutputDataConfig)

instance Data.FromJSON JobOutputDataConfig where
  parseJSON =
    Data.withObject
      "JobOutputDataConfig"
      ( \x ->
          JobOutputDataConfig'
            Prelude.<$> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..: "s3Path")
      )

instance Prelude.Hashable JobOutputDataConfig where
  hashWithSalt _salt JobOutputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Path

instance Prelude.NFData JobOutputDataConfig where
  rnf JobOutputDataConfig' {..} =
    Prelude.rnf kmsKeyId `Prelude.seq`
      Prelude.rnf s3Path

instance Data.ToJSON JobOutputDataConfig where
  toJSON JobOutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("s3Path" Data..= s3Path)
          ]
      )

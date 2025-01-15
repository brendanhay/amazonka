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
-- Module      : Amazonka.Glue.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CloudWatchEncryption
import Amazonka.Glue.Types.JobBookmarksEncryption
import Amazonka.Glue.Types.S3Encryption
import qualified Amazonka.Prelude as Prelude

-- | Specifies an encryption configuration.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption configuration for Amazon CloudWatch.
    cloudWatchEncryption :: Prelude.Maybe CloudWatchEncryption,
    -- | The encryption configuration for job bookmarks.
    jobBookmarksEncryption :: Prelude.Maybe JobBookmarksEncryption,
    -- | The encryption configuration for Amazon Simple Storage Service (Amazon
    -- S3) data.
    s3Encryption :: Prelude.Maybe [S3Encryption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchEncryption', 'encryptionConfiguration_cloudWatchEncryption' - The encryption configuration for Amazon CloudWatch.
--
-- 'jobBookmarksEncryption', 'encryptionConfiguration_jobBookmarksEncryption' - The encryption configuration for job bookmarks.
--
-- 's3Encryption', 'encryptionConfiguration_s3Encryption' - The encryption configuration for Amazon Simple Storage Service (Amazon
-- S3) data.
newEncryptionConfiguration ::
  EncryptionConfiguration
newEncryptionConfiguration =
  EncryptionConfiguration'
    { cloudWatchEncryption =
        Prelude.Nothing,
      jobBookmarksEncryption = Prelude.Nothing,
      s3Encryption = Prelude.Nothing
    }

-- | The encryption configuration for Amazon CloudWatch.
encryptionConfiguration_cloudWatchEncryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe CloudWatchEncryption)
encryptionConfiguration_cloudWatchEncryption = Lens.lens (\EncryptionConfiguration' {cloudWatchEncryption} -> cloudWatchEncryption) (\s@EncryptionConfiguration' {} a -> s {cloudWatchEncryption = a} :: EncryptionConfiguration)

-- | The encryption configuration for job bookmarks.
encryptionConfiguration_jobBookmarksEncryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe JobBookmarksEncryption)
encryptionConfiguration_jobBookmarksEncryption = Lens.lens (\EncryptionConfiguration' {jobBookmarksEncryption} -> jobBookmarksEncryption) (\s@EncryptionConfiguration' {} a -> s {jobBookmarksEncryption = a} :: EncryptionConfiguration)

-- | The encryption configuration for Amazon Simple Storage Service (Amazon
-- S3) data.
encryptionConfiguration_s3Encryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe [S3Encryption])
encryptionConfiguration_s3Encryption = Lens.lens (\EncryptionConfiguration' {s3Encryption} -> s3Encryption) (\s@EncryptionConfiguration' {} a -> s {s3Encryption = a} :: EncryptionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "CloudWatchEncryption")
            Prelude.<*> (x Data..:? "JobBookmarksEncryption")
            Prelude.<*> (x Data..:? "S3Encryption" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchEncryption
      `Prelude.hashWithSalt` jobBookmarksEncryption
      `Prelude.hashWithSalt` s3Encryption

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf cloudWatchEncryption `Prelude.seq`
      Prelude.rnf jobBookmarksEncryption `Prelude.seq`
        Prelude.rnf s3Encryption

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchEncryption" Data..=)
              Prelude.<$> cloudWatchEncryption,
            ("JobBookmarksEncryption" Data..=)
              Prelude.<$> jobBookmarksEncryption,
            ("S3Encryption" Data..=) Prelude.<$> s3Encryption
          ]
      )

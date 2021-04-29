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
-- Module      : Network.AWS.Glue.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionConfiguration where

import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.S3Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies an encryption configuration.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption configuration for job bookmarks.
    jobBookmarksEncryption :: Prelude.Maybe JobBookmarksEncryption,
    -- | The encryption configuration for Amazon Simple Storage Service (Amazon
    -- S3) data.
    s3Encryption :: Prelude.Maybe [S3Encryption],
    -- | The encryption configuration for Amazon CloudWatch.
    cloudWatchEncryption :: Prelude.Maybe CloudWatchEncryption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobBookmarksEncryption', 'encryptionConfiguration_jobBookmarksEncryption' - The encryption configuration for job bookmarks.
--
-- 's3Encryption', 'encryptionConfiguration_s3Encryption' - The encryption configuration for Amazon Simple Storage Service (Amazon
-- S3) data.
--
-- 'cloudWatchEncryption', 'encryptionConfiguration_cloudWatchEncryption' - The encryption configuration for Amazon CloudWatch.
newEncryptionConfiguration ::
  EncryptionConfiguration
newEncryptionConfiguration =
  EncryptionConfiguration'
    { jobBookmarksEncryption =
        Prelude.Nothing,
      s3Encryption = Prelude.Nothing,
      cloudWatchEncryption = Prelude.Nothing
    }

-- | The encryption configuration for job bookmarks.
encryptionConfiguration_jobBookmarksEncryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe JobBookmarksEncryption)
encryptionConfiguration_jobBookmarksEncryption = Lens.lens (\EncryptionConfiguration' {jobBookmarksEncryption} -> jobBookmarksEncryption) (\s@EncryptionConfiguration' {} a -> s {jobBookmarksEncryption = a} :: EncryptionConfiguration)

-- | The encryption configuration for Amazon Simple Storage Service (Amazon
-- S3) data.
encryptionConfiguration_s3Encryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe [S3Encryption])
encryptionConfiguration_s3Encryption = Lens.lens (\EncryptionConfiguration' {s3Encryption} -> s3Encryption) (\s@EncryptionConfiguration' {} a -> s {s3Encryption = a} :: EncryptionConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The encryption configuration for Amazon CloudWatch.
encryptionConfiguration_cloudWatchEncryption :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe CloudWatchEncryption)
encryptionConfiguration_cloudWatchEncryption = Lens.lens (\EncryptionConfiguration' {cloudWatchEncryption} -> cloudWatchEncryption) (\s@EncryptionConfiguration' {} a -> s {cloudWatchEncryption = a} :: EncryptionConfiguration)

instance Prelude.FromJSON EncryptionConfiguration where
  parseJSON =
    Prelude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Prelude..:? "JobBookmarksEncryption")
            Prelude.<*> ( x Prelude..:? "S3Encryption"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CloudWatchEncryption")
      )

instance Prelude.Hashable EncryptionConfiguration

instance Prelude.NFData EncryptionConfiguration

instance Prelude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("JobBookmarksEncryption" Prelude..=)
              Prelude.<$> jobBookmarksEncryption,
            ("S3Encryption" Prelude..=) Prelude.<$> s3Encryption,
            ("CloudWatchEncryption" Prelude..=)
              Prelude.<$> cloudWatchEncryption
          ]
      )

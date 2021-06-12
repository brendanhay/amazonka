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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.S3Encryption
import qualified Network.AWS.Lens as Lens

-- | Specifies an encryption configuration.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption configuration for job bookmarks.
    jobBookmarksEncryption :: Core.Maybe JobBookmarksEncryption,
    -- | The encryption configuration for Amazon Simple Storage Service (Amazon
    -- S3) data.
    s3Encryption :: Core.Maybe [S3Encryption],
    -- | The encryption configuration for Amazon CloudWatch.
    cloudWatchEncryption :: Core.Maybe CloudWatchEncryption
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      s3Encryption = Core.Nothing,
      cloudWatchEncryption = Core.Nothing
    }

-- | The encryption configuration for job bookmarks.
encryptionConfiguration_jobBookmarksEncryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe JobBookmarksEncryption)
encryptionConfiguration_jobBookmarksEncryption = Lens.lens (\EncryptionConfiguration' {jobBookmarksEncryption} -> jobBookmarksEncryption) (\s@EncryptionConfiguration' {} a -> s {jobBookmarksEncryption = a} :: EncryptionConfiguration)

-- | The encryption configuration for Amazon Simple Storage Service (Amazon
-- S3) data.
encryptionConfiguration_s3Encryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe [S3Encryption])
encryptionConfiguration_s3Encryption = Lens.lens (\EncryptionConfiguration' {s3Encryption} -> s3Encryption) (\s@EncryptionConfiguration' {} a -> s {s3Encryption = a} :: EncryptionConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The encryption configuration for Amazon CloudWatch.
encryptionConfiguration_cloudWatchEncryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe CloudWatchEncryption)
encryptionConfiguration_cloudWatchEncryption = Lens.lens (\EncryptionConfiguration' {cloudWatchEncryption} -> cloudWatchEncryption) (\s@EncryptionConfiguration' {} a -> s {cloudWatchEncryption = a} :: EncryptionConfiguration)

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Core.<$> (x Core..:? "JobBookmarksEncryption")
            Core.<*> (x Core..:? "S3Encryption" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CloudWatchEncryption")
      )

instance Core.Hashable EncryptionConfiguration

instance Core.NFData EncryptionConfiguration

instance Core.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobBookmarksEncryption" Core..=)
              Core.<$> jobBookmarksEncryption,
            ("S3Encryption" Core..=) Core.<$> s3Encryption,
            ("CloudWatchEncryption" Core..=)
              Core.<$> cloudWatchEncryption
          ]
      )

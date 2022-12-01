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
-- Module      : Amazonka.ImageBuilder.Types.S3Logs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.S3Logs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 logging configuration.
--
-- /See:/ 'newS3Logs' smart constructor.
data S3Logs = S3Logs'
  { -- | The Amazon S3 path to the bucket where the logs are stored.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket in which to store the logs.
    s3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Logs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 's3Logs_s3KeyPrefix' - The Amazon S3 path to the bucket where the logs are stored.
--
-- 's3BucketName', 's3Logs_s3BucketName' - The S3 bucket in which to store the logs.
newS3Logs ::
  S3Logs
newS3Logs =
  S3Logs'
    { s3KeyPrefix = Prelude.Nothing,
      s3BucketName = Prelude.Nothing
    }

-- | The Amazon S3 path to the bucket where the logs are stored.
s3Logs_s3KeyPrefix :: Lens.Lens' S3Logs (Prelude.Maybe Prelude.Text)
s3Logs_s3KeyPrefix = Lens.lens (\S3Logs' {s3KeyPrefix} -> s3KeyPrefix) (\s@S3Logs' {} a -> s {s3KeyPrefix = a} :: S3Logs)

-- | The S3 bucket in which to store the logs.
s3Logs_s3BucketName :: Lens.Lens' S3Logs (Prelude.Maybe Prelude.Text)
s3Logs_s3BucketName = Lens.lens (\S3Logs' {s3BucketName} -> s3BucketName) (\s@S3Logs' {} a -> s {s3BucketName = a} :: S3Logs)

instance Core.FromJSON S3Logs where
  parseJSON =
    Core.withObject
      "S3Logs"
      ( \x ->
          S3Logs'
            Prelude.<$> (x Core..:? "s3KeyPrefix")
            Prelude.<*> (x Core..:? "s3BucketName")
      )

instance Prelude.Hashable S3Logs where
  hashWithSalt _salt S3Logs' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` s3BucketName

instance Prelude.NFData S3Logs where
  rnf S3Logs' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf s3BucketName

instance Core.ToJSON S3Logs where
  toJSON S3Logs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
            ("s3BucketName" Core..=) Prelude.<$> s3BucketName
          ]
      )

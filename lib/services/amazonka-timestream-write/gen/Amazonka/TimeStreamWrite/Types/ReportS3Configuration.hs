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
-- Module      : Amazonka.TimeStreamWrite.Types.ReportS3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.ReportS3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.S3EncryptionOption

-- |
--
-- /See:/ 'newReportS3Configuration' smart constructor.
data ReportS3Configuration = ReportS3Configuration'
  { encryptionOption :: Prelude.Maybe S3EncryptionOption,
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    objectKeyPrefix :: Prelude.Maybe Prelude.Text,
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportS3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionOption', 'reportS3Configuration_encryptionOption' -
--
-- 'kmsKeyId', 'reportS3Configuration_kmsKeyId' -
--
-- 'objectKeyPrefix', 'reportS3Configuration_objectKeyPrefix' -
--
-- 'bucketName', 'reportS3Configuration_bucketName' -
newReportS3Configuration ::
  -- | 'bucketName'
  Prelude.Text ->
  ReportS3Configuration
newReportS3Configuration pBucketName_ =
  ReportS3Configuration'
    { encryptionOption =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      objectKeyPrefix = Prelude.Nothing,
      bucketName = pBucketName_
    }

reportS3Configuration_encryptionOption :: Lens.Lens' ReportS3Configuration (Prelude.Maybe S3EncryptionOption)
reportS3Configuration_encryptionOption = Lens.lens (\ReportS3Configuration' {encryptionOption} -> encryptionOption) (\s@ReportS3Configuration' {} a -> s {encryptionOption = a} :: ReportS3Configuration)

reportS3Configuration_kmsKeyId :: Lens.Lens' ReportS3Configuration (Prelude.Maybe Prelude.Text)
reportS3Configuration_kmsKeyId = Lens.lens (\ReportS3Configuration' {kmsKeyId} -> kmsKeyId) (\s@ReportS3Configuration' {} a -> s {kmsKeyId = a} :: ReportS3Configuration)

reportS3Configuration_objectKeyPrefix :: Lens.Lens' ReportS3Configuration (Prelude.Maybe Prelude.Text)
reportS3Configuration_objectKeyPrefix = Lens.lens (\ReportS3Configuration' {objectKeyPrefix} -> objectKeyPrefix) (\s@ReportS3Configuration' {} a -> s {objectKeyPrefix = a} :: ReportS3Configuration)

reportS3Configuration_bucketName :: Lens.Lens' ReportS3Configuration Prelude.Text
reportS3Configuration_bucketName = Lens.lens (\ReportS3Configuration' {bucketName} -> bucketName) (\s@ReportS3Configuration' {} a -> s {bucketName = a} :: ReportS3Configuration)

instance Data.FromJSON ReportS3Configuration where
  parseJSON =
    Data.withObject
      "ReportS3Configuration"
      ( \x ->
          ReportS3Configuration'
            Prelude.<$> (x Data..:? "EncryptionOption")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "ObjectKeyPrefix")
            Prelude.<*> (x Data..: "BucketName")
      )

instance Prelude.Hashable ReportS3Configuration where
  hashWithSalt _salt ReportS3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionOption
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` objectKeyPrefix
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData ReportS3Configuration where
  rnf ReportS3Configuration' {..} =
    Prelude.rnf encryptionOption
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf objectKeyPrefix
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON ReportS3Configuration where
  toJSON ReportS3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionOption" Data..=)
              Prelude.<$> encryptionOption,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("ObjectKeyPrefix" Data..=)
              Prelude.<$> objectKeyPrefix,
            Prelude.Just ("BucketName" Data..= bucketName)
          ]
      )

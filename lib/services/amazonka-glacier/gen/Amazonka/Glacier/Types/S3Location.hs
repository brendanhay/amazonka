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
-- Module      : Amazonka.Glacier.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.CannedACL
import Amazonka.Glacier.Types.Encryption
import Amazonka.Glacier.Types.Grant
import Amazonka.Glacier.Types.StorageClass
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the location in Amazon S3 where the select
-- job results are stored.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | A list of grants that control access to the staged results.
    accessControlList :: Prelude.Maybe [Grant],
    -- | The name of the Amazon S3 bucket where the job results are stored.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The canned access control list (ACL) to apply to the job results.
    cannedACL :: Prelude.Maybe CannedACL,
    -- | Contains information about the encryption used to store the job results
    -- in Amazon S3.
    encryption :: Prelude.Maybe Encryption,
    -- | The prefix that is prepended to the results for this request.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The storage class used to store the job results.
    storageClass :: Prelude.Maybe StorageClass,
    -- | The tag-set that is applied to the job results.
    tagging :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of metadata to store with the job results in Amazon S3.
    userMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 's3Location_accessControlList' - A list of grants that control access to the staged results.
--
-- 'bucketName', 's3Location_bucketName' - The name of the Amazon S3 bucket where the job results are stored.
--
-- 'cannedACL', 's3Location_cannedACL' - The canned access control list (ACL) to apply to the job results.
--
-- 'encryption', 's3Location_encryption' - Contains information about the encryption used to store the job results
-- in Amazon S3.
--
-- 'prefix', 's3Location_prefix' - The prefix that is prepended to the results for this request.
--
-- 'storageClass', 's3Location_storageClass' - The storage class used to store the job results.
--
-- 'tagging', 's3Location_tagging' - The tag-set that is applied to the job results.
--
-- 'userMetadata', 's3Location_userMetadata' - A map of metadata to store with the job results in Amazon S3.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { accessControlList = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      cannedACL = Prelude.Nothing,
      encryption = Prelude.Nothing,
      prefix = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      tagging = Prelude.Nothing,
      userMetadata = Prelude.Nothing
    }

-- | A list of grants that control access to the staged results.
s3Location_accessControlList :: Lens.Lens' S3Location (Prelude.Maybe [Grant])
s3Location_accessControlList = Lens.lens (\S3Location' {accessControlList} -> accessControlList) (\s@S3Location' {} a -> s {accessControlList = a} :: S3Location) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon S3 bucket where the job results are stored.
s3Location_bucketName :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | The canned access control list (ACL) to apply to the job results.
s3Location_cannedACL :: Lens.Lens' S3Location (Prelude.Maybe CannedACL)
s3Location_cannedACL = Lens.lens (\S3Location' {cannedACL} -> cannedACL) (\s@S3Location' {} a -> s {cannedACL = a} :: S3Location)

-- | Contains information about the encryption used to store the job results
-- in Amazon S3.
s3Location_encryption :: Lens.Lens' S3Location (Prelude.Maybe Encryption)
s3Location_encryption = Lens.lens (\S3Location' {encryption} -> encryption) (\s@S3Location' {} a -> s {encryption = a} :: S3Location)

-- | The prefix that is prepended to the results for this request.
s3Location_prefix :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_prefix = Lens.lens (\S3Location' {prefix} -> prefix) (\s@S3Location' {} a -> s {prefix = a} :: S3Location)

-- | The storage class used to store the job results.
s3Location_storageClass :: Lens.Lens' S3Location (Prelude.Maybe StorageClass)
s3Location_storageClass = Lens.lens (\S3Location' {storageClass} -> storageClass) (\s@S3Location' {} a -> s {storageClass = a} :: S3Location)

-- | The tag-set that is applied to the job results.
s3Location_tagging :: Lens.Lens' S3Location (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3Location_tagging = Lens.lens (\S3Location' {tagging} -> tagging) (\s@S3Location' {} a -> s {tagging = a} :: S3Location) Prelude.. Lens.mapping Lens.coerced

-- | A map of metadata to store with the job results in Amazon S3.
s3Location_userMetadata :: Lens.Lens' S3Location (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3Location_userMetadata = Lens.lens (\S3Location' {userMetadata} -> userMetadata) (\s@S3Location' {} a -> s {userMetadata = a} :: S3Location) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> ( x Data..:? "AccessControlList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "CannedACL")
            Prelude.<*> (x Data..:? "Encryption")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "StorageClass")
            Prelude.<*> (x Data..:? "Tagging" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserMetadata" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` cannedACL
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` tagging
      `Prelude.hashWithSalt` userMetadata

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf cannedACL
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf tagging
      `Prelude.seq` Prelude.rnf userMetadata

instance Data.ToJSON S3Location where
  toJSON S3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlList" Data..=)
              Prelude.<$> accessControlList,
            ("BucketName" Data..=) Prelude.<$> bucketName,
            ("CannedACL" Data..=) Prelude.<$> cannedACL,
            ("Encryption" Data..=) Prelude.<$> encryption,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("StorageClass" Data..=) Prelude.<$> storageClass,
            ("Tagging" Data..=) Prelude.<$> tagging,
            ("UserMetadata" Data..=) Prelude.<$> userMetadata
          ]
      )

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
-- Module      : Amazonka.S3.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Encryption
import Amazonka.S3.Types.Grant
import Amazonka.S3.Types.MetadataEntry
import Amazonka.S3.Types.ObjectCannedACL
import Amazonka.S3.Types.StorageClass
import Amazonka.S3.Types.Tagging

-- | Describes an Amazon S3 location that will receive the results of the
-- restore request.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | A list of grants that control access to the staged results.
    accessControlList :: Prelude.Maybe [Grant],
    -- | The canned ACL to apply to the restore results.
    cannedACL :: Prelude.Maybe ObjectCannedACL,
    encryption :: Prelude.Maybe Encryption,
    -- | The class of storage used to store the restore results.
    storageClass :: Prelude.Maybe StorageClass,
    -- | The tag-set that is applied to the restore results.
    tagging :: Prelude.Maybe Tagging,
    -- | A list of metadata to store with the restore results in S3.
    userMetadata :: Prelude.Maybe [MetadataEntry],
    -- | The name of the bucket where the restore results will be placed.
    bucketName :: BucketName,
    -- | The prefix that is prepended to the restore results for this request.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'cannedACL', 's3Location_cannedACL' - The canned ACL to apply to the restore results.
--
-- 'encryption', 's3Location_encryption' - Undocumented member.
--
-- 'storageClass', 's3Location_storageClass' - The class of storage used to store the restore results.
--
-- 'tagging', 's3Location_tagging' - The tag-set that is applied to the restore results.
--
-- 'userMetadata', 's3Location_userMetadata' - A list of metadata to store with the restore results in S3.
--
-- 'bucketName', 's3Location_bucketName' - The name of the bucket where the restore results will be placed.
--
-- 'prefix', 's3Location_prefix' - The prefix that is prepended to the restore results for this request.
newS3Location ::
  -- | 'bucketName'
  BucketName ->
  -- | 'prefix'
  Prelude.Text ->
  S3Location
newS3Location pBucketName_ pPrefix_ =
  S3Location'
    { accessControlList = Prelude.Nothing,
      cannedACL = Prelude.Nothing,
      encryption = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      tagging = Prelude.Nothing,
      userMetadata = Prelude.Nothing,
      bucketName = pBucketName_,
      prefix = pPrefix_
    }

-- | A list of grants that control access to the staged results.
s3Location_accessControlList :: Lens.Lens' S3Location (Prelude.Maybe [Grant])
s3Location_accessControlList = Lens.lens (\S3Location' {accessControlList} -> accessControlList) (\s@S3Location' {} a -> s {accessControlList = a} :: S3Location) Prelude.. Lens.mapping Lens.coerced

-- | The canned ACL to apply to the restore results.
s3Location_cannedACL :: Lens.Lens' S3Location (Prelude.Maybe ObjectCannedACL)
s3Location_cannedACL = Lens.lens (\S3Location' {cannedACL} -> cannedACL) (\s@S3Location' {} a -> s {cannedACL = a} :: S3Location)

-- | Undocumented member.
s3Location_encryption :: Lens.Lens' S3Location (Prelude.Maybe Encryption)
s3Location_encryption = Lens.lens (\S3Location' {encryption} -> encryption) (\s@S3Location' {} a -> s {encryption = a} :: S3Location)

-- | The class of storage used to store the restore results.
s3Location_storageClass :: Lens.Lens' S3Location (Prelude.Maybe StorageClass)
s3Location_storageClass = Lens.lens (\S3Location' {storageClass} -> storageClass) (\s@S3Location' {} a -> s {storageClass = a} :: S3Location)

-- | The tag-set that is applied to the restore results.
s3Location_tagging :: Lens.Lens' S3Location (Prelude.Maybe Tagging)
s3Location_tagging = Lens.lens (\S3Location' {tagging} -> tagging) (\s@S3Location' {} a -> s {tagging = a} :: S3Location)

-- | A list of metadata to store with the restore results in S3.
s3Location_userMetadata :: Lens.Lens' S3Location (Prelude.Maybe [MetadataEntry])
s3Location_userMetadata = Lens.lens (\S3Location' {userMetadata} -> userMetadata) (\s@S3Location' {} a -> s {userMetadata = a} :: S3Location) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bucket where the restore results will be placed.
s3Location_bucketName :: Lens.Lens' S3Location BucketName
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | The prefix that is prepended to the restore results for this request.
s3Location_prefix :: Lens.Lens' S3Location Prelude.Text
s3Location_prefix = Lens.lens (\S3Location' {prefix} -> prefix) (\s@S3Location' {} a -> s {prefix = a} :: S3Location)

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` cannedACL
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` tagging
      `Prelude.hashWithSalt` userMetadata
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf cannedACL
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf tagging
      `Prelude.seq` Prelude.rnf userMetadata
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf prefix

instance Data.ToXML S3Location where
  toXML S3Location' {..} =
    Prelude.mconcat
      [ "AccessControlList"
          Data.@= Data.toXML
            ( Data.toXMLList "Grant"
                Prelude.<$> accessControlList
            ),
        "CannedACL" Data.@= cannedACL,
        "Encryption" Data.@= encryption,
        "StorageClass" Data.@= storageClass,
        "Tagging" Data.@= tagging,
        "UserMetadata"
          Data.@= Data.toXML
            ( Data.toXMLList "MetadataEntry"
                Prelude.<$> userMetadata
            ),
        "BucketName" Data.@= bucketName,
        "Prefix" Data.@= prefix
      ]

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
-- Module      : Network.AWS.Glacier.Types.S3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.S3Location where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.CannedACL
import Network.AWS.Glacier.Types.Encryption
import Network.AWS.Glacier.Types.Grant
import Network.AWS.Glacier.Types.StorageClass
import qualified Network.AWS.Lens as Lens

-- | Contains information about the location in Amazon S3 where the select
-- job results are stored.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The name of the Amazon S3 bucket where the job results are stored.
    bucketName :: Core.Maybe Core.Text,
    -- | The prefix that is prepended to the results for this request.
    prefix :: Core.Maybe Core.Text,
    -- | The canned access control list (ACL) to apply to the job results.
    cannedACL :: Core.Maybe CannedACL,
    -- | Contains information about the encryption used to store the job results
    -- in Amazon S3.
    encryption :: Core.Maybe Encryption,
    -- | The storage class used to store the job results.
    storageClass :: Core.Maybe StorageClass,
    -- | A map of metadata to store with the job results in Amazon S3.
    userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list of grants that control access to the staged results.
    accessControlList :: Core.Maybe [Grant],
    -- | The tag-set that is applied to the job results.
    tagging :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3Location_bucketName' - The name of the Amazon S3 bucket where the job results are stored.
--
-- 'prefix', 's3Location_prefix' - The prefix that is prepended to the results for this request.
--
-- 'cannedACL', 's3Location_cannedACL' - The canned access control list (ACL) to apply to the job results.
--
-- 'encryption', 's3Location_encryption' - Contains information about the encryption used to store the job results
-- in Amazon S3.
--
-- 'storageClass', 's3Location_storageClass' - The storage class used to store the job results.
--
-- 'userMetadata', 's3Location_userMetadata' - A map of metadata to store with the job results in Amazon S3.
--
-- 'accessControlList', 's3Location_accessControlList' - A list of grants that control access to the staged results.
--
-- 'tagging', 's3Location_tagging' - The tag-set that is applied to the job results.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { bucketName = Core.Nothing,
      prefix = Core.Nothing,
      cannedACL = Core.Nothing,
      encryption = Core.Nothing,
      storageClass = Core.Nothing,
      userMetadata = Core.Nothing,
      accessControlList = Core.Nothing,
      tagging = Core.Nothing
    }

-- | The name of the Amazon S3 bucket where the job results are stored.
s3Location_bucketName :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | The prefix that is prepended to the results for this request.
s3Location_prefix :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_prefix = Lens.lens (\S3Location' {prefix} -> prefix) (\s@S3Location' {} a -> s {prefix = a} :: S3Location)

-- | The canned access control list (ACL) to apply to the job results.
s3Location_cannedACL :: Lens.Lens' S3Location (Core.Maybe CannedACL)
s3Location_cannedACL = Lens.lens (\S3Location' {cannedACL} -> cannedACL) (\s@S3Location' {} a -> s {cannedACL = a} :: S3Location)

-- | Contains information about the encryption used to store the job results
-- in Amazon S3.
s3Location_encryption :: Lens.Lens' S3Location (Core.Maybe Encryption)
s3Location_encryption = Lens.lens (\S3Location' {encryption} -> encryption) (\s@S3Location' {} a -> s {encryption = a} :: S3Location)

-- | The storage class used to store the job results.
s3Location_storageClass :: Lens.Lens' S3Location (Core.Maybe StorageClass)
s3Location_storageClass = Lens.lens (\S3Location' {storageClass} -> storageClass) (\s@S3Location' {} a -> s {storageClass = a} :: S3Location)

-- | A map of metadata to store with the job results in Amazon S3.
s3Location_userMetadata :: Lens.Lens' S3Location (Core.Maybe (Core.HashMap Core.Text Core.Text))
s3Location_userMetadata = Lens.lens (\S3Location' {userMetadata} -> userMetadata) (\s@S3Location' {} a -> s {userMetadata = a} :: S3Location) Core.. Lens.mapping Lens._Coerce

-- | A list of grants that control access to the staged results.
s3Location_accessControlList :: Lens.Lens' S3Location (Core.Maybe [Grant])
s3Location_accessControlList = Lens.lens (\S3Location' {accessControlList} -> accessControlList) (\s@S3Location' {} a -> s {accessControlList = a} :: S3Location) Core.. Lens.mapping Lens._Coerce

-- | The tag-set that is applied to the job results.
s3Location_tagging :: Lens.Lens' S3Location (Core.Maybe (Core.HashMap Core.Text Core.Text))
s3Location_tagging = Lens.lens (\S3Location' {tagging} -> tagging) (\s@S3Location' {} a -> s {tagging = a} :: S3Location) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Core.<$> (x Core..:? "BucketName")
            Core.<*> (x Core..:? "Prefix")
            Core.<*> (x Core..:? "CannedACL")
            Core.<*> (x Core..:? "Encryption")
            Core.<*> (x Core..:? "StorageClass")
            Core.<*> (x Core..:? "UserMetadata" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccessControlList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Tagging" Core..!= Core.mempty)
      )

instance Core.Hashable S3Location

instance Core.NFData S3Location

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BucketName" Core..=) Core.<$> bucketName,
            ("Prefix" Core..=) Core.<$> prefix,
            ("CannedACL" Core..=) Core.<$> cannedACL,
            ("Encryption" Core..=) Core.<$> encryption,
            ("StorageClass" Core..=) Core.<$> storageClass,
            ("UserMetadata" Core..=) Core.<$> userMetadata,
            ("AccessControlList" Core..=)
              Core.<$> accessControlList,
            ("Tagging" Core..=) Core.<$> tagging
          ]
      )

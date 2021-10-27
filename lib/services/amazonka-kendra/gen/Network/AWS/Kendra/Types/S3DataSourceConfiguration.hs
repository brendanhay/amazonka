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
-- Module      : Network.AWS.Kendra.Types.S3DataSourceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.S3DataSourceConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.AccessControlListConfiguration
import Network.AWS.Kendra.Types.DocumentsMetadataConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for a data source to index documents
-- in an Amazon S3 bucket.
--
-- /See:/ 'newS3DataSourceConfiguration' smart constructor.
data S3DataSourceConfiguration = S3DataSourceConfiguration'
  { documentsMetadataConfiguration :: Prelude.Maybe DocumentsMetadataConfiguration,
    -- | Provides the path to the S3 bucket that contains the user context
    -- filtering files for the data source. For the format of the file, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
    accessControlListConfiguration :: Prelude.Maybe AccessControlListConfiguration,
    -- | A list of glob patterns for documents that should not be indexed. If a
    -- document that matches an inclusion prefix or inclusion pattern also
    -- matches an exclusion pattern, the document is not indexed.
    --
    -- Some
    -- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
    -- are:
    --
    -- -   /*.png , *.jpg/ will exclude all PNG and JPEG image files in a
    --     directory (files with the extensions .png and .jpg).
    --
    -- -   /*internal*/ will exclude all files in a directory that contain
    --     \'internal\' in the file name, such as \'internal\',
    --     \'internal_only\', \'company_internal\'.
    --
    -- -   /**\/*internal*/ will exclude all internal-related files in a
    --     directory and its subdirectories.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of glob patterns for documents that should be indexed. If a
    -- document that matches an inclusion pattern also matches an exclusion
    -- pattern, the document is not indexed.
    --
    -- Some
    -- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
    -- are:
    --
    -- -   /*.txt/ will include all text files in a directory (files with the
    --     extension .txt).
    --
    -- -   /**\/*.txt/ will include all text files in a directory and its
    --     subdirectories.
    --
    -- -   /*tax*/ will include all files in a directory that contain \'tax\'
    --     in the file name, such as \'tax\', \'taxes\', \'income_tax\'.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of S3 prefixes for the documents that should be included in the
    -- index.
    inclusionPrefixes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the bucket that contains the documents.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentsMetadataConfiguration', 's3DataSourceConfiguration_documentsMetadataConfiguration' - Undocumented member.
--
-- 'accessControlListConfiguration', 's3DataSourceConfiguration_accessControlListConfiguration' - Provides the path to the S3 bucket that contains the user context
-- filtering files for the data source. For the format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
--
-- 'exclusionPatterns', 's3DataSourceConfiguration_exclusionPatterns' - A list of glob patterns for documents that should not be indexed. If a
-- document that matches an inclusion prefix or inclusion pattern also
-- matches an exclusion pattern, the document is not indexed.
--
-- Some
-- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
-- are:
--
-- -   /*.png , *.jpg/ will exclude all PNG and JPEG image files in a
--     directory (files with the extensions .png and .jpg).
--
-- -   /*internal*/ will exclude all files in a directory that contain
--     \'internal\' in the file name, such as \'internal\',
--     \'internal_only\', \'company_internal\'.
--
-- -   /**\/*internal*/ will exclude all internal-related files in a
--     directory and its subdirectories.
--
-- 'inclusionPatterns', 's3DataSourceConfiguration_inclusionPatterns' - A list of glob patterns for documents that should be indexed. If a
-- document that matches an inclusion pattern also matches an exclusion
-- pattern, the document is not indexed.
--
-- Some
-- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
-- are:
--
-- -   /*.txt/ will include all text files in a directory (files with the
--     extension .txt).
--
-- -   /**\/*.txt/ will include all text files in a directory and its
--     subdirectories.
--
-- -   /*tax*/ will include all files in a directory that contain \'tax\'
--     in the file name, such as \'tax\', \'taxes\', \'income_tax\'.
--
-- 'inclusionPrefixes', 's3DataSourceConfiguration_inclusionPrefixes' - A list of S3 prefixes for the documents that should be included in the
-- index.
--
-- 'bucketName', 's3DataSourceConfiguration_bucketName' - The name of the bucket that contains the documents.
newS3DataSourceConfiguration ::
  -- | 'bucketName'
  Prelude.Text ->
  S3DataSourceConfiguration
newS3DataSourceConfiguration pBucketName_ =
  S3DataSourceConfiguration'
    { documentsMetadataConfiguration =
        Prelude.Nothing,
      accessControlListConfiguration = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      inclusionPrefixes = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | Undocumented member.
s3DataSourceConfiguration_documentsMetadataConfiguration :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe DocumentsMetadataConfiguration)
s3DataSourceConfiguration_documentsMetadataConfiguration = Lens.lens (\S3DataSourceConfiguration' {documentsMetadataConfiguration} -> documentsMetadataConfiguration) (\s@S3DataSourceConfiguration' {} a -> s {documentsMetadataConfiguration = a} :: S3DataSourceConfiguration)

-- | Provides the path to the S3 bucket that contains the user context
-- filtering files for the data source. For the format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
s3DataSourceConfiguration_accessControlListConfiguration :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe AccessControlListConfiguration)
s3DataSourceConfiguration_accessControlListConfiguration = Lens.lens (\S3DataSourceConfiguration' {accessControlListConfiguration} -> accessControlListConfiguration) (\s@S3DataSourceConfiguration' {} a -> s {accessControlListConfiguration = a} :: S3DataSourceConfiguration)

-- | A list of glob patterns for documents that should not be indexed. If a
-- document that matches an inclusion prefix or inclusion pattern also
-- matches an exclusion pattern, the document is not indexed.
--
-- Some
-- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
-- are:
--
-- -   /*.png , *.jpg/ will exclude all PNG and JPEG image files in a
--     directory (files with the extensions .png and .jpg).
--
-- -   /*internal*/ will exclude all files in a directory that contain
--     \'internal\' in the file name, such as \'internal\',
--     \'internal_only\', \'company_internal\'.
--
-- -   /**\/*internal*/ will exclude all internal-related files in a
--     directory and its subdirectories.
s3DataSourceConfiguration_exclusionPatterns :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe [Prelude.Text])
s3DataSourceConfiguration_exclusionPatterns = Lens.lens (\S3DataSourceConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@S3DataSourceConfiguration' {} a -> s {exclusionPatterns = a} :: S3DataSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of glob patterns for documents that should be indexed. If a
-- document that matches an inclusion pattern also matches an exclusion
-- pattern, the document is not indexed.
--
-- Some
-- <https://docs.aws.amazon.com/cli/latest/reference/s3/#use-of-exclude-and-include-filters examples>
-- are:
--
-- -   /*.txt/ will include all text files in a directory (files with the
--     extension .txt).
--
-- -   /**\/*.txt/ will include all text files in a directory and its
--     subdirectories.
--
-- -   /*tax*/ will include all files in a directory that contain \'tax\'
--     in the file name, such as \'tax\', \'taxes\', \'income_tax\'.
s3DataSourceConfiguration_inclusionPatterns :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe [Prelude.Text])
s3DataSourceConfiguration_inclusionPatterns = Lens.lens (\S3DataSourceConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@S3DataSourceConfiguration' {} a -> s {inclusionPatterns = a} :: S3DataSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of S3 prefixes for the documents that should be included in the
-- index.
s3DataSourceConfiguration_inclusionPrefixes :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe [Prelude.Text])
s3DataSourceConfiguration_inclusionPrefixes = Lens.lens (\S3DataSourceConfiguration' {inclusionPrefixes} -> inclusionPrefixes) (\s@S3DataSourceConfiguration' {} a -> s {inclusionPrefixes = a} :: S3DataSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bucket that contains the documents.
s3DataSourceConfiguration_bucketName :: Lens.Lens' S3DataSourceConfiguration Prelude.Text
s3DataSourceConfiguration_bucketName = Lens.lens (\S3DataSourceConfiguration' {bucketName} -> bucketName) (\s@S3DataSourceConfiguration' {} a -> s {bucketName = a} :: S3DataSourceConfiguration)

instance Core.FromJSON S3DataSourceConfiguration where
  parseJSON =
    Core.withObject
      "S3DataSourceConfiguration"
      ( \x ->
          S3DataSourceConfiguration'
            Prelude.<$> (x Core..:? "DocumentsMetadataConfiguration")
            Prelude.<*> (x Core..:? "AccessControlListConfiguration")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionPrefixes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "BucketName")
      )

instance Prelude.Hashable S3DataSourceConfiguration

instance Prelude.NFData S3DataSourceConfiguration

instance Core.ToJSON S3DataSourceConfiguration where
  toJSON S3DataSourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DocumentsMetadataConfiguration" Core..=)
              Prelude.<$> documentsMetadataConfiguration,
            ("AccessControlListConfiguration" Core..=)
              Prelude.<$> accessControlListConfiguration,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            ("InclusionPrefixes" Core..=)
              Prelude.<$> inclusionPrefixes,
            Prelude.Just ("BucketName" Core..= bucketName)
          ]
      )

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
-- Module      : Amazonka.Kendra.Types.S3DataSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.S3DataSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AccessControlListConfiguration
import Amazonka.Kendra.Types.DocumentsMetadataConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to an Amazon S3
-- bucket.
--
-- /See:/ 'newS3DataSourceConfiguration' smart constructor.
data S3DataSourceConfiguration = S3DataSourceConfiguration'
  { -- | Provides the path to the S3 bucket that contains the user context
    -- filtering files for the data source. For the format of the file, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
    accessControlListConfiguration :: Prelude.Maybe AccessControlListConfiguration,
    documentsMetadataConfiguration :: Prelude.Maybe DocumentsMetadataConfiguration,
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
-- 'accessControlListConfiguration', 's3DataSourceConfiguration_accessControlListConfiguration' - Provides the path to the S3 bucket that contains the user context
-- filtering files for the data source. For the format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
--
-- 'documentsMetadataConfiguration', 's3DataSourceConfiguration_documentsMetadataConfiguration' - Undocumented member.
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
    { accessControlListConfiguration =
        Prelude.Nothing,
      documentsMetadataConfiguration = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      inclusionPrefixes = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | Provides the path to the S3 bucket that contains the user context
-- filtering files for the data source. For the format of the file, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/s3-acl.html Access control for S3 data sources>.
s3DataSourceConfiguration_accessControlListConfiguration :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe AccessControlListConfiguration)
s3DataSourceConfiguration_accessControlListConfiguration = Lens.lens (\S3DataSourceConfiguration' {accessControlListConfiguration} -> accessControlListConfiguration) (\s@S3DataSourceConfiguration' {} a -> s {accessControlListConfiguration = a} :: S3DataSourceConfiguration)

-- | Undocumented member.
s3DataSourceConfiguration_documentsMetadataConfiguration :: Lens.Lens' S3DataSourceConfiguration (Prelude.Maybe DocumentsMetadataConfiguration)
s3DataSourceConfiguration_documentsMetadataConfiguration = Lens.lens (\S3DataSourceConfiguration' {documentsMetadataConfiguration} -> documentsMetadataConfiguration) (\s@S3DataSourceConfiguration' {} a -> s {documentsMetadataConfiguration = a} :: S3DataSourceConfiguration)

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

instance Data.FromJSON S3DataSourceConfiguration where
  parseJSON =
    Data.withObject
      "S3DataSourceConfiguration"
      ( \x ->
          S3DataSourceConfiguration'
            Prelude.<$> (x Data..:? "AccessControlListConfiguration")
            Prelude.<*> (x Data..:? "DocumentsMetadataConfiguration")
            Prelude.<*> ( x
                            Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "InclusionPrefixes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "BucketName")
      )

instance Prelude.Hashable S3DataSourceConfiguration where
  hashWithSalt _salt S3DataSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` accessControlListConfiguration
      `Prelude.hashWithSalt` documentsMetadataConfiguration
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` inclusionPrefixes
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData S3DataSourceConfiguration where
  rnf S3DataSourceConfiguration' {..} =
    Prelude.rnf accessControlListConfiguration
      `Prelude.seq` Prelude.rnf documentsMetadataConfiguration
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf inclusionPrefixes
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON S3DataSourceConfiguration where
  toJSON S3DataSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlListConfiguration" Data..=)
              Prelude.<$> accessControlListConfiguration,
            ("DocumentsMetadataConfiguration" Data..=)
              Prelude.<$> documentsMetadataConfiguration,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("InclusionPrefixes" Data..=)
              Prelude.<$> inclusionPrefixes,
            Prelude.Just ("BucketName" Data..= bucketName)
          ]
      )

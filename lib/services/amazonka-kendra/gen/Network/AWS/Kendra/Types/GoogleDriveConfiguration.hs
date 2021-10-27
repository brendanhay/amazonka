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
-- Module      : Network.AWS.Kendra.Types.GoogleDriveConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.GoogleDriveConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for data sources that connect to
-- Google Drive.
--
-- /See:/ 'newGoogleDriveConfiguration' smart constructor.
data GoogleDriveConfiguration = GoogleDriveConfiguration'
  { -- | Defines mapping between a field in the Google Drive and a Amazon Kendra
    -- index field.
    --
    -- If you are using the console, you can define index fields when creating
    -- the mapping. If you are using the API, you must first create the field
    -- using the @UpdateIndex@ operation.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of email addresses of the users. Documents owned by these users
    -- are excluded from the index. Documents shared with excluded users are
    -- indexed unless they are excluded in another way.
    excludeUserAccounts :: Prelude.Maybe [Prelude.Text],
    -- | A list of MIME types to exclude from the index. All documents matching
    -- the specified MIME type are excluded.
    --
    -- For a list of MIME types, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
    excludeMimeTypes :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns that apply to the path on Google
    -- Drive. Items that match the pattern are excluded from the index from
    -- both shared drives and users\' My Drives. Items that don\'t match the
    -- pattern are included in the index. If an item matches both an exclusion
    -- pattern and an inclusion pattern, it is excluded from the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns that apply to path on Google
    -- Drive. Items that match the pattern are included in the index from both
    -- shared drives and users\' My Drives. Items that don\'t match the pattern
    -- are excluded from the index. If an item matches both an inclusion
    -- pattern and an exclusion pattern, it is excluded from the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of identifiers or shared drives to exclude from the index. All
    -- files and folders stored on the shared drive are excluded.
    excludeSharedDrives :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of a Secrets Managersecret that contains
    -- the credentials required to connect to Google Drive. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
    secretArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GoogleDriveConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldMappings', 'googleDriveConfiguration_fieldMappings' - Defines mapping between a field in the Google Drive and a Amazon Kendra
-- index field.
--
-- If you are using the console, you can define index fields when creating
-- the mapping. If you are using the API, you must first create the field
-- using the @UpdateIndex@ operation.
--
-- 'excludeUserAccounts', 'googleDriveConfiguration_excludeUserAccounts' - A list of email addresses of the users. Documents owned by these users
-- are excluded from the index. Documents shared with excluded users are
-- indexed unless they are excluded in another way.
--
-- 'excludeMimeTypes', 'googleDriveConfiguration_excludeMimeTypes' - A list of MIME types to exclude from the index. All documents matching
-- the specified MIME type are excluded.
--
-- For a list of MIME types, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
--
-- 'exclusionPatterns', 'googleDriveConfiguration_exclusionPatterns' - A list of regular expression patterns that apply to the path on Google
-- Drive. Items that match the pattern are excluded from the index from
-- both shared drives and users\' My Drives. Items that don\'t match the
-- pattern are included in the index. If an item matches both an exclusion
-- pattern and an inclusion pattern, it is excluded from the index.
--
-- 'inclusionPatterns', 'googleDriveConfiguration_inclusionPatterns' - A list of regular expression patterns that apply to path on Google
-- Drive. Items that match the pattern are included in the index from both
-- shared drives and users\' My Drives. Items that don\'t match the pattern
-- are excluded from the index. If an item matches both an inclusion
-- pattern and an exclusion pattern, it is excluded from the index.
--
-- 'excludeSharedDrives', 'googleDriveConfiguration_excludeSharedDrives' - A list of identifiers or shared drives to exclude from the index. All
-- files and folders stored on the shared drive are excluded.
--
-- 'secretArn', 'googleDriveConfiguration_secretArn' - The Amazon Resource Name (ARN) of a Secrets Managersecret that contains
-- the credentials required to connect to Google Drive. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
newGoogleDriveConfiguration ::
  -- | 'secretArn'
  Prelude.Text ->
  GoogleDriveConfiguration
newGoogleDriveConfiguration pSecretArn_ =
  GoogleDriveConfiguration'
    { fieldMappings =
        Prelude.Nothing,
      excludeUserAccounts = Prelude.Nothing,
      excludeMimeTypes = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      excludeSharedDrives = Prelude.Nothing,
      secretArn = pSecretArn_
    }

-- | Defines mapping between a field in the Google Drive and a Amazon Kendra
-- index field.
--
-- If you are using the console, you can define index fields when creating
-- the mapping. If you are using the API, you must first create the field
-- using the @UpdateIndex@ operation.
googleDriveConfiguration_fieldMappings :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
googleDriveConfiguration_fieldMappings = Lens.lens (\GoogleDriveConfiguration' {fieldMappings} -> fieldMappings) (\s@GoogleDriveConfiguration' {} a -> s {fieldMappings = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of email addresses of the users. Documents owned by these users
-- are excluded from the index. Documents shared with excluded users are
-- indexed unless they are excluded in another way.
googleDriveConfiguration_excludeUserAccounts :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_excludeUserAccounts = Lens.lens (\GoogleDriveConfiguration' {excludeUserAccounts} -> excludeUserAccounts) (\s@GoogleDriveConfiguration' {} a -> s {excludeUserAccounts = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of MIME types to exclude from the index. All documents matching
-- the specified MIME type are excluded.
--
-- For a list of MIME types, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
googleDriveConfiguration_excludeMimeTypes :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_excludeMimeTypes = Lens.lens (\GoogleDriveConfiguration' {excludeMimeTypes} -> excludeMimeTypes) (\s@GoogleDriveConfiguration' {} a -> s {excludeMimeTypes = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns that apply to the path on Google
-- Drive. Items that match the pattern are excluded from the index from
-- both shared drives and users\' My Drives. Items that don\'t match the
-- pattern are included in the index. If an item matches both an exclusion
-- pattern and an inclusion pattern, it is excluded from the index.
googleDriveConfiguration_exclusionPatterns :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_exclusionPatterns = Lens.lens (\GoogleDriveConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@GoogleDriveConfiguration' {} a -> s {exclusionPatterns = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns that apply to path on Google
-- Drive. Items that match the pattern are included in the index from both
-- shared drives and users\' My Drives. Items that don\'t match the pattern
-- are excluded from the index. If an item matches both an inclusion
-- pattern and an exclusion pattern, it is excluded from the index.
googleDriveConfiguration_inclusionPatterns :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_inclusionPatterns = Lens.lens (\GoogleDriveConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@GoogleDriveConfiguration' {} a -> s {inclusionPatterns = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of identifiers or shared drives to exclude from the index. All
-- files and folders stored on the shared drive are excluded.
googleDriveConfiguration_excludeSharedDrives :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_excludeSharedDrives = Lens.lens (\GoogleDriveConfiguration' {excludeSharedDrives} -> excludeSharedDrives) (\s@GoogleDriveConfiguration' {} a -> s {excludeSharedDrives = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a Secrets Managersecret that contains
-- the credentials required to connect to Google Drive. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/data-source-google-drive.html Using a Google Workspace Drive data source>.
googleDriveConfiguration_secretArn :: Lens.Lens' GoogleDriveConfiguration Prelude.Text
googleDriveConfiguration_secretArn = Lens.lens (\GoogleDriveConfiguration' {secretArn} -> secretArn) (\s@GoogleDriveConfiguration' {} a -> s {secretArn = a} :: GoogleDriveConfiguration)

instance Core.FromJSON GoogleDriveConfiguration where
  parseJSON =
    Core.withObject
      "GoogleDriveConfiguration"
      ( \x ->
          GoogleDriveConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> ( x Core..:? "ExcludeUserAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExcludeMimeTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExcludeSharedDrives"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable GoogleDriveConfiguration

instance Prelude.NFData GoogleDriveConfiguration

instance Core.ToJSON GoogleDriveConfiguration where
  toJSON GoogleDriveConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("ExcludeUserAccounts" Core..=)
              Prelude.<$> excludeUserAccounts,
            ("ExcludeMimeTypes" Core..=)
              Prelude.<$> excludeMimeTypes,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            ("ExcludeSharedDrives" Core..=)
              Prelude.<$> excludeSharedDrives,
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )

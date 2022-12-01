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
-- Module      : Amazonka.Kendra.Types.GoogleDriveConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.GoogleDriveConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Google Drive as
-- your data source.
--
-- /See:/ 'newGoogleDriveConfiguration' smart constructor.
data GoogleDriveConfiguration = GoogleDriveConfiguration'
  { -- | A list of regular expression patterns to include certain items in your
    -- Google Drive, including shared drives and users\' My Drives. Items that
    -- match the patterns are included in the index. Items that don\'t match
    -- the patterns are excluded from the index. If an item matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the item isn\'t included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | Maps Google Drive data source attributes or field names to Amazon Kendra
    -- index field names. To create custom fields, use the @UpdateIndex@ API
    -- before you map to Google Drive fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Google Drive data source field names must exist in your Google Drive
    -- custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of identifiers or shared drives to exclude from the index. All
    -- files and folders stored on the shared drive are excluded.
    excludeSharedDrives :: Prelude.Maybe [Prelude.Text],
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
    -- | A list of regular expression patterns to exclude certain items in your
    -- Google Drive, including shared drives and users\' My Drives. Items that
    -- match the patterns are excluded from the index. Items that don\'t match
    -- the patterns are included in the index. If an item matches both an
    -- inclusion and exclusion pattern, the exclusion pattern takes precedence
    -- and the item isn\'t included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
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
-- 'inclusionPatterns', 'googleDriveConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain items in your
-- Google Drive, including shared drives and users\' My Drives. Items that
-- match the patterns are included in the index. Items that don\'t match
-- the patterns are excluded from the index. If an item matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the item isn\'t included in the index.
--
-- 'fieldMappings', 'googleDriveConfiguration_fieldMappings' - Maps Google Drive data source attributes or field names to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Google Drive fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Google Drive data source field names must exist in your Google Drive
-- custom metadata.
--
-- 'excludeSharedDrives', 'googleDriveConfiguration_excludeSharedDrives' - A list of identifiers or shared drives to exclude from the index. All
-- files and folders stored on the shared drive are excluded.
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
-- 'exclusionPatterns', 'googleDriveConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain items in your
-- Google Drive, including shared drives and users\' My Drives. Items that
-- match the patterns are excluded from the index. Items that don\'t match
-- the patterns are included in the index. If an item matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the item isn\'t included in the index.
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
    { inclusionPatterns =
        Prelude.Nothing,
      fieldMappings = Prelude.Nothing,
      excludeSharedDrives = Prelude.Nothing,
      excludeUserAccounts = Prelude.Nothing,
      excludeMimeTypes = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      secretArn = pSecretArn_
    }

-- | A list of regular expression patterns to include certain items in your
-- Google Drive, including shared drives and users\' My Drives. Items that
-- match the patterns are included in the index. Items that don\'t match
-- the patterns are excluded from the index. If an item matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the item isn\'t included in the index.
googleDriveConfiguration_inclusionPatterns :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_inclusionPatterns = Lens.lens (\GoogleDriveConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@GoogleDriveConfiguration' {} a -> s {inclusionPatterns = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Maps Google Drive data source attributes or field names to Amazon Kendra
-- index field names. To create custom fields, use the @UpdateIndex@ API
-- before you map to Google Drive fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Google Drive data source field names must exist in your Google Drive
-- custom metadata.
googleDriveConfiguration_fieldMappings :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
googleDriveConfiguration_fieldMappings = Lens.lens (\GoogleDriveConfiguration' {fieldMappings} -> fieldMappings) (\s@GoogleDriveConfiguration' {} a -> s {fieldMappings = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of identifiers or shared drives to exclude from the index. All
-- files and folders stored on the shared drive are excluded.
googleDriveConfiguration_excludeSharedDrives :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_excludeSharedDrives = Lens.lens (\GoogleDriveConfiguration' {excludeSharedDrives} -> excludeSharedDrives) (\s@GoogleDriveConfiguration' {} a -> s {excludeSharedDrives = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

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

-- | A list of regular expression patterns to exclude certain items in your
-- Google Drive, including shared drives and users\' My Drives. Items that
-- match the patterns are excluded from the index. Items that don\'t match
-- the patterns are included in the index. If an item matches both an
-- inclusion and exclusion pattern, the exclusion pattern takes precedence
-- and the item isn\'t included in the index.
googleDriveConfiguration_exclusionPatterns :: Lens.Lens' GoogleDriveConfiguration (Prelude.Maybe [Prelude.Text])
googleDriveConfiguration_exclusionPatterns = Lens.lens (\GoogleDriveConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@GoogleDriveConfiguration' {} a -> s {exclusionPatterns = a} :: GoogleDriveConfiguration) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FieldMappings")
            Prelude.<*> ( x Core..:? "ExcludeSharedDrives"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExcludeUserAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExcludeMimeTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "SecretArn")
      )

instance Prelude.Hashable GoogleDriveConfiguration where
  hashWithSalt _salt GoogleDriveConfiguration' {..} =
    _salt `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` excludeSharedDrives
      `Prelude.hashWithSalt` excludeUserAccounts
      `Prelude.hashWithSalt` excludeMimeTypes
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` secretArn

instance Prelude.NFData GoogleDriveConfiguration where
  rnf GoogleDriveConfiguration' {..} =
    Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf excludeSharedDrives
      `Prelude.seq` Prelude.rnf excludeUserAccounts
      `Prelude.seq` Prelude.rnf excludeMimeTypes
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf secretArn

instance Core.ToJSON GoogleDriveConfiguration where
  toJSON GoogleDriveConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("ExcludeSharedDrives" Core..=)
              Prelude.<$> excludeSharedDrives,
            ("ExcludeUserAccounts" Core..=)
              Prelude.<$> excludeUserAccounts,
            ("ExcludeMimeTypes" Core..=)
              Prelude.<$> excludeMimeTypes,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            Prelude.Just ("SecretArn" Core..= secretArn)
          ]
      )

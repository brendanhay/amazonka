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
-- Module      : Amazonka.CodeDeploy.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.S3Location where

import Amazonka.CodeDeploy.Types.BundleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of application artifacts stored in Amazon
-- S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The name of the Amazon S3 bucket where the application revision is
    -- stored.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The file type of the application revision. Must be one of the following:
    --
    -- -   @tar@: A tar archive file.
    --
    -- -   @tgz@: A compressed tar archive file.
    --
    -- -   @zip@: A zip archive file.
    bundleType :: Prelude.Maybe BundleType,
    -- | The ETag of the Amazon S3 object that represents the bundled artifacts
    -- for the application revision.
    --
    -- If the ETag is not specified as an input parameter, ETag validation of
    -- the object is skipped.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 object that represents the bundled artifacts
    -- for the application revision.
    key :: Prelude.Maybe Prelude.Text,
    -- | A specific version of the Amazon S3 object that represents the bundled
    -- artifacts for the application revision.
    --
    -- If the version is not specified, the system uses the most recent version
    -- by default.
    version :: Prelude.Maybe Prelude.Text
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
-- 'bucket', 's3Location_bucket' - The name of the Amazon S3 bucket where the application revision is
-- stored.
--
-- 'bundleType', 's3Location_bundleType' - The file type of the application revision. Must be one of the following:
--
-- -   @tar@: A tar archive file.
--
-- -   @tgz@: A compressed tar archive file.
--
-- -   @zip@: A zip archive file.
--
-- 'eTag', 's3Location_eTag' - The ETag of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of
-- the object is skipped.
--
-- 'key', 's3Location_key' - The name of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- 'version', 's3Location_version' - A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version
-- by default.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { bucket = Prelude.Nothing,
      bundleType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      key = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket where the application revision is
-- stored.
s3Location_bucket :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

-- | The file type of the application revision. Must be one of the following:
--
-- -   @tar@: A tar archive file.
--
-- -   @tgz@: A compressed tar archive file.
--
-- -   @zip@: A zip archive file.
s3Location_bundleType :: Lens.Lens' S3Location (Prelude.Maybe BundleType)
s3Location_bundleType = Lens.lens (\S3Location' {bundleType} -> bundleType) (\s@S3Location' {} a -> s {bundleType = a} :: S3Location)

-- | The ETag of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of
-- the object is skipped.
s3Location_eTag :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_eTag = Lens.lens (\S3Location' {eTag} -> eTag) (\s@S3Location' {} a -> s {eTag = a} :: S3Location)

-- | The name of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
s3Location_key :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

-- | A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version
-- by default.
s3Location_version :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_version = Lens.lens (\S3Location' {version} -> version) (\s@S3Location' {} a -> s {version = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..:? "bucket")
            Prelude.<*> (x Data..:? "bundleType")
            Prelude.<*> (x Data..:? "eTag")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` bundleType
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` version

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf bundleType
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON S3Location where
  toJSON S3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("bundleType" Data..=) Prelude.<$> bundleType,
            ("eTag" Data..=) Prelude.<$> eTag,
            ("key" Data..=) Prelude.<$> key,
            ("version" Data..=) Prelude.<$> version
          ]
      )

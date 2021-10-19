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
-- Module      : Network.AWS.CodeDeploy.Types.S3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.S3Location where

import Network.AWS.CodeDeploy.Types.BundleType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the location of application artifacts stored in Amazon
-- S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The file type of the application revision. Must be one of the following:
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
    -- | The name of the Amazon S3 bucket where the application revision is
    -- stored.
    bucket :: Prelude.Maybe Prelude.Text,
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
-- 'bucket', 's3Location_bucket' - The name of the Amazon S3 bucket where the application revision is
-- stored.
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
    { bundleType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      bucket = Prelude.Nothing,
      key = Prelude.Nothing,
      version = Prelude.Nothing
    }

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

-- | The name of the Amazon S3 bucket where the application revision is
-- stored.
s3Location_bucket :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

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

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Core..:? "bundleType")
            Prelude.<*> (x Core..:? "eTag")
            Prelude.<*> (x Core..:? "bucket")
            Prelude.<*> (x Core..:? "key")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable S3Location

instance Prelude.NFData S3Location

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bundleType" Core..=) Prelude.<$> bundleType,
            ("eTag" Core..=) Prelude.<$> eTag,
            ("bucket" Core..=) Prelude.<$> bucket,
            ("key" Core..=) Prelude.<$> key,
            ("version" Core..=) Prelude.<$> version
          ]
      )

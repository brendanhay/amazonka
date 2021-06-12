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

-- | Information about the location of application artifacts stored in Amazon
-- S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The ETag of the Amazon S3 object that represents the bundled artifacts
    -- for the application revision.
    --
    -- If the ETag is not specified as an input parameter, ETag validation of
    -- the object is skipped.
    eTag :: Core.Maybe Core.Text,
    -- | The name of the Amazon S3 object that represents the bundled artifacts
    -- for the application revision.
    key :: Core.Maybe Core.Text,
    -- | The file type of the application revision. Must be one of the following:
    --
    -- -   @tar@: A tar archive file.
    --
    -- -   @tgz@: A compressed tar archive file.
    --
    -- -   @zip@: A zip archive file.
    bundleType :: Core.Maybe BundleType,
    -- | A specific version of the Amazon S3 object that represents the bundled
    -- artifacts for the application revision.
    --
    -- If the version is not specified, the system uses the most recent version
    -- by default.
    version :: Core.Maybe Core.Text,
    -- | The name of the Amazon S3 bucket where the application revision is
    -- stored.
    bucket :: Core.Maybe Core.Text
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
-- 'eTag', 's3Location_eTag' - The ETag of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of
-- the object is skipped.
--
-- 'key', 's3Location_key' - The name of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- 'bundleType', 's3Location_bundleType' - The file type of the application revision. Must be one of the following:
--
-- -   @tar@: A tar archive file.
--
-- -   @tgz@: A compressed tar archive file.
--
-- -   @zip@: A zip archive file.
--
-- 'version', 's3Location_version' - A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version
-- by default.
--
-- 'bucket', 's3Location_bucket' - The name of the Amazon S3 bucket where the application revision is
-- stored.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { eTag = Core.Nothing,
      key = Core.Nothing,
      bundleType = Core.Nothing,
      version = Core.Nothing,
      bucket = Core.Nothing
    }

-- | The ETag of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of
-- the object is skipped.
s3Location_eTag :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_eTag = Lens.lens (\S3Location' {eTag} -> eTag) (\s@S3Location' {} a -> s {eTag = a} :: S3Location)

-- | The name of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
s3Location_key :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_key = Lens.lens (\S3Location' {key} -> key) (\s@S3Location' {} a -> s {key = a} :: S3Location)

-- | The file type of the application revision. Must be one of the following:
--
-- -   @tar@: A tar archive file.
--
-- -   @tgz@: A compressed tar archive file.
--
-- -   @zip@: A zip archive file.
s3Location_bundleType :: Lens.Lens' S3Location (Core.Maybe BundleType)
s3Location_bundleType = Lens.lens (\S3Location' {bundleType} -> bundleType) (\s@S3Location' {} a -> s {bundleType = a} :: S3Location)

-- | A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version
-- by default.
s3Location_version :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_version = Lens.lens (\S3Location' {version} -> version) (\s@S3Location' {} a -> s {version = a} :: S3Location)

-- | The name of the Amazon S3 bucket where the application revision is
-- stored.
s3Location_bucket :: Lens.Lens' S3Location (Core.Maybe Core.Text)
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Core.<$> (x Core..:? "eTag")
            Core.<*> (x Core..:? "key")
            Core.<*> (x Core..:? "bundleType")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "bucket")
      )

instance Core.Hashable S3Location

instance Core.NFData S3Location

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Core.catMaybes
          [ ("eTag" Core..=) Core.<$> eTag,
            ("key" Core..=) Core.<$> key,
            ("bundleType" Core..=) Core.<$> bundleType,
            ("version" Core..=) Core.<$> version,
            ("bucket" Core..=) Core.<$> bucket
          ]
      )

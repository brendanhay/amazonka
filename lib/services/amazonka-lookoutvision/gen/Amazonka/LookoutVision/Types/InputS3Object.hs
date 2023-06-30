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
-- Module      : Amazonka.LookoutVision.Types.InputS3Object
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.InputS3Object where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 Location information for an input manifest file.
--
-- /See:/ 'newInputS3Object' smart constructor.
data InputS3Object = InputS3Object'
  { -- | The version ID of the bucket.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket that contains the manifest.
    bucket :: Prelude.Text,
    -- | The name and location of the manifest file withiin the bucket.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputS3Object' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'inputS3Object_versionId' - The version ID of the bucket.
--
-- 'bucket', 'inputS3Object_bucket' - The Amazon S3 bucket that contains the manifest.
--
-- 'key', 'inputS3Object_key' - The name and location of the manifest file withiin the bucket.
newInputS3Object ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  InputS3Object
newInputS3Object pBucket_ pKey_ =
  InputS3Object'
    { versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The version ID of the bucket.
inputS3Object_versionId :: Lens.Lens' InputS3Object (Prelude.Maybe Prelude.Text)
inputS3Object_versionId = Lens.lens (\InputS3Object' {versionId} -> versionId) (\s@InputS3Object' {} a -> s {versionId = a} :: InputS3Object)

-- | The Amazon S3 bucket that contains the manifest.
inputS3Object_bucket :: Lens.Lens' InputS3Object Prelude.Text
inputS3Object_bucket = Lens.lens (\InputS3Object' {bucket} -> bucket) (\s@InputS3Object' {} a -> s {bucket = a} :: InputS3Object)

-- | The name and location of the manifest file withiin the bucket.
inputS3Object_key :: Lens.Lens' InputS3Object Prelude.Text
inputS3Object_key = Lens.lens (\InputS3Object' {key} -> key) (\s@InputS3Object' {} a -> s {key = a} :: InputS3Object)

instance Prelude.Hashable InputS3Object where
  hashWithSalt _salt InputS3Object' {..} =
    _salt
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData InputS3Object where
  rnf InputS3Object' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON InputS3Object where
  toJSON InputS3Object' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VersionId" Data..=) Prelude.<$> versionId,
            Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("Key" Data..= key)
          ]
      )

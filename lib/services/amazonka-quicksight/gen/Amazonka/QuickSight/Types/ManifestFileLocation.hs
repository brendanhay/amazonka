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
-- Module      : Amazonka.QuickSight.Types.ManifestFileLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ManifestFileLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 manifest file location.
--
-- /See:/ 'newManifestFileLocation' smart constructor.
data ManifestFileLocation = ManifestFileLocation'
  { -- | Amazon S3 bucket.
    bucket :: Prelude.Text,
    -- | Amazon S3 key that identifies an object.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManifestFileLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'manifestFileLocation_bucket' - Amazon S3 bucket.
--
-- 'key', 'manifestFileLocation_key' - Amazon S3 key that identifies an object.
newManifestFileLocation ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  ManifestFileLocation
newManifestFileLocation pBucket_ pKey_ =
  ManifestFileLocation'
    { bucket = pBucket_,
      key = pKey_
    }

-- | Amazon S3 bucket.
manifestFileLocation_bucket :: Lens.Lens' ManifestFileLocation Prelude.Text
manifestFileLocation_bucket = Lens.lens (\ManifestFileLocation' {bucket} -> bucket) (\s@ManifestFileLocation' {} a -> s {bucket = a} :: ManifestFileLocation)

-- | Amazon S3 key that identifies an object.
manifestFileLocation_key :: Lens.Lens' ManifestFileLocation Prelude.Text
manifestFileLocation_key = Lens.lens (\ManifestFileLocation' {key} -> key) (\s@ManifestFileLocation' {} a -> s {key = a} :: ManifestFileLocation)

instance Data.FromJSON ManifestFileLocation where
  parseJSON =
    Data.withObject
      "ManifestFileLocation"
      ( \x ->
          ManifestFileLocation'
            Prelude.<$> (x Data..: "Bucket") Prelude.<*> (x Data..: "Key")
      )

instance Prelude.Hashable ManifestFileLocation where
  hashWithSalt _salt ManifestFileLocation' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData ManifestFileLocation where
  rnf ManifestFileLocation' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ManifestFileLocation where
  toJSON ManifestFileLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("Key" Data..= key)
          ]
      )

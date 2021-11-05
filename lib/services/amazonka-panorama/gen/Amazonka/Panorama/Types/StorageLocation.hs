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
-- Module      : Amazonka.Panorama.Types.StorageLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.StorageLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A storage location.
--
-- /See:/ 'newStorageLocation' smart constructor.
data StorageLocation = StorageLocation'
  { -- | The location\'s bucket.
    bucket :: Prelude.Text,
    -- | The location\'s repo prefix.
    repoPrefixLocation :: Prelude.Text,
    -- | The location\'s generated prefix.
    generatedPrefixLocation :: Prelude.Text,
    -- | The location\'s binary prefix.
    binaryPrefixLocation :: Prelude.Text,
    -- | The location\'s manifest prefix.
    manifestPrefixLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'storageLocation_bucket' - The location\'s bucket.
--
-- 'repoPrefixLocation', 'storageLocation_repoPrefixLocation' - The location\'s repo prefix.
--
-- 'generatedPrefixLocation', 'storageLocation_generatedPrefixLocation' - The location\'s generated prefix.
--
-- 'binaryPrefixLocation', 'storageLocation_binaryPrefixLocation' - The location\'s binary prefix.
--
-- 'manifestPrefixLocation', 'storageLocation_manifestPrefixLocation' - The location\'s manifest prefix.
newStorageLocation ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'repoPrefixLocation'
  Prelude.Text ->
  -- | 'generatedPrefixLocation'
  Prelude.Text ->
  -- | 'binaryPrefixLocation'
  Prelude.Text ->
  -- | 'manifestPrefixLocation'
  Prelude.Text ->
  StorageLocation
newStorageLocation
  pBucket_
  pRepoPrefixLocation_
  pGeneratedPrefixLocation_
  pBinaryPrefixLocation_
  pManifestPrefixLocation_ =
    StorageLocation'
      { bucket = pBucket_,
        repoPrefixLocation = pRepoPrefixLocation_,
        generatedPrefixLocation = pGeneratedPrefixLocation_,
        binaryPrefixLocation = pBinaryPrefixLocation_,
        manifestPrefixLocation = pManifestPrefixLocation_
      }

-- | The location\'s bucket.
storageLocation_bucket :: Lens.Lens' StorageLocation Prelude.Text
storageLocation_bucket = Lens.lens (\StorageLocation' {bucket} -> bucket) (\s@StorageLocation' {} a -> s {bucket = a} :: StorageLocation)

-- | The location\'s repo prefix.
storageLocation_repoPrefixLocation :: Lens.Lens' StorageLocation Prelude.Text
storageLocation_repoPrefixLocation = Lens.lens (\StorageLocation' {repoPrefixLocation} -> repoPrefixLocation) (\s@StorageLocation' {} a -> s {repoPrefixLocation = a} :: StorageLocation)

-- | The location\'s generated prefix.
storageLocation_generatedPrefixLocation :: Lens.Lens' StorageLocation Prelude.Text
storageLocation_generatedPrefixLocation = Lens.lens (\StorageLocation' {generatedPrefixLocation} -> generatedPrefixLocation) (\s@StorageLocation' {} a -> s {generatedPrefixLocation = a} :: StorageLocation)

-- | The location\'s binary prefix.
storageLocation_binaryPrefixLocation :: Lens.Lens' StorageLocation Prelude.Text
storageLocation_binaryPrefixLocation = Lens.lens (\StorageLocation' {binaryPrefixLocation} -> binaryPrefixLocation) (\s@StorageLocation' {} a -> s {binaryPrefixLocation = a} :: StorageLocation)

-- | The location\'s manifest prefix.
storageLocation_manifestPrefixLocation :: Lens.Lens' StorageLocation Prelude.Text
storageLocation_manifestPrefixLocation = Lens.lens (\StorageLocation' {manifestPrefixLocation} -> manifestPrefixLocation) (\s@StorageLocation' {} a -> s {manifestPrefixLocation = a} :: StorageLocation)

instance Core.FromJSON StorageLocation where
  parseJSON =
    Core.withObject
      "StorageLocation"
      ( \x ->
          StorageLocation'
            Prelude.<$> (x Core..: "Bucket")
            Prelude.<*> (x Core..: "RepoPrefixLocation")
            Prelude.<*> (x Core..: "GeneratedPrefixLocation")
            Prelude.<*> (x Core..: "BinaryPrefixLocation")
            Prelude.<*> (x Core..: "ManifestPrefixLocation")
      )

instance Prelude.Hashable StorageLocation

instance Prelude.NFData StorageLocation

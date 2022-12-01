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
-- Module      : Amazonka.DataExchange.Types.S3SnapshotAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.S3SnapshotAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The S3 object that is the asset.
--
-- /See:/ 'newS3SnapshotAsset' smart constructor.
data S3SnapshotAsset = S3SnapshotAsset'
  { -- | The size of the S3 object that is the object.
    size :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3SnapshotAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 's3SnapshotAsset_size' - The size of the S3 object that is the object.
newS3SnapshotAsset ::
  -- | 'size'
  Prelude.Double ->
  S3SnapshotAsset
newS3SnapshotAsset pSize_ =
  S3SnapshotAsset' {size = pSize_}

-- | The size of the S3 object that is the object.
s3SnapshotAsset_size :: Lens.Lens' S3SnapshotAsset Prelude.Double
s3SnapshotAsset_size = Lens.lens (\S3SnapshotAsset' {size} -> size) (\s@S3SnapshotAsset' {} a -> s {size = a} :: S3SnapshotAsset)

instance Core.FromJSON S3SnapshotAsset where
  parseJSON =
    Core.withObject
      "S3SnapshotAsset"
      ( \x ->
          S3SnapshotAsset' Prelude.<$> (x Core..: "Size")
      )

instance Prelude.Hashable S3SnapshotAsset where
  hashWithSalt _salt S3SnapshotAsset' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData S3SnapshotAsset where
  rnf S3SnapshotAsset' {..} = Prelude.rnf size

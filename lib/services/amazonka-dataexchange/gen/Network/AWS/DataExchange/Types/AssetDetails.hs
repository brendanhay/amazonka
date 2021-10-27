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
-- Module      : Network.AWS.DataExchange.Types.AssetDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.AssetDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types.RedshiftDataShareAsset
import Network.AWS.DataExchange.Types.S3SnapshotAsset
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the asset.
--
-- /See:/ 'newAssetDetails' smart constructor.
data AssetDetails = AssetDetails'
  { -- | The S3 object that is the asset.
    s3SnapshotAsset :: Prelude.Maybe S3SnapshotAsset,
    -- | The Amazon Redshift datashare that is the asset.
    redshiftDataShareAsset :: Prelude.Maybe RedshiftDataShareAsset
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3SnapshotAsset', 'assetDetails_s3SnapshotAsset' - The S3 object that is the asset.
--
-- 'redshiftDataShareAsset', 'assetDetails_redshiftDataShareAsset' - The Amazon Redshift datashare that is the asset.
newAssetDetails ::
  AssetDetails
newAssetDetails =
  AssetDetails'
    { s3SnapshotAsset = Prelude.Nothing,
      redshiftDataShareAsset = Prelude.Nothing
    }

-- | The S3 object that is the asset.
assetDetails_s3SnapshotAsset :: Lens.Lens' AssetDetails (Prelude.Maybe S3SnapshotAsset)
assetDetails_s3SnapshotAsset = Lens.lens (\AssetDetails' {s3SnapshotAsset} -> s3SnapshotAsset) (\s@AssetDetails' {} a -> s {s3SnapshotAsset = a} :: AssetDetails)

-- | The Amazon Redshift datashare that is the asset.
assetDetails_redshiftDataShareAsset :: Lens.Lens' AssetDetails (Prelude.Maybe RedshiftDataShareAsset)
assetDetails_redshiftDataShareAsset = Lens.lens (\AssetDetails' {redshiftDataShareAsset} -> redshiftDataShareAsset) (\s@AssetDetails' {} a -> s {redshiftDataShareAsset = a} :: AssetDetails)

instance Core.FromJSON AssetDetails where
  parseJSON =
    Core.withObject
      "AssetDetails"
      ( \x ->
          AssetDetails'
            Prelude.<$> (x Core..:? "S3SnapshotAsset")
            Prelude.<*> (x Core..:? "RedshiftDataShareAsset")
      )

instance Prelude.Hashable AssetDetails

instance Prelude.NFData AssetDetails

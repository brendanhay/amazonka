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
-- Module      : Amazonka.DataExchange.Types.AssetDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types.ApiGatewayApiAsset
import Amazonka.DataExchange.Types.RedshiftDataShareAsset
import Amazonka.DataExchange.Types.S3SnapshotAsset
import qualified Amazonka.Prelude as Prelude

-- | Information about the asset.
--
-- /See:/ 'newAssetDetails' smart constructor.
data AssetDetails = AssetDetails'
  { -- | The S3 object that is the asset.
    s3SnapshotAsset :: Prelude.Maybe S3SnapshotAsset,
    -- | Information about the API Gateway API asset.
    apiGatewayApiAsset :: Prelude.Maybe ApiGatewayApiAsset,
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
-- 'apiGatewayApiAsset', 'assetDetails_apiGatewayApiAsset' - Information about the API Gateway API asset.
--
-- 'redshiftDataShareAsset', 'assetDetails_redshiftDataShareAsset' - The Amazon Redshift datashare that is the asset.
newAssetDetails ::
  AssetDetails
newAssetDetails =
  AssetDetails'
    { s3SnapshotAsset = Prelude.Nothing,
      apiGatewayApiAsset = Prelude.Nothing,
      redshiftDataShareAsset = Prelude.Nothing
    }

-- | The S3 object that is the asset.
assetDetails_s3SnapshotAsset :: Lens.Lens' AssetDetails (Prelude.Maybe S3SnapshotAsset)
assetDetails_s3SnapshotAsset = Lens.lens (\AssetDetails' {s3SnapshotAsset} -> s3SnapshotAsset) (\s@AssetDetails' {} a -> s {s3SnapshotAsset = a} :: AssetDetails)

-- | Information about the API Gateway API asset.
assetDetails_apiGatewayApiAsset :: Lens.Lens' AssetDetails (Prelude.Maybe ApiGatewayApiAsset)
assetDetails_apiGatewayApiAsset = Lens.lens (\AssetDetails' {apiGatewayApiAsset} -> apiGatewayApiAsset) (\s@AssetDetails' {} a -> s {apiGatewayApiAsset = a} :: AssetDetails)

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
            Prelude.<*> (x Core..:? "ApiGatewayApiAsset")
            Prelude.<*> (x Core..:? "RedshiftDataShareAsset")
      )

instance Prelude.Hashable AssetDetails where
  hashWithSalt _salt AssetDetails' {..} =
    _salt `Prelude.hashWithSalt` s3SnapshotAsset
      `Prelude.hashWithSalt` apiGatewayApiAsset
      `Prelude.hashWithSalt` redshiftDataShareAsset

instance Prelude.NFData AssetDetails where
  rnf AssetDetails' {..} =
    Prelude.rnf s3SnapshotAsset
      `Prelude.seq` Prelude.rnf apiGatewayApiAsset
      `Prelude.seq` Prelude.rnf redshiftDataShareAsset

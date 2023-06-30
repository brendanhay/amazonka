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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.ApiGatewayApiAsset
import Amazonka.DataExchange.Types.LakeFormationDataPermissionAsset
import Amazonka.DataExchange.Types.RedshiftDataShareAsset
import Amazonka.DataExchange.Types.S3DataAccessAsset
import Amazonka.DataExchange.Types.S3SnapshotAsset
import qualified Amazonka.Prelude as Prelude

-- | Details about the asset.
--
-- /See:/ 'newAssetDetails' smart constructor.
data AssetDetails = AssetDetails'
  { -- | Information about the API Gateway API asset.
    apiGatewayApiAsset :: Prelude.Maybe ApiGatewayApiAsset,
    -- | The AWS Lake Formation data permission that is the asset.
    lakeFormationDataPermissionAsset :: Prelude.Maybe LakeFormationDataPermissionAsset,
    -- | The Amazon Redshift datashare that is the asset.
    redshiftDataShareAsset :: Prelude.Maybe RedshiftDataShareAsset,
    -- | The Amazon S3 data access that is the asset.
    s3DataAccessAsset :: Prelude.Maybe S3DataAccessAsset,
    -- | The Amazon S3 object that is the asset.
    s3SnapshotAsset :: Prelude.Maybe S3SnapshotAsset
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
-- 'apiGatewayApiAsset', 'assetDetails_apiGatewayApiAsset' - Information about the API Gateway API asset.
--
-- 'lakeFormationDataPermissionAsset', 'assetDetails_lakeFormationDataPermissionAsset' - The AWS Lake Formation data permission that is the asset.
--
-- 'redshiftDataShareAsset', 'assetDetails_redshiftDataShareAsset' - The Amazon Redshift datashare that is the asset.
--
-- 's3DataAccessAsset', 'assetDetails_s3DataAccessAsset' - The Amazon S3 data access that is the asset.
--
-- 's3SnapshotAsset', 'assetDetails_s3SnapshotAsset' - The Amazon S3 object that is the asset.
newAssetDetails ::
  AssetDetails
newAssetDetails =
  AssetDetails'
    { apiGatewayApiAsset = Prelude.Nothing,
      lakeFormationDataPermissionAsset = Prelude.Nothing,
      redshiftDataShareAsset = Prelude.Nothing,
      s3DataAccessAsset = Prelude.Nothing,
      s3SnapshotAsset = Prelude.Nothing
    }

-- | Information about the API Gateway API asset.
assetDetails_apiGatewayApiAsset :: Lens.Lens' AssetDetails (Prelude.Maybe ApiGatewayApiAsset)
assetDetails_apiGatewayApiAsset = Lens.lens (\AssetDetails' {apiGatewayApiAsset} -> apiGatewayApiAsset) (\s@AssetDetails' {} a -> s {apiGatewayApiAsset = a} :: AssetDetails)

-- | The AWS Lake Formation data permission that is the asset.
assetDetails_lakeFormationDataPermissionAsset :: Lens.Lens' AssetDetails (Prelude.Maybe LakeFormationDataPermissionAsset)
assetDetails_lakeFormationDataPermissionAsset = Lens.lens (\AssetDetails' {lakeFormationDataPermissionAsset} -> lakeFormationDataPermissionAsset) (\s@AssetDetails' {} a -> s {lakeFormationDataPermissionAsset = a} :: AssetDetails)

-- | The Amazon Redshift datashare that is the asset.
assetDetails_redshiftDataShareAsset :: Lens.Lens' AssetDetails (Prelude.Maybe RedshiftDataShareAsset)
assetDetails_redshiftDataShareAsset = Lens.lens (\AssetDetails' {redshiftDataShareAsset} -> redshiftDataShareAsset) (\s@AssetDetails' {} a -> s {redshiftDataShareAsset = a} :: AssetDetails)

-- | The Amazon S3 data access that is the asset.
assetDetails_s3DataAccessAsset :: Lens.Lens' AssetDetails (Prelude.Maybe S3DataAccessAsset)
assetDetails_s3DataAccessAsset = Lens.lens (\AssetDetails' {s3DataAccessAsset} -> s3DataAccessAsset) (\s@AssetDetails' {} a -> s {s3DataAccessAsset = a} :: AssetDetails)

-- | The Amazon S3 object that is the asset.
assetDetails_s3SnapshotAsset :: Lens.Lens' AssetDetails (Prelude.Maybe S3SnapshotAsset)
assetDetails_s3SnapshotAsset = Lens.lens (\AssetDetails' {s3SnapshotAsset} -> s3SnapshotAsset) (\s@AssetDetails' {} a -> s {s3SnapshotAsset = a} :: AssetDetails)

instance Data.FromJSON AssetDetails where
  parseJSON =
    Data.withObject
      "AssetDetails"
      ( \x ->
          AssetDetails'
            Prelude.<$> (x Data..:? "ApiGatewayApiAsset")
            Prelude.<*> (x Data..:? "LakeFormationDataPermissionAsset")
            Prelude.<*> (x Data..:? "RedshiftDataShareAsset")
            Prelude.<*> (x Data..:? "S3DataAccessAsset")
            Prelude.<*> (x Data..:? "S3SnapshotAsset")
      )

instance Prelude.Hashable AssetDetails where
  hashWithSalt _salt AssetDetails' {..} =
    _salt
      `Prelude.hashWithSalt` apiGatewayApiAsset
      `Prelude.hashWithSalt` lakeFormationDataPermissionAsset
      `Prelude.hashWithSalt` redshiftDataShareAsset
      `Prelude.hashWithSalt` s3DataAccessAsset
      `Prelude.hashWithSalt` s3SnapshotAsset

instance Prelude.NFData AssetDetails where
  rnf AssetDetails' {..} =
    Prelude.rnf apiGatewayApiAsset
      `Prelude.seq` Prelude.rnf lakeFormationDataPermissionAsset
      `Prelude.seq` Prelude.rnf redshiftDataShareAsset
      `Prelude.seq` Prelude.rnf s3DataAccessAsset
      `Prelude.seq` Prelude.rnf s3SnapshotAsset

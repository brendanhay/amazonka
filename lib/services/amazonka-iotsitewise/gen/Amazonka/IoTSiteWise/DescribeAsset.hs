{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.DescribeAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an asset.
module Amazonka.IoTSiteWise.DescribeAsset
  ( -- * Creating a Request
    DescribeAsset (..),
    newDescribeAsset,

    -- * Request Lenses
    describeAsset_excludeProperties,
    describeAsset_assetId,

    -- * Destructuring the Response
    DescribeAssetResponse (..),
    newDescribeAssetResponse,

    -- * Response Lenses
    describeAssetResponse_assetCompositeModels,
    describeAssetResponse_assetDescription,
    describeAssetResponse_httpStatus,
    describeAssetResponse_assetId,
    describeAssetResponse_assetArn,
    describeAssetResponse_assetName,
    describeAssetResponse_assetModelId,
    describeAssetResponse_assetProperties,
    describeAssetResponse_assetHierarchies,
    describeAssetResponse_assetCreationDate,
    describeAssetResponse_assetLastUpdateDate,
    describeAssetResponse_assetStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAsset' smart constructor.
data DescribeAsset = DescribeAsset'
  { -- | Whether or not to exclude asset properties from the response.
    excludeProperties :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the asset.
    assetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeProperties', 'describeAsset_excludeProperties' - Whether or not to exclude asset properties from the response.
--
-- 'assetId', 'describeAsset_assetId' - The ID of the asset.
newDescribeAsset ::
  -- | 'assetId'
  Prelude.Text ->
  DescribeAsset
newDescribeAsset pAssetId_ =
  DescribeAsset'
    { excludeProperties = Prelude.Nothing,
      assetId = pAssetId_
    }

-- | Whether or not to exclude asset properties from the response.
describeAsset_excludeProperties :: Lens.Lens' DescribeAsset (Prelude.Maybe Prelude.Bool)
describeAsset_excludeProperties = Lens.lens (\DescribeAsset' {excludeProperties} -> excludeProperties) (\s@DescribeAsset' {} a -> s {excludeProperties = a} :: DescribeAsset)

-- | The ID of the asset.
describeAsset_assetId :: Lens.Lens' DescribeAsset Prelude.Text
describeAsset_assetId = Lens.lens (\DescribeAsset' {assetId} -> assetId) (\s@DescribeAsset' {} a -> s {assetId = a} :: DescribeAsset)

instance Core.AWSRequest DescribeAsset where
  type
    AWSResponse DescribeAsset =
      DescribeAssetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetResponse'
            Prelude.<$> ( x Core..?> "assetCompositeModels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "assetDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "assetId")
            Prelude.<*> (x Core..:> "assetArn")
            Prelude.<*> (x Core..:> "assetName")
            Prelude.<*> (x Core..:> "assetModelId")
            Prelude.<*> ( x Core..?> "assetProperties"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "assetHierarchies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..:> "assetCreationDate")
            Prelude.<*> (x Core..:> "assetLastUpdateDate")
            Prelude.<*> (x Core..:> "assetStatus")
      )

instance Prelude.Hashable DescribeAsset where
  hashWithSalt _salt DescribeAsset' {..} =
    _salt `Prelude.hashWithSalt` excludeProperties
      `Prelude.hashWithSalt` assetId

instance Prelude.NFData DescribeAsset where
  rnf DescribeAsset' {..} =
    Prelude.rnf excludeProperties
      `Prelude.seq` Prelude.rnf assetId

instance Core.ToHeaders DescribeAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAsset where
  toPath DescribeAsset' {..} =
    Prelude.mconcat ["/assets/", Core.toBS assetId]

instance Core.ToQuery DescribeAsset where
  toQuery DescribeAsset' {..} =
    Prelude.mconcat
      ["excludeProperties" Core.=: excludeProperties]

-- | /See:/ 'newDescribeAssetResponse' smart constructor.
data DescribeAssetResponse = DescribeAssetResponse'
  { -- | The composite models for the asset.
    assetCompositeModels :: Prelude.Maybe [AssetCompositeModel],
    -- | A description for the asset.
    assetDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asset.
    assetId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
    assetArn :: Prelude.Text,
    -- | The name of the asset.
    assetName :: Prelude.Text,
    -- | The ID of the asset model that was used to create the asset.
    assetModelId :: Prelude.Text,
    -- | The list of asset properties for the asset.
    --
    -- This object doesn\'t include properties that you define in composite
    -- models. You can find composite model properties in the
    -- @assetCompositeModels@ object.
    assetProperties :: [AssetProperty],
    -- | A list of asset hierarchies that each contain a @hierarchyId@. A
    -- hierarchy specifies allowed parent\/child asset relationships.
    assetHierarchies :: [AssetHierarchy],
    -- | The date the asset was created, in Unix epoch time.
    assetCreationDate :: Core.POSIX,
    -- | The date the asset was last updated, in Unix epoch time.
    assetLastUpdateDate :: Core.POSIX,
    -- | The current status of the asset, which contains a state and any error
    -- message.
    assetStatus :: AssetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetCompositeModels', 'describeAssetResponse_assetCompositeModels' - The composite models for the asset.
--
-- 'assetDescription', 'describeAssetResponse_assetDescription' - A description for the asset.
--
-- 'httpStatus', 'describeAssetResponse_httpStatus' - The response's http status code.
--
-- 'assetId', 'describeAssetResponse_assetId' - The ID of the asset.
--
-- 'assetArn', 'describeAssetResponse_assetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
--
-- 'assetName', 'describeAssetResponse_assetName' - The name of the asset.
--
-- 'assetModelId', 'describeAssetResponse_assetModelId' - The ID of the asset model that was used to create the asset.
--
-- 'assetProperties', 'describeAssetResponse_assetProperties' - The list of asset properties for the asset.
--
-- This object doesn\'t include properties that you define in composite
-- models. You can find composite model properties in the
-- @assetCompositeModels@ object.
--
-- 'assetHierarchies', 'describeAssetResponse_assetHierarchies' - A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
--
-- 'assetCreationDate', 'describeAssetResponse_assetCreationDate' - The date the asset was created, in Unix epoch time.
--
-- 'assetLastUpdateDate', 'describeAssetResponse_assetLastUpdateDate' - The date the asset was last updated, in Unix epoch time.
--
-- 'assetStatus', 'describeAssetResponse_assetStatus' - The current status of the asset, which contains a state and any error
-- message.
newDescribeAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'assetArn'
  Prelude.Text ->
  -- | 'assetName'
  Prelude.Text ->
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'assetCreationDate'
  Prelude.UTCTime ->
  -- | 'assetLastUpdateDate'
  Prelude.UTCTime ->
  -- | 'assetStatus'
  AssetStatus ->
  DescribeAssetResponse
newDescribeAssetResponse
  pHttpStatus_
  pAssetId_
  pAssetArn_
  pAssetName_
  pAssetModelId_
  pAssetCreationDate_
  pAssetLastUpdateDate_
  pAssetStatus_ =
    DescribeAssetResponse'
      { assetCompositeModels =
          Prelude.Nothing,
        assetDescription = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        assetId = pAssetId_,
        assetArn = pAssetArn_,
        assetName = pAssetName_,
        assetModelId = pAssetModelId_,
        assetProperties = Prelude.mempty,
        assetHierarchies = Prelude.mempty,
        assetCreationDate =
          Core._Time Lens.# pAssetCreationDate_,
        assetLastUpdateDate =
          Core._Time Lens.# pAssetLastUpdateDate_,
        assetStatus = pAssetStatus_
      }

-- | The composite models for the asset.
describeAssetResponse_assetCompositeModels :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe [AssetCompositeModel])
describeAssetResponse_assetCompositeModels = Lens.lens (\DescribeAssetResponse' {assetCompositeModels} -> assetCompositeModels) (\s@DescribeAssetResponse' {} a -> s {assetCompositeModels = a} :: DescribeAssetResponse) Prelude.. Lens.mapping Lens.coerced

-- | A description for the asset.
describeAssetResponse_assetDescription :: Lens.Lens' DescribeAssetResponse (Prelude.Maybe Prelude.Text)
describeAssetResponse_assetDescription = Lens.lens (\DescribeAssetResponse' {assetDescription} -> assetDescription) (\s@DescribeAssetResponse' {} a -> s {assetDescription = a} :: DescribeAssetResponse)

-- | The response's http status code.
describeAssetResponse_httpStatus :: Lens.Lens' DescribeAssetResponse Prelude.Int
describeAssetResponse_httpStatus = Lens.lens (\DescribeAssetResponse' {httpStatus} -> httpStatus) (\s@DescribeAssetResponse' {} a -> s {httpStatus = a} :: DescribeAssetResponse)

-- | The ID of the asset.
describeAssetResponse_assetId :: Lens.Lens' DescribeAssetResponse Prelude.Text
describeAssetResponse_assetId = Lens.lens (\DescribeAssetResponse' {assetId} -> assetId) (\s@DescribeAssetResponse' {} a -> s {assetId = a} :: DescribeAssetResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
describeAssetResponse_assetArn :: Lens.Lens' DescribeAssetResponse Prelude.Text
describeAssetResponse_assetArn = Lens.lens (\DescribeAssetResponse' {assetArn} -> assetArn) (\s@DescribeAssetResponse' {} a -> s {assetArn = a} :: DescribeAssetResponse)

-- | The name of the asset.
describeAssetResponse_assetName :: Lens.Lens' DescribeAssetResponse Prelude.Text
describeAssetResponse_assetName = Lens.lens (\DescribeAssetResponse' {assetName} -> assetName) (\s@DescribeAssetResponse' {} a -> s {assetName = a} :: DescribeAssetResponse)

-- | The ID of the asset model that was used to create the asset.
describeAssetResponse_assetModelId :: Lens.Lens' DescribeAssetResponse Prelude.Text
describeAssetResponse_assetModelId = Lens.lens (\DescribeAssetResponse' {assetModelId} -> assetModelId) (\s@DescribeAssetResponse' {} a -> s {assetModelId = a} :: DescribeAssetResponse)

-- | The list of asset properties for the asset.
--
-- This object doesn\'t include properties that you define in composite
-- models. You can find composite model properties in the
-- @assetCompositeModels@ object.
describeAssetResponse_assetProperties :: Lens.Lens' DescribeAssetResponse [AssetProperty]
describeAssetResponse_assetProperties = Lens.lens (\DescribeAssetResponse' {assetProperties} -> assetProperties) (\s@DescribeAssetResponse' {} a -> s {assetProperties = a} :: DescribeAssetResponse) Prelude.. Lens.coerced

-- | A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
describeAssetResponse_assetHierarchies :: Lens.Lens' DescribeAssetResponse [AssetHierarchy]
describeAssetResponse_assetHierarchies = Lens.lens (\DescribeAssetResponse' {assetHierarchies} -> assetHierarchies) (\s@DescribeAssetResponse' {} a -> s {assetHierarchies = a} :: DescribeAssetResponse) Prelude.. Lens.coerced

-- | The date the asset was created, in Unix epoch time.
describeAssetResponse_assetCreationDate :: Lens.Lens' DescribeAssetResponse Prelude.UTCTime
describeAssetResponse_assetCreationDate = Lens.lens (\DescribeAssetResponse' {assetCreationDate} -> assetCreationDate) (\s@DescribeAssetResponse' {} a -> s {assetCreationDate = a} :: DescribeAssetResponse) Prelude.. Core._Time

-- | The date the asset was last updated, in Unix epoch time.
describeAssetResponse_assetLastUpdateDate :: Lens.Lens' DescribeAssetResponse Prelude.UTCTime
describeAssetResponse_assetLastUpdateDate = Lens.lens (\DescribeAssetResponse' {assetLastUpdateDate} -> assetLastUpdateDate) (\s@DescribeAssetResponse' {} a -> s {assetLastUpdateDate = a} :: DescribeAssetResponse) Prelude.. Core._Time

-- | The current status of the asset, which contains a state and any error
-- message.
describeAssetResponse_assetStatus :: Lens.Lens' DescribeAssetResponse AssetStatus
describeAssetResponse_assetStatus = Lens.lens (\DescribeAssetResponse' {assetStatus} -> assetStatus) (\s@DescribeAssetResponse' {} a -> s {assetStatus = a} :: DescribeAssetResponse)

instance Prelude.NFData DescribeAssetResponse where
  rnf DescribeAssetResponse' {..} =
    Prelude.rnf assetCompositeModels
      `Prelude.seq` Prelude.rnf assetDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf assetArn
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf assetProperties
      `Prelude.seq` Prelude.rnf assetHierarchies
      `Prelude.seq` Prelude.rnf assetCreationDate
      `Prelude.seq` Prelude.rnf assetLastUpdateDate
      `Prelude.seq` Prelude.rnf assetStatus

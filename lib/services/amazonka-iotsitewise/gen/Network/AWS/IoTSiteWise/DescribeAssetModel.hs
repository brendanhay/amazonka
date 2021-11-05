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
-- Module      : Amazonka.IoTSiteWise.DescribeAssetModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an asset model.
module Amazonka.IoTSiteWise.DescribeAssetModel
  ( -- * Creating a Request
    DescribeAssetModel (..),
    newDescribeAssetModel,

    -- * Request Lenses
    describeAssetModel_assetModelId,

    -- * Destructuring the Response
    DescribeAssetModelResponse (..),
    newDescribeAssetModelResponse,

    -- * Response Lenses
    describeAssetModelResponse_assetModelCompositeModels,
    describeAssetModelResponse_httpStatus,
    describeAssetModelResponse_assetModelId,
    describeAssetModelResponse_assetModelArn,
    describeAssetModelResponse_assetModelName,
    describeAssetModelResponse_assetModelDescription,
    describeAssetModelResponse_assetModelProperties,
    describeAssetModelResponse_assetModelHierarchies,
    describeAssetModelResponse_assetModelCreationDate,
    describeAssetModelResponse_assetModelLastUpdateDate,
    describeAssetModelResponse_assetModelStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAssetModel' smart constructor.
data DescribeAssetModel = DescribeAssetModel'
  { -- | The ID of the asset model.
    assetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetModelId', 'describeAssetModel_assetModelId' - The ID of the asset model.
newDescribeAssetModel ::
  -- | 'assetModelId'
  Prelude.Text ->
  DescribeAssetModel
newDescribeAssetModel pAssetModelId_ =
  DescribeAssetModel' {assetModelId = pAssetModelId_}

-- | The ID of the asset model.
describeAssetModel_assetModelId :: Lens.Lens' DescribeAssetModel Prelude.Text
describeAssetModel_assetModelId = Lens.lens (\DescribeAssetModel' {assetModelId} -> assetModelId) (\s@DescribeAssetModel' {} a -> s {assetModelId = a} :: DescribeAssetModel)

instance Core.AWSRequest DescribeAssetModel where
  type
    AWSResponse DescribeAssetModel =
      DescribeAssetModelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetModelResponse'
            Prelude.<$> ( x Core..?> "assetModelCompositeModels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "assetModelId")
            Prelude.<*> (x Core..:> "assetModelArn")
            Prelude.<*> (x Core..:> "assetModelName")
            Prelude.<*> (x Core..:> "assetModelDescription")
            Prelude.<*> ( x Core..?> "assetModelProperties"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "assetModelHierarchies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..:> "assetModelCreationDate")
            Prelude.<*> (x Core..:> "assetModelLastUpdateDate")
            Prelude.<*> (x Core..:> "assetModelStatus")
      )

instance Prelude.Hashable DescribeAssetModel

instance Prelude.NFData DescribeAssetModel

instance Core.ToHeaders DescribeAssetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeAssetModel where
  toPath DescribeAssetModel' {..} =
    Prelude.mconcat
      ["/asset-models/", Core.toBS assetModelId]

instance Core.ToQuery DescribeAssetModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssetModelResponse' smart constructor.
data DescribeAssetModelResponse = DescribeAssetModelResponse'
  { -- | The list of composite asset models for the asset model.
    assetModelCompositeModels :: Prelude.Maybe [AssetModelCompositeModel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asset model.
    assetModelId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset model, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
    assetModelArn :: Prelude.Text,
    -- | The name of the asset model.
    assetModelName :: Prelude.Text,
    -- | The asset model\'s description.
    assetModelDescription :: Prelude.Text,
    -- | The list of asset properties for the asset model.
    --
    -- This object doesn\'t include properties that you define in composite
    -- models. You can find composite model properties in the
    -- @assetModelCompositeModels@ object.
    assetModelProperties :: [AssetModelProperty],
    -- | A list of asset model hierarchies that each contain a
    -- @childAssetModelId@ and a @hierarchyId@ (named @id@). A hierarchy
    -- specifies allowed parent\/child asset relationships for an asset model.
    assetModelHierarchies :: [AssetModelHierarchy],
    -- | The date the asset model was created, in Unix epoch time.
    assetModelCreationDate :: Core.POSIX,
    -- | The date the asset model was last updated, in Unix epoch time.
    assetModelLastUpdateDate :: Core.POSIX,
    -- | The current status of the asset model, which contains a state and any
    -- error message.
    assetModelStatus :: AssetModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetModelCompositeModels', 'describeAssetModelResponse_assetModelCompositeModels' - The list of composite asset models for the asset model.
--
-- 'httpStatus', 'describeAssetModelResponse_httpStatus' - The response's http status code.
--
-- 'assetModelId', 'describeAssetModelResponse_assetModelId' - The ID of the asset model.
--
-- 'assetModelArn', 'describeAssetModelResponse_assetModelArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
--
-- 'assetModelName', 'describeAssetModelResponse_assetModelName' - The name of the asset model.
--
-- 'assetModelDescription', 'describeAssetModelResponse_assetModelDescription' - The asset model\'s description.
--
-- 'assetModelProperties', 'describeAssetModelResponse_assetModelProperties' - The list of asset properties for the asset model.
--
-- This object doesn\'t include properties that you define in composite
-- models. You can find composite model properties in the
-- @assetModelCompositeModels@ object.
--
-- 'assetModelHierarchies', 'describeAssetModelResponse_assetModelHierarchies' - A list of asset model hierarchies that each contain a
-- @childAssetModelId@ and a @hierarchyId@ (named @id@). A hierarchy
-- specifies allowed parent\/child asset relationships for an asset model.
--
-- 'assetModelCreationDate', 'describeAssetModelResponse_assetModelCreationDate' - The date the asset model was created, in Unix epoch time.
--
-- 'assetModelLastUpdateDate', 'describeAssetModelResponse_assetModelLastUpdateDate' - The date the asset model was last updated, in Unix epoch time.
--
-- 'assetModelStatus', 'describeAssetModelResponse_assetModelStatus' - The current status of the asset model, which contains a state and any
-- error message.
newDescribeAssetModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'assetModelArn'
  Prelude.Text ->
  -- | 'assetModelName'
  Prelude.Text ->
  -- | 'assetModelDescription'
  Prelude.Text ->
  -- | 'assetModelCreationDate'
  Prelude.UTCTime ->
  -- | 'assetModelLastUpdateDate'
  Prelude.UTCTime ->
  -- | 'assetModelStatus'
  AssetModelStatus ->
  DescribeAssetModelResponse
newDescribeAssetModelResponse
  pHttpStatus_
  pAssetModelId_
  pAssetModelArn_
  pAssetModelName_
  pAssetModelDescription_
  pAssetModelCreationDate_
  pAssetModelLastUpdateDate_
  pAssetModelStatus_ =
    DescribeAssetModelResponse'
      { assetModelCompositeModels =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        assetModelId = pAssetModelId_,
        assetModelArn = pAssetModelArn_,
        assetModelName = pAssetModelName_,
        assetModelDescription = pAssetModelDescription_,
        assetModelProperties = Prelude.mempty,
        assetModelHierarchies = Prelude.mempty,
        assetModelCreationDate =
          Core._Time Lens.# pAssetModelCreationDate_,
        assetModelLastUpdateDate =
          Core._Time Lens.# pAssetModelLastUpdateDate_,
        assetModelStatus = pAssetModelStatus_
      }

-- | The list of composite asset models for the asset model.
describeAssetModelResponse_assetModelCompositeModels :: Lens.Lens' DescribeAssetModelResponse (Prelude.Maybe [AssetModelCompositeModel])
describeAssetModelResponse_assetModelCompositeModels = Lens.lens (\DescribeAssetModelResponse' {assetModelCompositeModels} -> assetModelCompositeModels) (\s@DescribeAssetModelResponse' {} a -> s {assetModelCompositeModels = a} :: DescribeAssetModelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAssetModelResponse_httpStatus :: Lens.Lens' DescribeAssetModelResponse Prelude.Int
describeAssetModelResponse_httpStatus = Lens.lens (\DescribeAssetModelResponse' {httpStatus} -> httpStatus) (\s@DescribeAssetModelResponse' {} a -> s {httpStatus = a} :: DescribeAssetModelResponse)

-- | The ID of the asset model.
describeAssetModelResponse_assetModelId :: Lens.Lens' DescribeAssetModelResponse Prelude.Text
describeAssetModelResponse_assetModelId = Lens.lens (\DescribeAssetModelResponse' {assetModelId} -> assetModelId) (\s@DescribeAssetModelResponse' {} a -> s {assetModelId = a} :: DescribeAssetModelResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
describeAssetModelResponse_assetModelArn :: Lens.Lens' DescribeAssetModelResponse Prelude.Text
describeAssetModelResponse_assetModelArn = Lens.lens (\DescribeAssetModelResponse' {assetModelArn} -> assetModelArn) (\s@DescribeAssetModelResponse' {} a -> s {assetModelArn = a} :: DescribeAssetModelResponse)

-- | The name of the asset model.
describeAssetModelResponse_assetModelName :: Lens.Lens' DescribeAssetModelResponse Prelude.Text
describeAssetModelResponse_assetModelName = Lens.lens (\DescribeAssetModelResponse' {assetModelName} -> assetModelName) (\s@DescribeAssetModelResponse' {} a -> s {assetModelName = a} :: DescribeAssetModelResponse)

-- | The asset model\'s description.
describeAssetModelResponse_assetModelDescription :: Lens.Lens' DescribeAssetModelResponse Prelude.Text
describeAssetModelResponse_assetModelDescription = Lens.lens (\DescribeAssetModelResponse' {assetModelDescription} -> assetModelDescription) (\s@DescribeAssetModelResponse' {} a -> s {assetModelDescription = a} :: DescribeAssetModelResponse)

-- | The list of asset properties for the asset model.
--
-- This object doesn\'t include properties that you define in composite
-- models. You can find composite model properties in the
-- @assetModelCompositeModels@ object.
describeAssetModelResponse_assetModelProperties :: Lens.Lens' DescribeAssetModelResponse [AssetModelProperty]
describeAssetModelResponse_assetModelProperties = Lens.lens (\DescribeAssetModelResponse' {assetModelProperties} -> assetModelProperties) (\s@DescribeAssetModelResponse' {} a -> s {assetModelProperties = a} :: DescribeAssetModelResponse) Prelude.. Lens.coerced

-- | A list of asset model hierarchies that each contain a
-- @childAssetModelId@ and a @hierarchyId@ (named @id@). A hierarchy
-- specifies allowed parent\/child asset relationships for an asset model.
describeAssetModelResponse_assetModelHierarchies :: Lens.Lens' DescribeAssetModelResponse [AssetModelHierarchy]
describeAssetModelResponse_assetModelHierarchies = Lens.lens (\DescribeAssetModelResponse' {assetModelHierarchies} -> assetModelHierarchies) (\s@DescribeAssetModelResponse' {} a -> s {assetModelHierarchies = a} :: DescribeAssetModelResponse) Prelude.. Lens.coerced

-- | The date the asset model was created, in Unix epoch time.
describeAssetModelResponse_assetModelCreationDate :: Lens.Lens' DescribeAssetModelResponse Prelude.UTCTime
describeAssetModelResponse_assetModelCreationDate = Lens.lens (\DescribeAssetModelResponse' {assetModelCreationDate} -> assetModelCreationDate) (\s@DescribeAssetModelResponse' {} a -> s {assetModelCreationDate = a} :: DescribeAssetModelResponse) Prelude.. Core._Time

-- | The date the asset model was last updated, in Unix epoch time.
describeAssetModelResponse_assetModelLastUpdateDate :: Lens.Lens' DescribeAssetModelResponse Prelude.UTCTime
describeAssetModelResponse_assetModelLastUpdateDate = Lens.lens (\DescribeAssetModelResponse' {assetModelLastUpdateDate} -> assetModelLastUpdateDate) (\s@DescribeAssetModelResponse' {} a -> s {assetModelLastUpdateDate = a} :: DescribeAssetModelResponse) Prelude.. Core._Time

-- | The current status of the asset model, which contains a state and any
-- error message.
describeAssetModelResponse_assetModelStatus :: Lens.Lens' DescribeAssetModelResponse AssetModelStatus
describeAssetModelResponse_assetModelStatus = Lens.lens (\DescribeAssetModelResponse' {assetModelStatus} -> assetModelStatus) (\s@DescribeAssetModelResponse' {} a -> s {assetModelStatus = a} :: DescribeAssetModelResponse)

instance Prelude.NFData DescribeAssetModelResponse

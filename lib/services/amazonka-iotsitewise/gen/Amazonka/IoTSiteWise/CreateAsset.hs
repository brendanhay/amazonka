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
-- Module      : Amazonka.IoTSiteWise.CreateAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an asset from an existing asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/create-assets.html Creating assets>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.CreateAsset
  ( -- * Creating a Request
    CreateAsset (..),
    newCreateAsset,

    -- * Request Lenses
    createAsset_assetDescription,
    createAsset_clientToken,
    createAsset_tags,
    createAsset_assetName,
    createAsset_assetModelId,

    -- * Destructuring the Response
    CreateAssetResponse (..),
    newCreateAssetResponse,

    -- * Response Lenses
    createAssetResponse_httpStatus,
    createAssetResponse_assetId,
    createAssetResponse_assetArn,
    createAssetResponse_assetStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAsset' smart constructor.
data CreateAsset = CreateAsset'
  { -- | A description for the asset.
    assetDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the asset. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
    -- in the /IoT SiteWise User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A friendly name for the asset.
    assetName :: Prelude.Text,
    -- | The ID of the asset model from which to create the asset.
    assetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetDescription', 'createAsset_assetDescription' - A description for the asset.
--
-- 'clientToken', 'createAsset_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'tags', 'createAsset_tags' - A list of key-value pairs that contain metadata for the asset. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetName', 'createAsset_assetName' - A friendly name for the asset.
--
-- 'assetModelId', 'createAsset_assetModelId' - The ID of the asset model from which to create the asset.
newCreateAsset ::
  -- | 'assetName'
  Prelude.Text ->
  -- | 'assetModelId'
  Prelude.Text ->
  CreateAsset
newCreateAsset pAssetName_ pAssetModelId_ =
  CreateAsset'
    { assetDescription = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      assetName = pAssetName_,
      assetModelId = pAssetModelId_
    }

-- | A description for the asset.
createAsset_assetDescription :: Lens.Lens' CreateAsset (Prelude.Maybe Prelude.Text)
createAsset_assetDescription = Lens.lens (\CreateAsset' {assetDescription} -> assetDescription) (\s@CreateAsset' {} a -> s {assetDescription = a} :: CreateAsset)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createAsset_clientToken :: Lens.Lens' CreateAsset (Prelude.Maybe Prelude.Text)
createAsset_clientToken = Lens.lens (\CreateAsset' {clientToken} -> clientToken) (\s@CreateAsset' {} a -> s {clientToken = a} :: CreateAsset)

-- | A list of key-value pairs that contain metadata for the asset. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
createAsset_tags :: Lens.Lens' CreateAsset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAsset_tags = Lens.lens (\CreateAsset' {tags} -> tags) (\s@CreateAsset' {} a -> s {tags = a} :: CreateAsset) Prelude.. Lens.mapping Lens.coerced

-- | A friendly name for the asset.
createAsset_assetName :: Lens.Lens' CreateAsset Prelude.Text
createAsset_assetName = Lens.lens (\CreateAsset' {assetName} -> assetName) (\s@CreateAsset' {} a -> s {assetName = a} :: CreateAsset)

-- | The ID of the asset model from which to create the asset.
createAsset_assetModelId :: Lens.Lens' CreateAsset Prelude.Text
createAsset_assetModelId = Lens.lens (\CreateAsset' {assetModelId} -> assetModelId) (\s@CreateAsset' {} a -> s {assetModelId = a} :: CreateAsset)

instance Core.AWSRequest CreateAsset where
  type AWSResponse CreateAsset = CreateAssetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetId")
            Prelude.<*> (x Data..:> "assetArn")
            Prelude.<*> (x Data..:> "assetStatus")
      )

instance Prelude.Hashable CreateAsset where
  hashWithSalt _salt CreateAsset' {..} =
    _salt `Prelude.hashWithSalt` assetDescription
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assetName
      `Prelude.hashWithSalt` assetModelId

instance Prelude.NFData CreateAsset where
  rnf CreateAsset' {..} =
    Prelude.rnf assetDescription
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf assetModelId

instance Data.ToHeaders CreateAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAsset where
  toJSON CreateAsset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetDescription" Data..=)
              Prelude.<$> assetDescription,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("assetName" Data..= assetName),
            Prelude.Just ("assetModelId" Data..= assetModelId)
          ]
      )

instance Data.ToPath CreateAsset where
  toPath = Prelude.const "/assets"

instance Data.ToQuery CreateAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssetResponse' smart constructor.
data CreateAssetResponse = CreateAssetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asset. This ID uniquely identifies the asset within IoT
    -- SiteWise and can be used with other IoT SiteWise APIs.
    assetId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
    assetArn :: Prelude.Text,
    -- | The status of the asset, which contains a state (@CREATING@ after
    -- successfully calling this operation) and any error message.
    assetStatus :: AssetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAssetResponse_httpStatus' - The response's http status code.
--
-- 'assetId', 'createAssetResponse_assetId' - The ID of the asset. This ID uniquely identifies the asset within IoT
-- SiteWise and can be used with other IoT SiteWise APIs.
--
-- 'assetArn', 'createAssetResponse_assetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
--
-- 'assetStatus', 'createAssetResponse_assetStatus' - The status of the asset, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
newCreateAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'assetArn'
  Prelude.Text ->
  -- | 'assetStatus'
  AssetStatus ->
  CreateAssetResponse
newCreateAssetResponse
  pHttpStatus_
  pAssetId_
  pAssetArn_
  pAssetStatus_ =
    CreateAssetResponse'
      { httpStatus = pHttpStatus_,
        assetId = pAssetId_,
        assetArn = pAssetArn_,
        assetStatus = pAssetStatus_
      }

-- | The response's http status code.
createAssetResponse_httpStatus :: Lens.Lens' CreateAssetResponse Prelude.Int
createAssetResponse_httpStatus = Lens.lens (\CreateAssetResponse' {httpStatus} -> httpStatus) (\s@CreateAssetResponse' {} a -> s {httpStatus = a} :: CreateAssetResponse)

-- | The ID of the asset. This ID uniquely identifies the asset within IoT
-- SiteWise and can be used with other IoT SiteWise APIs.
createAssetResponse_assetId :: Lens.Lens' CreateAssetResponse Prelude.Text
createAssetResponse_assetId = Lens.lens (\CreateAssetResponse' {assetId} -> assetId) (\s@CreateAssetResponse' {} a -> s {assetId = a} :: CreateAssetResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
createAssetResponse_assetArn :: Lens.Lens' CreateAssetResponse Prelude.Text
createAssetResponse_assetArn = Lens.lens (\CreateAssetResponse' {assetArn} -> assetArn) (\s@CreateAssetResponse' {} a -> s {assetArn = a} :: CreateAssetResponse)

-- | The status of the asset, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
createAssetResponse_assetStatus :: Lens.Lens' CreateAssetResponse AssetStatus
createAssetResponse_assetStatus = Lens.lens (\CreateAssetResponse' {assetStatus} -> assetStatus) (\s@CreateAssetResponse' {} a -> s {assetStatus = a} :: CreateAssetResponse)

instance Prelude.NFData CreateAssetResponse where
  rnf CreateAssetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf assetArn
      `Prelude.seq` Prelude.rnf assetStatus

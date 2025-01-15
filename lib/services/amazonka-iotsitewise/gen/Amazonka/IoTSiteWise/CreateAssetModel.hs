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
-- Module      : Amazonka.IoTSiteWise.CreateAssetModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an asset model from specified property and hierarchy
-- definitions. You create assets from asset models. With asset models, you
-- can easily create assets of the same type that have standardized
-- definitions. Each asset created from a model inherits the asset model\'s
-- property and hierarchy definitions. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/define-models.html Defining asset models>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.CreateAssetModel
  ( -- * Creating a Request
    CreateAssetModel (..),
    newCreateAssetModel,

    -- * Request Lenses
    createAssetModel_assetModelCompositeModels,
    createAssetModel_assetModelDescription,
    createAssetModel_assetModelHierarchies,
    createAssetModel_assetModelProperties,
    createAssetModel_clientToken,
    createAssetModel_tags,
    createAssetModel_assetModelName,

    -- * Destructuring the Response
    CreateAssetModelResponse (..),
    newCreateAssetModelResponse,

    -- * Response Lenses
    createAssetModelResponse_httpStatus,
    createAssetModelResponse_assetModelId,
    createAssetModelResponse_assetModelArn,
    createAssetModelResponse_assetModelStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAssetModel' smart constructor.
data CreateAssetModel = CreateAssetModel'
  { -- | The composite asset models that are part of this asset model. Composite
    -- asset models are asset models that contain specific properties. Each
    -- composite model has a type that defines the properties that the
    -- composite model supports. Use composite asset models to define alarms on
    -- this asset model.
    assetModelCompositeModels :: Prelude.Maybe [AssetModelCompositeModelDefinition],
    -- | A description for the asset model.
    assetModelDescription :: Prelude.Maybe Prelude.Text,
    -- | The hierarchy definitions of the asset model. Each hierarchy specifies
    -- an asset model whose assets can be children of any other assets created
    -- from this asset model. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
    -- in the /IoT SiteWise User Guide/.
    --
    -- You can specify up to 10 hierarchies per asset model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    assetModelHierarchies :: Prelude.Maybe [AssetModelHierarchyDefinition],
    -- | The property definitions of the asset model. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
    -- in the /IoT SiteWise User Guide/.
    --
    -- You can specify up to 200 properties per asset model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    assetModelProperties :: Prelude.Maybe [AssetModelPropertyDefinition],
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that contain metadata for the asset model. For
    -- more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
    -- in the /IoT SiteWise User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, friendly name for the asset model.
    assetModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssetModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetModelCompositeModels', 'createAssetModel_assetModelCompositeModels' - The composite asset models that are part of this asset model. Composite
-- asset models are asset models that contain specific properties. Each
-- composite model has a type that defines the properties that the
-- composite model supports. Use composite asset models to define alarms on
-- this asset model.
--
-- 'assetModelDescription', 'createAssetModel_assetModelDescription' - A description for the asset model.
--
-- 'assetModelHierarchies', 'createAssetModel_assetModelHierarchies' - The hierarchy definitions of the asset model. Each hierarchy specifies
-- an asset model whose assets can be children of any other assets created
-- from this asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 10 hierarchies per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetModelProperties', 'createAssetModel_assetModelProperties' - The property definitions of the asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 200 properties per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'clientToken', 'createAssetModel_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'tags', 'createAssetModel_tags' - A list of key-value pairs that contain metadata for the asset model. For
-- more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetModelName', 'createAssetModel_assetModelName' - A unique, friendly name for the asset model.
newCreateAssetModel ::
  -- | 'assetModelName'
  Prelude.Text ->
  CreateAssetModel
newCreateAssetModel pAssetModelName_ =
  CreateAssetModel'
    { assetModelCompositeModels =
        Prelude.Nothing,
      assetModelDescription = Prelude.Nothing,
      assetModelHierarchies = Prelude.Nothing,
      assetModelProperties = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      assetModelName = pAssetModelName_
    }

-- | The composite asset models that are part of this asset model. Composite
-- asset models are asset models that contain specific properties. Each
-- composite model has a type that defines the properties that the
-- composite model supports. Use composite asset models to define alarms on
-- this asset model.
createAssetModel_assetModelCompositeModels :: Lens.Lens' CreateAssetModel (Prelude.Maybe [AssetModelCompositeModelDefinition])
createAssetModel_assetModelCompositeModels = Lens.lens (\CreateAssetModel' {assetModelCompositeModels} -> assetModelCompositeModels) (\s@CreateAssetModel' {} a -> s {assetModelCompositeModels = a} :: CreateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | A description for the asset model.
createAssetModel_assetModelDescription :: Lens.Lens' CreateAssetModel (Prelude.Maybe Prelude.Text)
createAssetModel_assetModelDescription = Lens.lens (\CreateAssetModel' {assetModelDescription} -> assetModelDescription) (\s@CreateAssetModel' {} a -> s {assetModelDescription = a} :: CreateAssetModel)

-- | The hierarchy definitions of the asset model. Each hierarchy specifies
-- an asset model whose assets can be children of any other assets created
-- from this asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 10 hierarchies per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
createAssetModel_assetModelHierarchies :: Lens.Lens' CreateAssetModel (Prelude.Maybe [AssetModelHierarchyDefinition])
createAssetModel_assetModelHierarchies = Lens.lens (\CreateAssetModel' {assetModelHierarchies} -> assetModelHierarchies) (\s@CreateAssetModel' {} a -> s {assetModelHierarchies = a} :: CreateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | The property definitions of the asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 200 properties per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
createAssetModel_assetModelProperties :: Lens.Lens' CreateAssetModel (Prelude.Maybe [AssetModelPropertyDefinition])
createAssetModel_assetModelProperties = Lens.lens (\CreateAssetModel' {assetModelProperties} -> assetModelProperties) (\s@CreateAssetModel' {} a -> s {assetModelProperties = a} :: CreateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createAssetModel_clientToken :: Lens.Lens' CreateAssetModel (Prelude.Maybe Prelude.Text)
createAssetModel_clientToken = Lens.lens (\CreateAssetModel' {clientToken} -> clientToken) (\s@CreateAssetModel' {} a -> s {clientToken = a} :: CreateAssetModel)

-- | A list of key-value pairs that contain metadata for the asset model. For
-- more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
createAssetModel_tags :: Lens.Lens' CreateAssetModel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssetModel_tags = Lens.lens (\CreateAssetModel' {tags} -> tags) (\s@CreateAssetModel' {} a -> s {tags = a} :: CreateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | A unique, friendly name for the asset model.
createAssetModel_assetModelName :: Lens.Lens' CreateAssetModel Prelude.Text
createAssetModel_assetModelName = Lens.lens (\CreateAssetModel' {assetModelName} -> assetModelName) (\s@CreateAssetModel' {} a -> s {assetModelName = a} :: CreateAssetModel)

instance Core.AWSRequest CreateAssetModel where
  type
    AWSResponse CreateAssetModel =
      CreateAssetModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssetModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetModelId")
            Prelude.<*> (x Data..:> "assetModelArn")
            Prelude.<*> (x Data..:> "assetModelStatus")
      )

instance Prelude.Hashable CreateAssetModel where
  hashWithSalt _salt CreateAssetModel' {..} =
    _salt
      `Prelude.hashWithSalt` assetModelCompositeModels
      `Prelude.hashWithSalt` assetModelDescription
      `Prelude.hashWithSalt` assetModelHierarchies
      `Prelude.hashWithSalt` assetModelProperties
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` assetModelName

instance Prelude.NFData CreateAssetModel where
  rnf CreateAssetModel' {..} =
    Prelude.rnf assetModelCompositeModels `Prelude.seq`
      Prelude.rnf assetModelDescription `Prelude.seq`
        Prelude.rnf assetModelHierarchies `Prelude.seq`
          Prelude.rnf assetModelProperties `Prelude.seq`
            Prelude.rnf clientToken `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf assetModelName

instance Data.ToHeaders CreateAssetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssetModel where
  toJSON CreateAssetModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetModelCompositeModels" Data..=)
              Prelude.<$> assetModelCompositeModels,
            ("assetModelDescription" Data..=)
              Prelude.<$> assetModelDescription,
            ("assetModelHierarchies" Data..=)
              Prelude.<$> assetModelHierarchies,
            ("assetModelProperties" Data..=)
              Prelude.<$> assetModelProperties,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("assetModelName" Data..= assetModelName)
          ]
      )

instance Data.ToPath CreateAssetModel where
  toPath = Prelude.const "/asset-models"

instance Data.ToQuery CreateAssetModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssetModelResponse' smart constructor.
data CreateAssetModelResponse = CreateAssetModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asset model. You can use this ID when you call other IoT
    -- SiteWise APIs.
    assetModelId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset model, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
    assetModelArn :: Prelude.Text,
    -- | The status of the asset model, which contains a state (@CREATING@ after
    -- successfully calling this operation) and any error message.
    assetModelStatus :: AssetModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssetModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAssetModelResponse_httpStatus' - The response's http status code.
--
-- 'assetModelId', 'createAssetModelResponse_assetModelId' - The ID of the asset model. You can use this ID when you call other IoT
-- SiteWise APIs.
--
-- 'assetModelArn', 'createAssetModelResponse_assetModelArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
--
-- 'assetModelStatus', 'createAssetModelResponse_assetModelStatus' - The status of the asset model, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
newCreateAssetModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'assetModelArn'
  Prelude.Text ->
  -- | 'assetModelStatus'
  AssetModelStatus ->
  CreateAssetModelResponse
newCreateAssetModelResponse
  pHttpStatus_
  pAssetModelId_
  pAssetModelArn_
  pAssetModelStatus_ =
    CreateAssetModelResponse'
      { httpStatus =
          pHttpStatus_,
        assetModelId = pAssetModelId_,
        assetModelArn = pAssetModelArn_,
        assetModelStatus = pAssetModelStatus_
      }

-- | The response's http status code.
createAssetModelResponse_httpStatus :: Lens.Lens' CreateAssetModelResponse Prelude.Int
createAssetModelResponse_httpStatus = Lens.lens (\CreateAssetModelResponse' {httpStatus} -> httpStatus) (\s@CreateAssetModelResponse' {} a -> s {httpStatus = a} :: CreateAssetModelResponse)

-- | The ID of the asset model. You can use this ID when you call other IoT
-- SiteWise APIs.
createAssetModelResponse_assetModelId :: Lens.Lens' CreateAssetModelResponse Prelude.Text
createAssetModelResponse_assetModelId = Lens.lens (\CreateAssetModelResponse' {assetModelId} -> assetModelId) (\s@CreateAssetModelResponse' {} a -> s {assetModelId = a} :: CreateAssetModelResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
createAssetModelResponse_assetModelArn :: Lens.Lens' CreateAssetModelResponse Prelude.Text
createAssetModelResponse_assetModelArn = Lens.lens (\CreateAssetModelResponse' {assetModelArn} -> assetModelArn) (\s@CreateAssetModelResponse' {} a -> s {assetModelArn = a} :: CreateAssetModelResponse)

-- | The status of the asset model, which contains a state (@CREATING@ after
-- successfully calling this operation) and any error message.
createAssetModelResponse_assetModelStatus :: Lens.Lens' CreateAssetModelResponse AssetModelStatus
createAssetModelResponse_assetModelStatus = Lens.lens (\CreateAssetModelResponse' {assetModelStatus} -> assetModelStatus) (\s@CreateAssetModelResponse' {} a -> s {assetModelStatus = a} :: CreateAssetModelResponse)

instance Prelude.NFData CreateAssetModelResponse where
  rnf CreateAssetModelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf assetModelId `Prelude.seq`
        Prelude.rnf assetModelArn `Prelude.seq`
          Prelude.rnf assetModelStatus

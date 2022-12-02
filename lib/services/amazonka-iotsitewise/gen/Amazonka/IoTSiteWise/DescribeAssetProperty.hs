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
-- Module      : Amazonka.IoTSiteWise.DescribeAssetProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an asset property.
--
-- When you call this operation for an attribute property, this response
-- includes the default attribute value that you define in the asset model.
-- If you update the default value in the model, this operation\'s response
-- includes the new default value.
--
-- This operation doesn\'t return the value of the asset property. To get
-- the value of an asset property, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_GetAssetPropertyValue.html GetAssetPropertyValue>.
module Amazonka.IoTSiteWise.DescribeAssetProperty
  ( -- * Creating a Request
    DescribeAssetProperty (..),
    newDescribeAssetProperty,

    -- * Request Lenses
    describeAssetProperty_assetId,
    describeAssetProperty_propertyId,

    -- * Destructuring the Response
    DescribeAssetPropertyResponse (..),
    newDescribeAssetPropertyResponse,

    -- * Response Lenses
    describeAssetPropertyResponse_assetProperty,
    describeAssetPropertyResponse_compositeModel,
    describeAssetPropertyResponse_httpStatus,
    describeAssetPropertyResponse_assetId,
    describeAssetPropertyResponse_assetName,
    describeAssetPropertyResponse_assetModelId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAssetProperty' smart constructor.
data DescribeAssetProperty = DescribeAssetProperty'
  { -- | The ID of the asset.
    assetId :: Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'describeAssetProperty_assetId' - The ID of the asset.
--
-- 'propertyId', 'describeAssetProperty_propertyId' - The ID of the asset property.
newDescribeAssetProperty ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'propertyId'
  Prelude.Text ->
  DescribeAssetProperty
newDescribeAssetProperty pAssetId_ pPropertyId_ =
  DescribeAssetProperty'
    { assetId = pAssetId_,
      propertyId = pPropertyId_
    }

-- | The ID of the asset.
describeAssetProperty_assetId :: Lens.Lens' DescribeAssetProperty Prelude.Text
describeAssetProperty_assetId = Lens.lens (\DescribeAssetProperty' {assetId} -> assetId) (\s@DescribeAssetProperty' {} a -> s {assetId = a} :: DescribeAssetProperty)

-- | The ID of the asset property.
describeAssetProperty_propertyId :: Lens.Lens' DescribeAssetProperty Prelude.Text
describeAssetProperty_propertyId = Lens.lens (\DescribeAssetProperty' {propertyId} -> propertyId) (\s@DescribeAssetProperty' {} a -> s {propertyId = a} :: DescribeAssetProperty)

instance Core.AWSRequest DescribeAssetProperty where
  type
    AWSResponse DescribeAssetProperty =
      DescribeAssetPropertyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssetPropertyResponse'
            Prelude.<$> (x Data..?> "assetProperty")
            Prelude.<*> (x Data..?> "compositeModel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetId")
            Prelude.<*> (x Data..:> "assetName")
            Prelude.<*> (x Data..:> "assetModelId")
      )

instance Prelude.Hashable DescribeAssetProperty where
  hashWithSalt _salt DescribeAssetProperty' {..} =
    _salt `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` propertyId

instance Prelude.NFData DescribeAssetProperty where
  rnf DescribeAssetProperty' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyId

instance Data.ToHeaders DescribeAssetProperty where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAssetProperty where
  toPath DescribeAssetProperty' {..} =
    Prelude.mconcat
      [ "/assets/",
        Data.toBS assetId,
        "/properties/",
        Data.toBS propertyId
      ]

instance Data.ToQuery DescribeAssetProperty where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssetPropertyResponse' smart constructor.
data DescribeAssetPropertyResponse = DescribeAssetPropertyResponse'
  { -- | The asset property\'s definition, alias, and notification state.
    --
    -- This response includes this object for normal asset properties. If you
    -- describe an asset property in a composite model, this response includes
    -- the asset property information in @compositeModel@.
    assetProperty :: Prelude.Maybe Property,
    -- | The composite asset model that declares this asset property, if this
    -- asset property exists in a composite model.
    compositeModel :: Prelude.Maybe CompositeModelProperty,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the asset.
    assetId :: Prelude.Text,
    -- | The name of the asset.
    assetName :: Prelude.Text,
    -- | The ID of the asset model.
    assetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssetPropertyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetProperty', 'describeAssetPropertyResponse_assetProperty' - The asset property\'s definition, alias, and notification state.
--
-- This response includes this object for normal asset properties. If you
-- describe an asset property in a composite model, this response includes
-- the asset property information in @compositeModel@.
--
-- 'compositeModel', 'describeAssetPropertyResponse_compositeModel' - The composite asset model that declares this asset property, if this
-- asset property exists in a composite model.
--
-- 'httpStatus', 'describeAssetPropertyResponse_httpStatus' - The response's http status code.
--
-- 'assetId', 'describeAssetPropertyResponse_assetId' - The ID of the asset.
--
-- 'assetName', 'describeAssetPropertyResponse_assetName' - The name of the asset.
--
-- 'assetModelId', 'describeAssetPropertyResponse_assetModelId' - The ID of the asset model.
newDescribeAssetPropertyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'assetName'
  Prelude.Text ->
  -- | 'assetModelId'
  Prelude.Text ->
  DescribeAssetPropertyResponse
newDescribeAssetPropertyResponse
  pHttpStatus_
  pAssetId_
  pAssetName_
  pAssetModelId_ =
    DescribeAssetPropertyResponse'
      { assetProperty =
          Prelude.Nothing,
        compositeModel = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        assetId = pAssetId_,
        assetName = pAssetName_,
        assetModelId = pAssetModelId_
      }

-- | The asset property\'s definition, alias, and notification state.
--
-- This response includes this object for normal asset properties. If you
-- describe an asset property in a composite model, this response includes
-- the asset property information in @compositeModel@.
describeAssetPropertyResponse_assetProperty :: Lens.Lens' DescribeAssetPropertyResponse (Prelude.Maybe Property)
describeAssetPropertyResponse_assetProperty = Lens.lens (\DescribeAssetPropertyResponse' {assetProperty} -> assetProperty) (\s@DescribeAssetPropertyResponse' {} a -> s {assetProperty = a} :: DescribeAssetPropertyResponse)

-- | The composite asset model that declares this asset property, if this
-- asset property exists in a composite model.
describeAssetPropertyResponse_compositeModel :: Lens.Lens' DescribeAssetPropertyResponse (Prelude.Maybe CompositeModelProperty)
describeAssetPropertyResponse_compositeModel = Lens.lens (\DescribeAssetPropertyResponse' {compositeModel} -> compositeModel) (\s@DescribeAssetPropertyResponse' {} a -> s {compositeModel = a} :: DescribeAssetPropertyResponse)

-- | The response's http status code.
describeAssetPropertyResponse_httpStatus :: Lens.Lens' DescribeAssetPropertyResponse Prelude.Int
describeAssetPropertyResponse_httpStatus = Lens.lens (\DescribeAssetPropertyResponse' {httpStatus} -> httpStatus) (\s@DescribeAssetPropertyResponse' {} a -> s {httpStatus = a} :: DescribeAssetPropertyResponse)

-- | The ID of the asset.
describeAssetPropertyResponse_assetId :: Lens.Lens' DescribeAssetPropertyResponse Prelude.Text
describeAssetPropertyResponse_assetId = Lens.lens (\DescribeAssetPropertyResponse' {assetId} -> assetId) (\s@DescribeAssetPropertyResponse' {} a -> s {assetId = a} :: DescribeAssetPropertyResponse)

-- | The name of the asset.
describeAssetPropertyResponse_assetName :: Lens.Lens' DescribeAssetPropertyResponse Prelude.Text
describeAssetPropertyResponse_assetName = Lens.lens (\DescribeAssetPropertyResponse' {assetName} -> assetName) (\s@DescribeAssetPropertyResponse' {} a -> s {assetName = a} :: DescribeAssetPropertyResponse)

-- | The ID of the asset model.
describeAssetPropertyResponse_assetModelId :: Lens.Lens' DescribeAssetPropertyResponse Prelude.Text
describeAssetPropertyResponse_assetModelId = Lens.lens (\DescribeAssetPropertyResponse' {assetModelId} -> assetModelId) (\s@DescribeAssetPropertyResponse' {} a -> s {assetModelId = a} :: DescribeAssetPropertyResponse)

instance Prelude.NFData DescribeAssetPropertyResponse where
  rnf DescribeAssetPropertyResponse' {..} =
    Prelude.rnf assetProperty
      `Prelude.seq` Prelude.rnf compositeModel
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf assetModelId

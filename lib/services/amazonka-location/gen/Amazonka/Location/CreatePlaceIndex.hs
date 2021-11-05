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
-- Module      : Amazonka.Location.CreatePlaceIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a place index resource in your AWS account, which supports
-- functions with geospatial data sourced from your chosen data provider.
module Amazonka.Location.CreatePlaceIndex
  ( -- * Creating a Request
    CreatePlaceIndex (..),
    newCreatePlaceIndex,

    -- * Request Lenses
    createPlaceIndex_dataSourceConfiguration,
    createPlaceIndex_description,
    createPlaceIndex_tags,
    createPlaceIndex_dataSource,
    createPlaceIndex_indexName,
    createPlaceIndex_pricingPlan,

    -- * Destructuring the Response
    CreatePlaceIndexResponse (..),
    newCreatePlaceIndexResponse,

    -- * Response Lenses
    createPlaceIndexResponse_httpStatus,
    createPlaceIndexResponse_createTime,
    createPlaceIndexResponse_indexArn,
    createPlaceIndexResponse_indexName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePlaceIndex' smart constructor.
data CreatePlaceIndex = CreatePlaceIndex'
  { -- | Specifies the data storage option requesting Places.
    dataSourceConfiguration :: Prelude.Maybe DataSourceConfiguration,
    -- | The optional description for the place index resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | Applies one or more tags to the place index resource. A tag is a
    -- key-value pair helps manage, identify, search, and filter your resources
    -- by labelling them.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource
    --
    -- -   Each resource tag must be unique with a maximum of one value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the data provider of geospatial data.
    --
    -- This field is case-sensitive. Enter the valid values as shown. For
    -- example, entering @HERE@ returns an error.
    --
    -- Valid values include:
    --
    -- -   @Esri@ – For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
    --     coverage in your region of interest, see
    --     <https://developers.arcgis.com/rest/geocode/api-reference/geocode-coverage.htm Esri details on geocoding coverage>.
    --
    -- -   @Here@ – For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
    --     coverage in your region of interest, see
    --     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
    --
    --     Place index resources using HERE Technologies as a data provider
    --     can\'t
    --     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
    --     for locations in Japan. For more information, see the
    --     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
    --     Location Service.
    --
    -- For additional information , see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
    -- on the /Amazon Location Service Developer Guide/.
    dataSource :: Prelude.Text,
    -- | The name of the place index resource.
    --
    -- Requirements:
    --
    -- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
    --     periods (.), and underscores (_).
    --
    -- -   Must be a unique place index resource name.
    --
    -- -   No spaces allowed. For example, @ExamplePlaceIndex@.
    indexName :: Prelude.Text,
    -- | Specifies the pricing plan for your place index resource.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: PricingPlan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlaceIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceConfiguration', 'createPlaceIndex_dataSourceConfiguration' - Specifies the data storage option requesting Places.
--
-- 'description', 'createPlaceIndex_description' - The optional description for the place index resource.
--
-- 'tags', 'createPlaceIndex_tags' - Applies one or more tags to the place index resource. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- 'dataSource', 'createPlaceIndex_dataSource' - Specifies the data provider of geospatial data.
--
-- This field is case-sensitive. Enter the valid values as shown. For
-- example, entering @HERE@ returns an error.
--
-- Valid values include:
--
-- -   @Esri@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
--     coverage in your region of interest, see
--     <https://developers.arcgis.com/rest/geocode/api-reference/geocode-coverage.htm Esri details on geocoding coverage>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
--
--     Place index resources using HERE Technologies as a data provider
--     can\'t
--     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
--     for locations in Japan. For more information, see the
--     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
--     Location Service.
--
-- For additional information , see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
-- on the /Amazon Location Service Developer Guide/.
--
-- 'indexName', 'createPlaceIndex_indexName' - The name of the place index resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique place index resource name.
--
-- -   No spaces allowed. For example, @ExamplePlaceIndex@.
--
-- 'pricingPlan', 'createPlaceIndex_pricingPlan' - Specifies the pricing plan for your place index resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
newCreatePlaceIndex ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 'pricingPlan'
  PricingPlan ->
  CreatePlaceIndex
newCreatePlaceIndex
  pDataSource_
  pIndexName_
  pPricingPlan_ =
    CreatePlaceIndex'
      { dataSourceConfiguration =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        dataSource = pDataSource_,
        indexName = pIndexName_,
        pricingPlan = pPricingPlan_
      }

-- | Specifies the data storage option requesting Places.
createPlaceIndex_dataSourceConfiguration :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe DataSourceConfiguration)
createPlaceIndex_dataSourceConfiguration = Lens.lens (\CreatePlaceIndex' {dataSourceConfiguration} -> dataSourceConfiguration) (\s@CreatePlaceIndex' {} a -> s {dataSourceConfiguration = a} :: CreatePlaceIndex)

-- | The optional description for the place index resource.
createPlaceIndex_description :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe Prelude.Text)
createPlaceIndex_description = Lens.lens (\CreatePlaceIndex' {description} -> description) (\s@CreatePlaceIndex' {} a -> s {description = a} :: CreatePlaceIndex)

-- | Applies one or more tags to the place index resource. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
createPlaceIndex_tags :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPlaceIndex_tags = Lens.lens (\CreatePlaceIndex' {tags} -> tags) (\s@CreatePlaceIndex' {} a -> s {tags = a} :: CreatePlaceIndex) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the data provider of geospatial data.
--
-- This field is case-sensitive. Enter the valid values as shown. For
-- example, entering @HERE@ returns an error.
--
-- Valid values include:
--
-- -   @Esri@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
--     coverage in your region of interest, see
--     <https://developers.arcgis.com/rest/geocode/api-reference/geocode-coverage.htm Esri details on geocoding coverage>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
--
--     Place index resources using HERE Technologies as a data provider
--     can\'t
--     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
--     for locations in Japan. For more information, see the
--     <https://aws.amazon.com/service-terms/ AWS Service Terms> for Amazon
--     Location Service.
--
-- For additional information , see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
-- on the /Amazon Location Service Developer Guide/.
createPlaceIndex_dataSource :: Lens.Lens' CreatePlaceIndex Prelude.Text
createPlaceIndex_dataSource = Lens.lens (\CreatePlaceIndex' {dataSource} -> dataSource) (\s@CreatePlaceIndex' {} a -> s {dataSource = a} :: CreatePlaceIndex)

-- | The name of the place index resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique place index resource name.
--
-- -   No spaces allowed. For example, @ExamplePlaceIndex@.
createPlaceIndex_indexName :: Lens.Lens' CreatePlaceIndex Prelude.Text
createPlaceIndex_indexName = Lens.lens (\CreatePlaceIndex' {indexName} -> indexName) (\s@CreatePlaceIndex' {} a -> s {indexName = a} :: CreatePlaceIndex)

-- | Specifies the pricing plan for your place index resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
createPlaceIndex_pricingPlan :: Lens.Lens' CreatePlaceIndex PricingPlan
createPlaceIndex_pricingPlan = Lens.lens (\CreatePlaceIndex' {pricingPlan} -> pricingPlan) (\s@CreatePlaceIndex' {} a -> s {pricingPlan = a} :: CreatePlaceIndex)

instance Core.AWSRequest CreatePlaceIndex where
  type
    AWSResponse CreatePlaceIndex =
      CreatePlaceIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlaceIndexResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CreateTime")
            Prelude.<*> (x Core..:> "IndexArn")
            Prelude.<*> (x Core..:> "IndexName")
      )

instance Prelude.Hashable CreatePlaceIndex

instance Prelude.NFData CreatePlaceIndex

instance Core.ToHeaders CreatePlaceIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePlaceIndex where
  toJSON CreatePlaceIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataSourceConfiguration" Core..=)
              Prelude.<$> dataSourceConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("DataSource" Core..= dataSource),
            Prelude.Just ("IndexName" Core..= indexName),
            Prelude.Just ("PricingPlan" Core..= pricingPlan)
          ]
      )

instance Core.ToPath CreatePlaceIndex where
  toPath = Prelude.const "/places/v0/indexes"

instance Core.ToQuery CreatePlaceIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePlaceIndexResponse' smart constructor.
data CreatePlaceIndexResponse = CreatePlaceIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the place index resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) for the place index resource. Used to
    -- specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
    indexArn :: Prelude.Text,
    -- | The name for the place index resource.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlaceIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPlaceIndexResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'createPlaceIndexResponse_createTime' - The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'indexArn', 'createPlaceIndexResponse_indexArn' - The Amazon Resource Name (ARN) for the place index resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
--
-- 'indexName', 'createPlaceIndexResponse_indexName' - The name for the place index resource.
newCreatePlaceIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'indexArn'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  CreatePlaceIndexResponse
newCreatePlaceIndexResponse
  pHttpStatus_
  pCreateTime_
  pIndexArn_
  pIndexName_ =
    CreatePlaceIndexResponse'
      { httpStatus =
          pHttpStatus_,
        createTime = Core._Time Lens.# pCreateTime_,
        indexArn = pIndexArn_,
        indexName = pIndexName_
      }

-- | The response's http status code.
createPlaceIndexResponse_httpStatus :: Lens.Lens' CreatePlaceIndexResponse Prelude.Int
createPlaceIndexResponse_httpStatus = Lens.lens (\CreatePlaceIndexResponse' {httpStatus} -> httpStatus) (\s@CreatePlaceIndexResponse' {} a -> s {httpStatus = a} :: CreatePlaceIndexResponse)

-- | The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
createPlaceIndexResponse_createTime :: Lens.Lens' CreatePlaceIndexResponse Prelude.UTCTime
createPlaceIndexResponse_createTime = Lens.lens (\CreatePlaceIndexResponse' {createTime} -> createTime) (\s@CreatePlaceIndexResponse' {} a -> s {createTime = a} :: CreatePlaceIndexResponse) Prelude.. Core._Time

-- | The Amazon Resource Name (ARN) for the place index resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
createPlaceIndexResponse_indexArn :: Lens.Lens' CreatePlaceIndexResponse Prelude.Text
createPlaceIndexResponse_indexArn = Lens.lens (\CreatePlaceIndexResponse' {indexArn} -> indexArn) (\s@CreatePlaceIndexResponse' {} a -> s {indexArn = a} :: CreatePlaceIndexResponse)

-- | The name for the place index resource.
createPlaceIndexResponse_indexName :: Lens.Lens' CreatePlaceIndexResponse Prelude.Text
createPlaceIndexResponse_indexName = Lens.lens (\CreatePlaceIndexResponse' {indexName} -> indexName) (\s@CreatePlaceIndexResponse' {} a -> s {indexName = a} :: CreatePlaceIndexResponse)

instance Prelude.NFData CreatePlaceIndexResponse

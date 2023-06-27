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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a place index resource in your Amazon Web Services account. Use
-- a place index resource to geocode addresses and other text queries by
-- using the @SearchPlaceIndexForText@ operation, and reverse geocode
-- coordinates by using the @SearchPlaceIndexForPosition@ operation, and
-- enable autosuggestions by using the @SearchPlaceIndexForSuggestions@
-- operation.
--
-- If your application is tracking or routing assets you use in your
-- business, such as delivery vehicles or employees, you must not use Esri
-- as your geolocation provider. See section 82 of the
-- <http://aws.amazon.com/service-terms Amazon Web Services service terms>
-- for more details.
module Amazonka.Location.CreatePlaceIndex
  ( -- * Creating a Request
    CreatePlaceIndex (..),
    newCreatePlaceIndex,

    -- * Request Lenses
    createPlaceIndex_dataSourceConfiguration,
    createPlaceIndex_description,
    createPlaceIndex_pricingPlan,
    createPlaceIndex_tags,
    createPlaceIndex_dataSource,
    createPlaceIndex_indexName,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Applies one or more tags to the place index resource. A tag is a
    -- key-value pair that helps you manage, identify, search, and filter your
    -- resources.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource.
    --
    -- -   Each tag key must be unique and must have exactly one associated
    --     value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@
    --
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the geospatial data provider for the new place index.
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
    -- -   @Grab@ – Grab provides place index functionality for Southeast Asia.
    --     For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>\'
    --     coverage, see
    --     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and areas covered>.
    --
    -- -   @Here@ – For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
    --     coverage in your region of interest, see
    --     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
    --
    --     If you specify HERE Technologies (@Here@) as the data provider, you
    --     may not
    --     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
    --     for locations in Japan. For more information, see the
    --     <http://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
    --     for Amazon Location Service.
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
    indexName :: Prelude.Text
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
-- 'pricingPlan', 'createPlaceIndex_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'tags', 'createPlaceIndex_tags' - Applies one or more tags to the place index resource. A tag is a
-- key-value pair that helps you manage, identify, search, and filter your
-- resources.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource.
--
-- -   Each tag key must be unique and must have exactly one associated
--     value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@
--
-- -   Cannot use \"aws:\" as a prefix for a key.
--
-- 'dataSource', 'createPlaceIndex_dataSource' - Specifies the geospatial data provider for the new place index.
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
-- -   @Grab@ – Grab provides place index functionality for Southeast Asia.
--     For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>\'
--     coverage, see
--     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and areas covered>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
--
--     If you specify HERE Technologies (@Here@) as the data provider, you
--     may not
--     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
--     for locations in Japan. For more information, see the
--     <http://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
--     for Amazon Location Service.
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
newCreatePlaceIndex ::
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  CreatePlaceIndex
newCreatePlaceIndex pDataSource_ pIndexName_ =
  CreatePlaceIndex'
    { dataSourceConfiguration =
        Prelude.Nothing,
      description = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      tags = Prelude.Nothing,
      dataSource = pDataSource_,
      indexName = pIndexName_
    }

-- | Specifies the data storage option requesting Places.
createPlaceIndex_dataSourceConfiguration :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe DataSourceConfiguration)
createPlaceIndex_dataSourceConfiguration = Lens.lens (\CreatePlaceIndex' {dataSourceConfiguration} -> dataSourceConfiguration) (\s@CreatePlaceIndex' {} a -> s {dataSourceConfiguration = a} :: CreatePlaceIndex)

-- | The optional description for the place index resource.
createPlaceIndex_description :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe Prelude.Text)
createPlaceIndex_description = Lens.lens (\CreatePlaceIndex' {description} -> description) (\s@CreatePlaceIndex' {} a -> s {description = a} :: CreatePlaceIndex)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
createPlaceIndex_pricingPlan :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe PricingPlan)
createPlaceIndex_pricingPlan = Lens.lens (\CreatePlaceIndex' {pricingPlan} -> pricingPlan) (\s@CreatePlaceIndex' {} a -> s {pricingPlan = a} :: CreatePlaceIndex)

-- | Applies one or more tags to the place index resource. A tag is a
-- key-value pair that helps you manage, identify, search, and filter your
-- resources.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource.
--
-- -   Each tag key must be unique and must have exactly one associated
--     value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@
--
-- -   Cannot use \"aws:\" as a prefix for a key.
createPlaceIndex_tags :: Lens.Lens' CreatePlaceIndex (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPlaceIndex_tags = Lens.lens (\CreatePlaceIndex' {tags} -> tags) (\s@CreatePlaceIndex' {} a -> s {tags = a} :: CreatePlaceIndex) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the geospatial data provider for the new place index.
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
-- -   @Grab@ – Grab provides place index functionality for Southeast Asia.
--     For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html GrabMaps>\'
--     coverage, see
--     <https://docs.aws.amazon.com/location/latest/developerguide/grab.html#grab-coverage-area GrabMaps countries and areas covered>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/geocoder/dev_guide/topics/coverage-geocoder.html HERE details on goecoding coverage>.
--
--     If you specify HERE Technologies (@Here@) as the data provider, you
--     may not
--     <https://docs.aws.amazon.com/location-places/latest/APIReference/API_DataSourceConfiguration.html store results>
--     for locations in Japan. For more information, see the
--     <http://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
--     for Amazon Location Service.
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

instance Core.AWSRequest CreatePlaceIndex where
  type
    AWSResponse CreatePlaceIndex =
      CreatePlaceIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlaceIndexResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "IndexArn")
            Prelude.<*> (x Data..:> "IndexName")
      )

instance Prelude.Hashable CreatePlaceIndex where
  hashWithSalt _salt CreatePlaceIndex' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` indexName

instance Prelude.NFData CreatePlaceIndex where
  rnf CreatePlaceIndex' {..} =
    Prelude.rnf dataSourceConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf indexName

instance Data.ToHeaders CreatePlaceIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePlaceIndex where
  toJSON CreatePlaceIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceConfiguration" Data..=)
              Prelude.<$> dataSourceConfiguration,
            ("Description" Data..=) Prelude.<$> description,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("DataSource" Data..= dataSource),
            Prelude.Just ("IndexName" Data..= indexName)
          ]
      )

instance Data.ToPath CreatePlaceIndex where
  toPath = Prelude.const "/places/v0/indexes"

instance Data.ToQuery CreatePlaceIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePlaceIndexResponse' smart constructor.
data CreatePlaceIndexResponse = CreatePlaceIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the place index resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) for the place index resource. Used to
    -- specify a resource across Amazon Web Services.
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
-- specify a resource across Amazon Web Services.
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
        createTime = Data._Time Lens.# pCreateTime_,
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
createPlaceIndexResponse_createTime = Lens.lens (\CreatePlaceIndexResponse' {createTime} -> createTime) (\s@CreatePlaceIndexResponse' {} a -> s {createTime = a} :: CreatePlaceIndexResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) for the place index resource. Used to
-- specify a resource across Amazon Web Services.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
createPlaceIndexResponse_indexArn :: Lens.Lens' CreatePlaceIndexResponse Prelude.Text
createPlaceIndexResponse_indexArn = Lens.lens (\CreatePlaceIndexResponse' {indexArn} -> indexArn) (\s@CreatePlaceIndexResponse' {} a -> s {indexArn = a} :: CreatePlaceIndexResponse)

-- | The name for the place index resource.
createPlaceIndexResponse_indexName :: Lens.Lens' CreatePlaceIndexResponse Prelude.Text
createPlaceIndexResponse_indexName = Lens.lens (\CreatePlaceIndexResponse' {indexName} -> indexName) (\s@CreatePlaceIndexResponse' {} a -> s {indexName = a} :: CreatePlaceIndexResponse)

instance Prelude.NFData CreatePlaceIndexResponse where
  rnf CreatePlaceIndexResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName

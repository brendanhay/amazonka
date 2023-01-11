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
-- Module      : Amazonka.Location.CreateRouteCalculator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route calculator resource in your AWS account.
--
-- You can send requests to a route calculator resource to estimate travel
-- time, distance, and get directions. A route calculator sources traffic
-- and road network data from your chosen data provider.
--
-- If your application is tracking or routing assets you use in your
-- business, such as delivery vehicles or employees, you may only use HERE
-- as your geolocation provider. See section 82 of the
-- <http://aws.amazon.com/service-terms AWS service terms> for more
-- details.
module Amazonka.Location.CreateRouteCalculator
  ( -- * Creating a Request
    CreateRouteCalculator (..),
    newCreateRouteCalculator,

    -- * Request Lenses
    createRouteCalculator_description,
    createRouteCalculator_pricingPlan,
    createRouteCalculator_tags,
    createRouteCalculator_calculatorName,
    createRouteCalculator_dataSource,

    -- * Destructuring the Response
    CreateRouteCalculatorResponse (..),
    newCreateRouteCalculatorResponse,

    -- * Response Lenses
    createRouteCalculatorResponse_httpStatus,
    createRouteCalculatorResponse_calculatorArn,
    createRouteCalculatorResponse_calculatorName,
    createRouteCalculatorResponse_createTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRouteCalculator' smart constructor.
data CreateRouteCalculator = CreateRouteCalculator'
  { -- | The optional description for the route calculator resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Applies one or more tags to the route calculator resource. A tag is a
    -- key-value pair helps manage, identify, search, and filter your resources
    -- by labelling them.
    --
    -- -   For example: { @\"tag1\" : \"value1\"@, @\"tag2\" : \"value2\"@}
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
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the route calculator resource.
    --
    -- Requirements:
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9) , hyphens (-),
    --     periods (.), and underscores (_).
    --
    -- -   Must be a unique Route calculator resource name.
    --
    -- -   No spaces allowed. For example, @ExampleRouteCalculator@.
    calculatorName :: Prelude.Text,
    -- | Specifies the data provider of traffic and road network data.
    --
    -- This field is case-sensitive. Enter the valid values as shown. For
    -- example, entering @HERE@ returns an error. Route calculators that use
    -- Esri as a data source only calculate routes that are shorter than 400
    -- km.
    --
    -- Valid values include:
    --
    -- -   @Esri@ – For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
    --     coverage in your region of interest, see
    --     <https://doc.arcgis.com/en/arcgis-online/reference/network-coverage.htm Esri details on street networks and traffic coverage>.
    --
    -- -   @Here@ – For additional information about
    --     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
    --     coverage in your region of interest, see
    --     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/car-routing.html HERE car routing coverage>
    --     and
    --     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/truck-routing.html HERE truck routing coverage>.
    --
    -- For additional information , see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
    -- on the /Amazon Location Service Developer Guide/.
    dataSource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRouteCalculator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createRouteCalculator_description' - The optional description for the route calculator resource.
--
-- 'pricingPlan', 'createRouteCalculator_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'tags', 'createRouteCalculator_tags' - Applies one or more tags to the route calculator resource. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
--
-- -   For example: { @\"tag1\" : \"value1\"@, @\"tag2\" : \"value2\"@}
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
-- -   Cannot use \"aws:\" as a prefix for a key.
--
-- 'calculatorName', 'createRouteCalculator_calculatorName' - The name of the route calculator resource.
--
-- Requirements:
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9) , hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique Route calculator resource name.
--
-- -   No spaces allowed. For example, @ExampleRouteCalculator@.
--
-- 'dataSource', 'createRouteCalculator_dataSource' - Specifies the data provider of traffic and road network data.
--
-- This field is case-sensitive. Enter the valid values as shown. For
-- example, entering @HERE@ returns an error. Route calculators that use
-- Esri as a data source only calculate routes that are shorter than 400
-- km.
--
-- Valid values include:
--
-- -   @Esri@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
--     coverage in your region of interest, see
--     <https://doc.arcgis.com/en/arcgis-online/reference/network-coverage.htm Esri details on street networks and traffic coverage>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/car-routing.html HERE car routing coverage>
--     and
--     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/truck-routing.html HERE truck routing coverage>.
--
-- For additional information , see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
-- on the /Amazon Location Service Developer Guide/.
newCreateRouteCalculator ::
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'dataSource'
  Prelude.Text ->
  CreateRouteCalculator
newCreateRouteCalculator
  pCalculatorName_
  pDataSource_ =
    CreateRouteCalculator'
      { description =
          Prelude.Nothing,
        pricingPlan = Prelude.Nothing,
        tags = Prelude.Nothing,
        calculatorName = pCalculatorName_,
        dataSource = pDataSource_
      }

-- | The optional description for the route calculator resource.
createRouteCalculator_description :: Lens.Lens' CreateRouteCalculator (Prelude.Maybe Prelude.Text)
createRouteCalculator_description = Lens.lens (\CreateRouteCalculator' {description} -> description) (\s@CreateRouteCalculator' {} a -> s {description = a} :: CreateRouteCalculator)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
createRouteCalculator_pricingPlan :: Lens.Lens' CreateRouteCalculator (Prelude.Maybe PricingPlan)
createRouteCalculator_pricingPlan = Lens.lens (\CreateRouteCalculator' {pricingPlan} -> pricingPlan) (\s@CreateRouteCalculator' {} a -> s {pricingPlan = a} :: CreateRouteCalculator)

-- | Applies one or more tags to the route calculator resource. A tag is a
-- key-value pair helps manage, identify, search, and filter your resources
-- by labelling them.
--
-- -   For example: { @\"tag1\" : \"value1\"@, @\"tag2\" : \"value2\"@}
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
-- -   Cannot use \"aws:\" as a prefix for a key.
createRouteCalculator_tags :: Lens.Lens' CreateRouteCalculator (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRouteCalculator_tags = Lens.lens (\CreateRouteCalculator' {tags} -> tags) (\s@CreateRouteCalculator' {} a -> s {tags = a} :: CreateRouteCalculator) Prelude.. Lens.mapping Lens.coerced

-- | The name of the route calculator resource.
--
-- Requirements:
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9) , hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique Route calculator resource name.
--
-- -   No spaces allowed. For example, @ExampleRouteCalculator@.
createRouteCalculator_calculatorName :: Lens.Lens' CreateRouteCalculator Prelude.Text
createRouteCalculator_calculatorName = Lens.lens (\CreateRouteCalculator' {calculatorName} -> calculatorName) (\s@CreateRouteCalculator' {} a -> s {calculatorName = a} :: CreateRouteCalculator)

-- | Specifies the data provider of traffic and road network data.
--
-- This field is case-sensitive. Enter the valid values as shown. For
-- example, entering @HERE@ returns an error. Route calculators that use
-- Esri as a data source only calculate routes that are shorter than 400
-- km.
--
-- Valid values include:
--
-- -   @Esri@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/esri.html Esri>\'s
--     coverage in your region of interest, see
--     <https://doc.arcgis.com/en/arcgis-online/reference/network-coverage.htm Esri details on street networks and traffic coverage>.
--
-- -   @Here@ – For additional information about
--     <https://docs.aws.amazon.com/location/latest/developerguide/HERE.html HERE Technologies>\'
--     coverage in your region of interest, see
--     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/car-routing.html HERE car routing coverage>
--     and
--     <https://developer.here.com/documentation/routing-api/dev_guide/topics/coverage/truck-routing.html HERE truck routing coverage>.
--
-- For additional information , see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Data providers>
-- on the /Amazon Location Service Developer Guide/.
createRouteCalculator_dataSource :: Lens.Lens' CreateRouteCalculator Prelude.Text
createRouteCalculator_dataSource = Lens.lens (\CreateRouteCalculator' {dataSource} -> dataSource) (\s@CreateRouteCalculator' {} a -> s {dataSource = a} :: CreateRouteCalculator)

instance Core.AWSRequest CreateRouteCalculator where
  type
    AWSResponse CreateRouteCalculator =
      CreateRouteCalculatorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRouteCalculatorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CalculatorArn")
            Prelude.<*> (x Data..:> "CalculatorName")
            Prelude.<*> (x Data..:> "CreateTime")
      )

instance Prelude.Hashable CreateRouteCalculator where
  hashWithSalt _salt CreateRouteCalculator' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` calculatorName
      `Prelude.hashWithSalt` dataSource

instance Prelude.NFData CreateRouteCalculator where
  rnf CreateRouteCalculator' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf dataSource

instance Data.ToHeaders CreateRouteCalculator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRouteCalculator where
  toJSON CreateRouteCalculator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CalculatorName" Data..= calculatorName),
            Prelude.Just ("DataSource" Data..= dataSource)
          ]
      )

instance Data.ToPath CreateRouteCalculator where
  toPath = Prelude.const "/routes/v0/calculators"

instance Data.ToQuery CreateRouteCalculator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRouteCalculatorResponse' smart constructor.
data CreateRouteCalculatorResponse = CreateRouteCalculatorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the route calculator resource. Use
    -- the ARN when you specify a resource across all AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
    calculatorArn :: Prelude.Text,
    -- | The name of the route calculator resource.
    --
    -- -   For example, @ExampleRouteCalculator@.
    calculatorName :: Prelude.Text,
    -- | The timestamp when the route calculator resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    createTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRouteCalculatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRouteCalculatorResponse_httpStatus' - The response's http status code.
--
-- 'calculatorArn', 'createRouteCalculatorResponse_calculatorArn' - The Amazon Resource Name (ARN) for the route calculator resource. Use
-- the ARN when you specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
--
-- 'calculatorName', 'createRouteCalculatorResponse_calculatorName' - The name of the route calculator resource.
--
-- -   For example, @ExampleRouteCalculator@.
--
-- 'createTime', 'createRouteCalculatorResponse_createTime' - The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
newCreateRouteCalculatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'calculatorArn'
  Prelude.Text ->
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  CreateRouteCalculatorResponse
newCreateRouteCalculatorResponse
  pHttpStatus_
  pCalculatorArn_
  pCalculatorName_
  pCreateTime_ =
    CreateRouteCalculatorResponse'
      { httpStatus =
          pHttpStatus_,
        calculatorArn = pCalculatorArn_,
        calculatorName = pCalculatorName_,
        createTime = Data._Time Lens.# pCreateTime_
      }

-- | The response's http status code.
createRouteCalculatorResponse_httpStatus :: Lens.Lens' CreateRouteCalculatorResponse Prelude.Int
createRouteCalculatorResponse_httpStatus = Lens.lens (\CreateRouteCalculatorResponse' {httpStatus} -> httpStatus) (\s@CreateRouteCalculatorResponse' {} a -> s {httpStatus = a} :: CreateRouteCalculatorResponse)

-- | The Amazon Resource Name (ARN) for the route calculator resource. Use
-- the ARN when you specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
createRouteCalculatorResponse_calculatorArn :: Lens.Lens' CreateRouteCalculatorResponse Prelude.Text
createRouteCalculatorResponse_calculatorArn = Lens.lens (\CreateRouteCalculatorResponse' {calculatorArn} -> calculatorArn) (\s@CreateRouteCalculatorResponse' {} a -> s {calculatorArn = a} :: CreateRouteCalculatorResponse)

-- | The name of the route calculator resource.
--
-- -   For example, @ExampleRouteCalculator@.
createRouteCalculatorResponse_calculatorName :: Lens.Lens' CreateRouteCalculatorResponse Prelude.Text
createRouteCalculatorResponse_calculatorName = Lens.lens (\CreateRouteCalculatorResponse' {calculatorName} -> calculatorName) (\s@CreateRouteCalculatorResponse' {} a -> s {calculatorName = a} :: CreateRouteCalculatorResponse)

-- | The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
createRouteCalculatorResponse_createTime :: Lens.Lens' CreateRouteCalculatorResponse Prelude.UTCTime
createRouteCalculatorResponse_createTime = Lens.lens (\CreateRouteCalculatorResponse' {createTime} -> createTime) (\s@CreateRouteCalculatorResponse' {} a -> s {createTime = a} :: CreateRouteCalculatorResponse) Prelude.. Data._Time

instance Prelude.NFData CreateRouteCalculatorResponse where
  rnf CreateRouteCalculatorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf calculatorArn
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf createTime

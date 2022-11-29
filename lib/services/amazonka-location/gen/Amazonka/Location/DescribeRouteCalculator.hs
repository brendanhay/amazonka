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
-- Module      : Amazonka.Location.DescribeRouteCalculator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the route calculator resource details.
module Amazonka.Location.DescribeRouteCalculator
  ( -- * Creating a Request
    DescribeRouteCalculator (..),
    newDescribeRouteCalculator,

    -- * Request Lenses
    describeRouteCalculator_calculatorName,

    -- * Destructuring the Response
    DescribeRouteCalculatorResponse (..),
    newDescribeRouteCalculatorResponse,

    -- * Response Lenses
    describeRouteCalculatorResponse_tags,
    describeRouteCalculatorResponse_pricingPlan,
    describeRouteCalculatorResponse_httpStatus,
    describeRouteCalculatorResponse_calculatorArn,
    describeRouteCalculatorResponse_calculatorName,
    describeRouteCalculatorResponse_createTime,
    describeRouteCalculatorResponse_dataSource,
    describeRouteCalculatorResponse_description,
    describeRouteCalculatorResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRouteCalculator' smart constructor.
data DescribeRouteCalculator = DescribeRouteCalculator'
  { -- | The name of the route calculator resource.
    calculatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouteCalculator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatorName', 'describeRouteCalculator_calculatorName' - The name of the route calculator resource.
newDescribeRouteCalculator ::
  -- | 'calculatorName'
  Prelude.Text ->
  DescribeRouteCalculator
newDescribeRouteCalculator pCalculatorName_ =
  DescribeRouteCalculator'
    { calculatorName =
        pCalculatorName_
    }

-- | The name of the route calculator resource.
describeRouteCalculator_calculatorName :: Lens.Lens' DescribeRouteCalculator Prelude.Text
describeRouteCalculator_calculatorName = Lens.lens (\DescribeRouteCalculator' {calculatorName} -> calculatorName) (\s@DescribeRouteCalculator' {} a -> s {calculatorName = a} :: DescribeRouteCalculator)

instance Core.AWSRequest DescribeRouteCalculator where
  type
    AWSResponse DescribeRouteCalculator =
      DescribeRouteCalculatorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRouteCalculatorResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "PricingPlan")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CalculatorArn")
            Prelude.<*> (x Core..:> "CalculatorName")
            Prelude.<*> (x Core..:> "CreateTime")
            Prelude.<*> (x Core..:> "DataSource")
            Prelude.<*> (x Core..:> "Description")
            Prelude.<*> (x Core..:> "UpdateTime")
      )

instance Prelude.Hashable DescribeRouteCalculator where
  hashWithSalt _salt DescribeRouteCalculator' {..} =
    _salt `Prelude.hashWithSalt` calculatorName

instance Prelude.NFData DescribeRouteCalculator where
  rnf DescribeRouteCalculator' {..} =
    Prelude.rnf calculatorName

instance Core.ToHeaders DescribeRouteCalculator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeRouteCalculator where
  toPath DescribeRouteCalculator' {..} =
    Prelude.mconcat
      ["/routes/v0/calculators/", Core.toBS calculatorName]

instance Core.ToQuery DescribeRouteCalculator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRouteCalculatorResponse' smart constructor.
data DescribeRouteCalculatorResponse = DescribeRouteCalculatorResponse'
  { -- | Tags associated with route calculator resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the Route calculator resource. Use
    -- the ARN when you specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
    calculatorArn :: Prelude.Text,
    -- | The name of the route calculator resource being described.
    calculatorName :: Prelude.Text,
    -- | The timestamp when the route calculator resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    createTime :: Core.POSIX,
    -- | The data provider of traffic and road network data. Indicates one of the
    -- available providers:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The optional description of the route calculator resource.
    description :: Prelude.Text,
    -- | The timestamp when the route calculator resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    updateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouteCalculatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeRouteCalculatorResponse_tags' - Tags associated with route calculator resource.
--
-- 'pricingPlan', 'describeRouteCalculatorResponse_pricingPlan' - Always returns @RequestBasedUsage@.
--
-- 'httpStatus', 'describeRouteCalculatorResponse_httpStatus' - The response's http status code.
--
-- 'calculatorArn', 'describeRouteCalculatorResponse_calculatorArn' - The Amazon Resource Name (ARN) for the Route calculator resource. Use
-- the ARN when you specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
--
-- 'calculatorName', 'describeRouteCalculatorResponse_calculatorName' - The name of the route calculator resource being described.
--
-- 'createTime', 'describeRouteCalculatorResponse_createTime' - The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
--
-- 'dataSource', 'describeRouteCalculatorResponse_dataSource' - The data provider of traffic and road network data. Indicates one of the
-- available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'description', 'describeRouteCalculatorResponse_description' - The optional description of the route calculator resource.
--
-- 'updateTime', 'describeRouteCalculatorResponse_updateTime' - The timestamp when the route calculator resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
newDescribeRouteCalculatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'calculatorArn'
  Prelude.Text ->
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  DescribeRouteCalculatorResponse
newDescribeRouteCalculatorResponse
  pHttpStatus_
  pCalculatorArn_
  pCalculatorName_
  pCreateTime_
  pDataSource_
  pDescription_
  pUpdateTime_ =
    DescribeRouteCalculatorResponse'
      { tags =
          Prelude.Nothing,
        pricingPlan = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        calculatorArn = pCalculatorArn_,
        calculatorName = pCalculatorName_,
        createTime =
          Core._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        updateTime =
          Core._Time Lens.# pUpdateTime_
      }

-- | Tags associated with route calculator resource.
describeRouteCalculatorResponse_tags :: Lens.Lens' DescribeRouteCalculatorResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRouteCalculatorResponse_tags = Lens.lens (\DescribeRouteCalculatorResponse' {tags} -> tags) (\s@DescribeRouteCalculatorResponse' {} a -> s {tags = a} :: DescribeRouteCalculatorResponse) Prelude.. Lens.mapping Lens.coerced

-- | Always returns @RequestBasedUsage@.
describeRouteCalculatorResponse_pricingPlan :: Lens.Lens' DescribeRouteCalculatorResponse (Prelude.Maybe PricingPlan)
describeRouteCalculatorResponse_pricingPlan = Lens.lens (\DescribeRouteCalculatorResponse' {pricingPlan} -> pricingPlan) (\s@DescribeRouteCalculatorResponse' {} a -> s {pricingPlan = a} :: DescribeRouteCalculatorResponse)

-- | The response's http status code.
describeRouteCalculatorResponse_httpStatus :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.Int
describeRouteCalculatorResponse_httpStatus = Lens.lens (\DescribeRouteCalculatorResponse' {httpStatus} -> httpStatus) (\s@DescribeRouteCalculatorResponse' {} a -> s {httpStatus = a} :: DescribeRouteCalculatorResponse)

-- | The Amazon Resource Name (ARN) for the Route calculator resource. Use
-- the ARN when you specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route-calculator\/ExampleCalculator@
describeRouteCalculatorResponse_calculatorArn :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.Text
describeRouteCalculatorResponse_calculatorArn = Lens.lens (\DescribeRouteCalculatorResponse' {calculatorArn} -> calculatorArn) (\s@DescribeRouteCalculatorResponse' {} a -> s {calculatorArn = a} :: DescribeRouteCalculatorResponse)

-- | The name of the route calculator resource being described.
describeRouteCalculatorResponse_calculatorName :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.Text
describeRouteCalculatorResponse_calculatorName = Lens.lens (\DescribeRouteCalculatorResponse' {calculatorName} -> calculatorName) (\s@DescribeRouteCalculatorResponse' {} a -> s {calculatorName = a} :: DescribeRouteCalculatorResponse)

-- | The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
describeRouteCalculatorResponse_createTime :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.UTCTime
describeRouteCalculatorResponse_createTime = Lens.lens (\DescribeRouteCalculatorResponse' {createTime} -> createTime) (\s@DescribeRouteCalculatorResponse' {} a -> s {createTime = a} :: DescribeRouteCalculatorResponse) Prelude.. Core._Time

-- | The data provider of traffic and road network data. Indicates one of the
-- available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
describeRouteCalculatorResponse_dataSource :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.Text
describeRouteCalculatorResponse_dataSource = Lens.lens (\DescribeRouteCalculatorResponse' {dataSource} -> dataSource) (\s@DescribeRouteCalculatorResponse' {} a -> s {dataSource = a} :: DescribeRouteCalculatorResponse)

-- | The optional description of the route calculator resource.
describeRouteCalculatorResponse_description :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.Text
describeRouteCalculatorResponse_description = Lens.lens (\DescribeRouteCalculatorResponse' {description} -> description) (\s@DescribeRouteCalculatorResponse' {} a -> s {description = a} :: DescribeRouteCalculatorResponse)

-- | The timestamp when the route calculator resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
describeRouteCalculatorResponse_updateTime :: Lens.Lens' DescribeRouteCalculatorResponse Prelude.UTCTime
describeRouteCalculatorResponse_updateTime = Lens.lens (\DescribeRouteCalculatorResponse' {updateTime} -> updateTime) (\s@DescribeRouteCalculatorResponse' {} a -> s {updateTime = a} :: DescribeRouteCalculatorResponse) Prelude.. Core._Time

instance
  Prelude.NFData
    DescribeRouteCalculatorResponse
  where
  rnf DescribeRouteCalculatorResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf calculatorArn
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf updateTime

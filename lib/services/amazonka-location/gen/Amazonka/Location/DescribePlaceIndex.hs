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
-- Module      : Amazonka.Location.DescribePlaceIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the place index resource details.
module Amazonka.Location.DescribePlaceIndex
  ( -- * Creating a Request
    DescribePlaceIndex (..),
    newDescribePlaceIndex,

    -- * Request Lenses
    describePlaceIndex_indexName,

    -- * Destructuring the Response
    DescribePlaceIndexResponse (..),
    newDescribePlaceIndexResponse,

    -- * Response Lenses
    describePlaceIndexResponse_pricingPlan,
    describePlaceIndexResponse_tags,
    describePlaceIndexResponse_httpStatus,
    describePlaceIndexResponse_createTime,
    describePlaceIndexResponse_dataSource,
    describePlaceIndexResponse_dataSourceConfiguration,
    describePlaceIndexResponse_description,
    describePlaceIndexResponse_indexArn,
    describePlaceIndexResponse_indexName,
    describePlaceIndexResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePlaceIndex' smart constructor.
data DescribePlaceIndex = DescribePlaceIndex'
  { -- | The name of the place index resource.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlaceIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'describePlaceIndex_indexName' - The name of the place index resource.
newDescribePlaceIndex ::
  -- | 'indexName'
  Prelude.Text ->
  DescribePlaceIndex
newDescribePlaceIndex pIndexName_ =
  DescribePlaceIndex' {indexName = pIndexName_}

-- | The name of the place index resource.
describePlaceIndex_indexName :: Lens.Lens' DescribePlaceIndex Prelude.Text
describePlaceIndex_indexName = Lens.lens (\DescribePlaceIndex' {indexName} -> indexName) (\s@DescribePlaceIndex' {} a -> s {indexName = a} :: DescribePlaceIndex)

instance Core.AWSRequest DescribePlaceIndex where
  type
    AWSResponse DescribePlaceIndex =
      DescribePlaceIndexResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePlaceIndexResponse'
            Prelude.<$> (x Data..?> "PricingPlan")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "DataSource")
            Prelude.<*> (x Data..:> "DataSourceConfiguration")
            Prelude.<*> (x Data..:> "Description")
            Prelude.<*> (x Data..:> "IndexArn")
            Prelude.<*> (x Data..:> "IndexName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable DescribePlaceIndex where
  hashWithSalt _salt DescribePlaceIndex' {..} =
    _salt `Prelude.hashWithSalt` indexName

instance Prelude.NFData DescribePlaceIndex where
  rnf DescribePlaceIndex' {..} = Prelude.rnf indexName

instance Data.ToHeaders DescribePlaceIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePlaceIndex where
  toPath DescribePlaceIndex' {..} =
    Prelude.mconcat
      ["/places/v0/indexes/", Data.toBS indexName]

instance Data.ToQuery DescribePlaceIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePlaceIndexResponse' smart constructor.
data DescribePlaceIndexResponse = DescribePlaceIndexResponse'
  { -- | No longer used. Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Tags associated with place index resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the place index resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The data provider of geospatial data. Values can be one of the
    -- following:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The specified data storage option for requesting Places.
    dataSourceConfiguration :: DataSourceConfiguration,
    -- | The optional description for the place index resource.
    description :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the place index resource. Used to
    -- specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
    indexArn :: Prelude.Text,
    -- | The name of the place index resource being described.
    indexName :: Prelude.Text,
    -- | The timestamp for when the place index resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlaceIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'describePlaceIndexResponse_pricingPlan' - No longer used. Always returns @RequestBasedUsage@.
--
-- 'tags', 'describePlaceIndexResponse_tags' - Tags associated with place index resource.
--
-- 'httpStatus', 'describePlaceIndexResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'describePlaceIndexResponse_createTime' - The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'dataSource', 'describePlaceIndexResponse_dataSource' - The data provider of geospatial data. Values can be one of the
-- following:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'dataSourceConfiguration', 'describePlaceIndexResponse_dataSourceConfiguration' - The specified data storage option for requesting Places.
--
-- 'description', 'describePlaceIndexResponse_description' - The optional description for the place index resource.
--
-- 'indexArn', 'describePlaceIndexResponse_indexArn' - The Amazon Resource Name (ARN) for the place index resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
--
-- 'indexName', 'describePlaceIndexResponse_indexName' - The name of the place index resource being described.
--
-- 'updateTime', 'describePlaceIndexResponse_updateTime' - The timestamp for when the place index resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newDescribePlaceIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'dataSourceConfiguration'
  DataSourceConfiguration ->
  -- | 'description'
  Prelude.Text ->
  -- | 'indexArn'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  DescribePlaceIndexResponse
newDescribePlaceIndexResponse
  pHttpStatus_
  pCreateTime_
  pDataSource_
  pDataSourceConfiguration_
  pDescription_
  pIndexArn_
  pIndexName_
  pUpdateTime_ =
    DescribePlaceIndexResponse'
      { pricingPlan =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        dataSourceConfiguration =
          pDataSourceConfiguration_,
        description = pDescription_,
        indexArn = pIndexArn_,
        indexName = pIndexName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | No longer used. Always returns @RequestBasedUsage@.
describePlaceIndexResponse_pricingPlan :: Lens.Lens' DescribePlaceIndexResponse (Prelude.Maybe PricingPlan)
describePlaceIndexResponse_pricingPlan = Lens.lens (\DescribePlaceIndexResponse' {pricingPlan} -> pricingPlan) (\s@DescribePlaceIndexResponse' {} a -> s {pricingPlan = a} :: DescribePlaceIndexResponse)

-- | Tags associated with place index resource.
describePlaceIndexResponse_tags :: Lens.Lens' DescribePlaceIndexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describePlaceIndexResponse_tags = Lens.lens (\DescribePlaceIndexResponse' {tags} -> tags) (\s@DescribePlaceIndexResponse' {} a -> s {tags = a} :: DescribePlaceIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePlaceIndexResponse_httpStatus :: Lens.Lens' DescribePlaceIndexResponse Prelude.Int
describePlaceIndexResponse_httpStatus = Lens.lens (\DescribePlaceIndexResponse' {httpStatus} -> httpStatus) (\s@DescribePlaceIndexResponse' {} a -> s {httpStatus = a} :: DescribePlaceIndexResponse)

-- | The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describePlaceIndexResponse_createTime :: Lens.Lens' DescribePlaceIndexResponse Prelude.UTCTime
describePlaceIndexResponse_createTime = Lens.lens (\DescribePlaceIndexResponse' {createTime} -> createTime) (\s@DescribePlaceIndexResponse' {} a -> s {createTime = a} :: DescribePlaceIndexResponse) Prelude.. Data._Time

-- | The data provider of geospatial data. Values can be one of the
-- following:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
describePlaceIndexResponse_dataSource :: Lens.Lens' DescribePlaceIndexResponse Prelude.Text
describePlaceIndexResponse_dataSource = Lens.lens (\DescribePlaceIndexResponse' {dataSource} -> dataSource) (\s@DescribePlaceIndexResponse' {} a -> s {dataSource = a} :: DescribePlaceIndexResponse)

-- | The specified data storage option for requesting Places.
describePlaceIndexResponse_dataSourceConfiguration :: Lens.Lens' DescribePlaceIndexResponse DataSourceConfiguration
describePlaceIndexResponse_dataSourceConfiguration = Lens.lens (\DescribePlaceIndexResponse' {dataSourceConfiguration} -> dataSourceConfiguration) (\s@DescribePlaceIndexResponse' {} a -> s {dataSourceConfiguration = a} :: DescribePlaceIndexResponse)

-- | The optional description for the place index resource.
describePlaceIndexResponse_description :: Lens.Lens' DescribePlaceIndexResponse Prelude.Text
describePlaceIndexResponse_description = Lens.lens (\DescribePlaceIndexResponse' {description} -> description) (\s@DescribePlaceIndexResponse' {} a -> s {description = a} :: DescribePlaceIndexResponse)

-- | The Amazon Resource Name (ARN) for the place index resource. Used to
-- specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:place-index\/ExamplePlaceIndex@
describePlaceIndexResponse_indexArn :: Lens.Lens' DescribePlaceIndexResponse Prelude.Text
describePlaceIndexResponse_indexArn = Lens.lens (\DescribePlaceIndexResponse' {indexArn} -> indexArn) (\s@DescribePlaceIndexResponse' {} a -> s {indexArn = a} :: DescribePlaceIndexResponse)

-- | The name of the place index resource being described.
describePlaceIndexResponse_indexName :: Lens.Lens' DescribePlaceIndexResponse Prelude.Text
describePlaceIndexResponse_indexName = Lens.lens (\DescribePlaceIndexResponse' {indexName} -> indexName) (\s@DescribePlaceIndexResponse' {} a -> s {indexName = a} :: DescribePlaceIndexResponse)

-- | The timestamp for when the place index resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describePlaceIndexResponse_updateTime :: Lens.Lens' DescribePlaceIndexResponse Prelude.UTCTime
describePlaceIndexResponse_updateTime = Lens.lens (\DescribePlaceIndexResponse' {updateTime} -> updateTime) (\s@DescribePlaceIndexResponse' {} a -> s {updateTime = a} :: DescribePlaceIndexResponse) Prelude.. Data._Time

instance Prelude.NFData DescribePlaceIndexResponse where
  rnf DescribePlaceIndexResponse' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf dataSourceConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf indexArn
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf updateTime

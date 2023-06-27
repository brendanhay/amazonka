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
-- Module      : Amazonka.Location.DescribeMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the map resource details.
module Amazonka.Location.DescribeMap
  ( -- * Creating a Request
    DescribeMap (..),
    newDescribeMap,

    -- * Request Lenses
    describeMap_mapName,

    -- * Destructuring the Response
    DescribeMapResponse (..),
    newDescribeMapResponse,

    -- * Response Lenses
    describeMapResponse_pricingPlan,
    describeMapResponse_tags,
    describeMapResponse_httpStatus,
    describeMapResponse_configuration,
    describeMapResponse_createTime,
    describeMapResponse_dataSource,
    describeMapResponse_description,
    describeMapResponse_mapArn,
    describeMapResponse_mapName,
    describeMapResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMap' smart constructor.
data DescribeMap = DescribeMap'
  { -- | The name of the map resource.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapName', 'describeMap_mapName' - The name of the map resource.
newDescribeMap ::
  -- | 'mapName'
  Prelude.Text ->
  DescribeMap
newDescribeMap pMapName_ =
  DescribeMap' {mapName = pMapName_}

-- | The name of the map resource.
describeMap_mapName :: Lens.Lens' DescribeMap Prelude.Text
describeMap_mapName = Lens.lens (\DescribeMap' {mapName} -> mapName) (\s@DescribeMap' {} a -> s {mapName = a} :: DescribeMap)

instance Core.AWSRequest DescribeMap where
  type AWSResponse DescribeMap = DescribeMapResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMapResponse'
            Prelude.<$> (x Data..?> "PricingPlan")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Configuration")
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "DataSource")
            Prelude.<*> (x Data..:> "Description")
            Prelude.<*> (x Data..:> "MapArn")
            Prelude.<*> (x Data..:> "MapName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable DescribeMap where
  hashWithSalt _salt DescribeMap' {..} =
    _salt `Prelude.hashWithSalt` mapName

instance Prelude.NFData DescribeMap where
  rnf DescribeMap' {..} = Prelude.rnf mapName

instance Data.ToHeaders DescribeMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeMap where
  toPath DescribeMap' {..} =
    Prelude.mconcat
      ["/maps/v0/maps/", Data.toBS mapName]

instance Data.ToQuery DescribeMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMapResponse' smart constructor.
data DescribeMapResponse = DescribeMapResponse'
  { -- | No longer used. Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Tags associated with the map resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies the map tile style selected from a partner data provider.
    configuration :: MapConfiguration,
    -- | The timestamp for when the map resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | Specifies the data provider for the associated map tiles.
    dataSource :: Prelude.Text,
    -- | The optional description for the map resource.
    description :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the map resource. Used to specify a
    -- resource across all Amazon Web Services.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
    mapArn :: Prelude.Text,
    -- | The map style selected from an available provider.
    mapName :: Prelude.Text,
    -- | The timestamp for when the map resource was last update in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'describeMapResponse_pricingPlan' - No longer used. Always returns @RequestBasedUsage@.
--
-- 'tags', 'describeMapResponse_tags' - Tags associated with the map resource.
--
-- 'httpStatus', 'describeMapResponse_httpStatus' - The response's http status code.
--
-- 'configuration', 'describeMapResponse_configuration' - Specifies the map tile style selected from a partner data provider.
--
-- 'createTime', 'describeMapResponse_createTime' - The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'dataSource', 'describeMapResponse_dataSource' - Specifies the data provider for the associated map tiles.
--
-- 'description', 'describeMapResponse_description' - The optional description for the map resource.
--
-- 'mapArn', 'describeMapResponse_mapArn' - The Amazon Resource Name (ARN) for the map resource. Used to specify a
-- resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
--
-- 'mapName', 'describeMapResponse_mapName' - The map style selected from an available provider.
--
-- 'updateTime', 'describeMapResponse_updateTime' - The timestamp for when the map resource was last update in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newDescribeMapResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuration'
  MapConfiguration ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'mapArn'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  DescribeMapResponse
newDescribeMapResponse
  pHttpStatus_
  pConfiguration_
  pCreateTime_
  pDataSource_
  pDescription_
  pMapArn_
  pMapName_
  pUpdateTime_ =
    DescribeMapResponse'
      { pricingPlan = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        configuration = pConfiguration_,
        createTime = Data._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        mapArn = pMapArn_,
        mapName = pMapName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | No longer used. Always returns @RequestBasedUsage@.
describeMapResponse_pricingPlan :: Lens.Lens' DescribeMapResponse (Prelude.Maybe PricingPlan)
describeMapResponse_pricingPlan = Lens.lens (\DescribeMapResponse' {pricingPlan} -> pricingPlan) (\s@DescribeMapResponse' {} a -> s {pricingPlan = a} :: DescribeMapResponse)

-- | Tags associated with the map resource.
describeMapResponse_tags :: Lens.Lens' DescribeMapResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeMapResponse_tags = Lens.lens (\DescribeMapResponse' {tags} -> tags) (\s@DescribeMapResponse' {} a -> s {tags = a} :: DescribeMapResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMapResponse_httpStatus :: Lens.Lens' DescribeMapResponse Prelude.Int
describeMapResponse_httpStatus = Lens.lens (\DescribeMapResponse' {httpStatus} -> httpStatus) (\s@DescribeMapResponse' {} a -> s {httpStatus = a} :: DescribeMapResponse)

-- | Specifies the map tile style selected from a partner data provider.
describeMapResponse_configuration :: Lens.Lens' DescribeMapResponse MapConfiguration
describeMapResponse_configuration = Lens.lens (\DescribeMapResponse' {configuration} -> configuration) (\s@DescribeMapResponse' {} a -> s {configuration = a} :: DescribeMapResponse)

-- | The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeMapResponse_createTime :: Lens.Lens' DescribeMapResponse Prelude.UTCTime
describeMapResponse_createTime = Lens.lens (\DescribeMapResponse' {createTime} -> createTime) (\s@DescribeMapResponse' {} a -> s {createTime = a} :: DescribeMapResponse) Prelude.. Data._Time

-- | Specifies the data provider for the associated map tiles.
describeMapResponse_dataSource :: Lens.Lens' DescribeMapResponse Prelude.Text
describeMapResponse_dataSource = Lens.lens (\DescribeMapResponse' {dataSource} -> dataSource) (\s@DescribeMapResponse' {} a -> s {dataSource = a} :: DescribeMapResponse)

-- | The optional description for the map resource.
describeMapResponse_description :: Lens.Lens' DescribeMapResponse Prelude.Text
describeMapResponse_description = Lens.lens (\DescribeMapResponse' {description} -> description) (\s@DescribeMapResponse' {} a -> s {description = a} :: DescribeMapResponse)

-- | The Amazon Resource Name (ARN) for the map resource. Used to specify a
-- resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
describeMapResponse_mapArn :: Lens.Lens' DescribeMapResponse Prelude.Text
describeMapResponse_mapArn = Lens.lens (\DescribeMapResponse' {mapArn} -> mapArn) (\s@DescribeMapResponse' {} a -> s {mapArn = a} :: DescribeMapResponse)

-- | The map style selected from an available provider.
describeMapResponse_mapName :: Lens.Lens' DescribeMapResponse Prelude.Text
describeMapResponse_mapName = Lens.lens (\DescribeMapResponse' {mapName} -> mapName) (\s@DescribeMapResponse' {} a -> s {mapName = a} :: DescribeMapResponse)

-- | The timestamp for when the map resource was last update in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeMapResponse_updateTime :: Lens.Lens' DescribeMapResponse Prelude.UTCTime
describeMapResponse_updateTime = Lens.lens (\DescribeMapResponse' {updateTime} -> updateTime) (\s@DescribeMapResponse' {} a -> s {updateTime = a} :: DescribeMapResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeMapResponse where
  rnf DescribeMapResponse' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf mapArn
      `Prelude.seq` Prelude.rnf mapName
      `Prelude.seq` Prelude.rnf updateTime

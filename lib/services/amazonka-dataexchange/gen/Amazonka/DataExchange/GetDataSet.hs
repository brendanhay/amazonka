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
-- Module      : Amazonka.DataExchange.GetDataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a data set.
module Amazonka.DataExchange.GetDataSet
  ( -- * Creating a Request
    GetDataSet (..),
    newGetDataSet,

    -- * Request Lenses
    getDataSet_dataSetId,

    -- * Destructuring the Response
    GetDataSetResponse (..),
    newGetDataSetResponse,

    -- * Response Lenses
    getDataSetResponse_arn,
    getDataSetResponse_assetType,
    getDataSetResponse_createdAt,
    getDataSetResponse_description,
    getDataSetResponse_id,
    getDataSetResponse_name,
    getDataSetResponse_origin,
    getDataSetResponse_originDetails,
    getDataSetResponse_sourceId,
    getDataSetResponse_tags,
    getDataSetResponse_updatedAt,
    getDataSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataSet' smart constructor.
data GetDataSet = GetDataSet'
  { -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'getDataSet_dataSetId' - The unique identifier for a data set.
newGetDataSet ::
  -- | 'dataSetId'
  Prelude.Text ->
  GetDataSet
newGetDataSet pDataSetId_ =
  GetDataSet' {dataSetId = pDataSetId_}

-- | The unique identifier for a data set.
getDataSet_dataSetId :: Lens.Lens' GetDataSet Prelude.Text
getDataSet_dataSetId = Lens.lens (\GetDataSet' {dataSetId} -> dataSetId) (\s@GetDataSet' {} a -> s {dataSetId = a} :: GetDataSet)

instance Core.AWSRequest GetDataSet where
  type AWSResponse GetDataSet = GetDataSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataSetResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetType")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Origin")
            Prelude.<*> (x Data..?> "OriginDetails")
            Prelude.<*> (x Data..?> "SourceId")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataSet where
  hashWithSalt _salt GetDataSet' {..} =
    _salt `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData GetDataSet where
  rnf GetDataSet' {..} = Prelude.rnf dataSetId

instance Data.ToHeaders GetDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataSet where
  toPath GetDataSet' {..} =
    Prelude.mconcat
      ["/v1/data-sets/", Data.toBS dataSetId]

instance Data.ToQuery GetDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataSetResponse' smart constructor.
data GetDataSetResponse = GetDataSetResponse'
  { -- | The ARN for the data set.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of asset that is added to a data set.
    assetType :: Prelude.Maybe AssetType,
    -- | The date and time that the data set was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The description for the data set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the data set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the data set.
    name :: Prelude.Maybe Prelude.Text,
    -- | A property that defines the data set as OWNED by the account (for
    -- providers) or ENTITLED to the account (for subscribers).
    origin :: Prelude.Maybe Origin,
    -- | If the origin of this data set is ENTITLED, includes the details for the
    -- product on AWS Marketplace.
    originDetails :: Prelude.Maybe OriginDetails,
    -- | The data set ID of the owned data set corresponding to the entitled data
    -- set being viewed. This parameter is returned when a data set owner is
    -- viewing the entitled copy of its owned data set.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the data set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time that the data set was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDataSetResponse_arn' - The ARN for the data set.
--
-- 'assetType', 'getDataSetResponse_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'getDataSetResponse_createdAt' - The date and time that the data set was created, in ISO 8601 format.
--
-- 'description', 'getDataSetResponse_description' - The description for the data set.
--
-- 'id', 'getDataSetResponse_id' - The unique identifier for the data set.
--
-- 'name', 'getDataSetResponse_name' - The name of the data set.
--
-- 'origin', 'getDataSetResponse_origin' - A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
--
-- 'originDetails', 'getDataSetResponse_originDetails' - If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
--
-- 'sourceId', 'getDataSetResponse_sourceId' - The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
--
-- 'tags', 'getDataSetResponse_tags' - The tags for the data set.
--
-- 'updatedAt', 'getDataSetResponse_updatedAt' - The date and time that the data set was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'getDataSetResponse_httpStatus' - The response's http status code.
newGetDataSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataSetResponse
newGetDataSetResponse pHttpStatus_ =
  GetDataSetResponse'
    { arn = Prelude.Nothing,
      assetType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      origin = Prelude.Nothing,
      originDetails = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the data set.
getDataSetResponse_arn :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.Text)
getDataSetResponse_arn = Lens.lens (\GetDataSetResponse' {arn} -> arn) (\s@GetDataSetResponse' {} a -> s {arn = a} :: GetDataSetResponse)

-- | The type of asset that is added to a data set.
getDataSetResponse_assetType :: Lens.Lens' GetDataSetResponse (Prelude.Maybe AssetType)
getDataSetResponse_assetType = Lens.lens (\GetDataSetResponse' {assetType} -> assetType) (\s@GetDataSetResponse' {} a -> s {assetType = a} :: GetDataSetResponse)

-- | The date and time that the data set was created, in ISO 8601 format.
getDataSetResponse_createdAt :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.UTCTime)
getDataSetResponse_createdAt = Lens.lens (\GetDataSetResponse' {createdAt} -> createdAt) (\s@GetDataSetResponse' {} a -> s {createdAt = a} :: GetDataSetResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the data set.
getDataSetResponse_description :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.Text)
getDataSetResponse_description = Lens.lens (\GetDataSetResponse' {description} -> description) (\s@GetDataSetResponse' {} a -> s {description = a} :: GetDataSetResponse)

-- | The unique identifier for the data set.
getDataSetResponse_id :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.Text)
getDataSetResponse_id = Lens.lens (\GetDataSetResponse' {id} -> id) (\s@GetDataSetResponse' {} a -> s {id = a} :: GetDataSetResponse)

-- | The name of the data set.
getDataSetResponse_name :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.Text)
getDataSetResponse_name = Lens.lens (\GetDataSetResponse' {name} -> name) (\s@GetDataSetResponse' {} a -> s {name = a} :: GetDataSetResponse)

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
getDataSetResponse_origin :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Origin)
getDataSetResponse_origin = Lens.lens (\GetDataSetResponse' {origin} -> origin) (\s@GetDataSetResponse' {} a -> s {origin = a} :: GetDataSetResponse)

-- | If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
getDataSetResponse_originDetails :: Lens.Lens' GetDataSetResponse (Prelude.Maybe OriginDetails)
getDataSetResponse_originDetails = Lens.lens (\GetDataSetResponse' {originDetails} -> originDetails) (\s@GetDataSetResponse' {} a -> s {originDetails = a} :: GetDataSetResponse)

-- | The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
getDataSetResponse_sourceId :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.Text)
getDataSetResponse_sourceId = Lens.lens (\GetDataSetResponse' {sourceId} -> sourceId) (\s@GetDataSetResponse' {} a -> s {sourceId = a} :: GetDataSetResponse)

-- | The tags for the data set.
getDataSetResponse_tags :: Lens.Lens' GetDataSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDataSetResponse_tags = Lens.lens (\GetDataSetResponse' {tags} -> tags) (\s@GetDataSetResponse' {} a -> s {tags = a} :: GetDataSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the data set was last updated, in ISO 8601
-- format.
getDataSetResponse_updatedAt :: Lens.Lens' GetDataSetResponse (Prelude.Maybe Prelude.UTCTime)
getDataSetResponse_updatedAt = Lens.lens (\GetDataSetResponse' {updatedAt} -> updatedAt) (\s@GetDataSetResponse' {} a -> s {updatedAt = a} :: GetDataSetResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getDataSetResponse_httpStatus :: Lens.Lens' GetDataSetResponse Prelude.Int
getDataSetResponse_httpStatus = Lens.lens (\GetDataSetResponse' {httpStatus} -> httpStatus) (\s@GetDataSetResponse' {} a -> s {httpStatus = a} :: GetDataSetResponse)

instance Prelude.NFData GetDataSetResponse where
  rnf GetDataSetResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf assetType `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf id `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf origin `Prelude.seq`
                  Prelude.rnf originDetails `Prelude.seq`
                    Prelude.rnf sourceId `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf updatedAt `Prelude.seq`
                          Prelude.rnf httpStatus

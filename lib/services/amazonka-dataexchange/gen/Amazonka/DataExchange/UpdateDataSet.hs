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
-- Module      : Amazonka.DataExchange.UpdateDataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a data set.
module Amazonka.DataExchange.UpdateDataSet
  ( -- * Creating a Request
    UpdateDataSet (..),
    newUpdateDataSet,

    -- * Request Lenses
    updateDataSet_description,
    updateDataSet_name,
    updateDataSet_dataSetId,

    -- * Destructuring the Response
    UpdateDataSetResponse (..),
    newUpdateDataSetResponse,

    -- * Response Lenses
    updateDataSetResponse_arn,
    updateDataSetResponse_assetType,
    updateDataSetResponse_createdAt,
    updateDataSetResponse_description,
    updateDataSetResponse_id,
    updateDataSetResponse_name,
    updateDataSetResponse_origin,
    updateDataSetResponse_originDetails,
    updateDataSetResponse_sourceId,
    updateDataSetResponse_updatedAt,
    updateDataSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataSet' smart constructor.
data UpdateDataSet = UpdateDataSet'
  { -- | The description for the data set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the data set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDataSet_description' - The description for the data set.
--
-- 'name', 'updateDataSet_name' - The name of the data set.
--
-- 'dataSetId', 'updateDataSet_dataSetId' - The unique identifier for a data set.
newUpdateDataSet ::
  -- | 'dataSetId'
  Prelude.Text ->
  UpdateDataSet
newUpdateDataSet pDataSetId_ =
  UpdateDataSet'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      dataSetId = pDataSetId_
    }

-- | The description for the data set.
updateDataSet_description :: Lens.Lens' UpdateDataSet (Prelude.Maybe Prelude.Text)
updateDataSet_description = Lens.lens (\UpdateDataSet' {description} -> description) (\s@UpdateDataSet' {} a -> s {description = a} :: UpdateDataSet)

-- | The name of the data set.
updateDataSet_name :: Lens.Lens' UpdateDataSet (Prelude.Maybe Prelude.Text)
updateDataSet_name = Lens.lens (\UpdateDataSet' {name} -> name) (\s@UpdateDataSet' {} a -> s {name = a} :: UpdateDataSet)

-- | The unique identifier for a data set.
updateDataSet_dataSetId :: Lens.Lens' UpdateDataSet Prelude.Text
updateDataSet_dataSetId = Lens.lens (\UpdateDataSet' {dataSetId} -> dataSetId) (\s@UpdateDataSet' {} a -> s {dataSetId = a} :: UpdateDataSet)

instance Core.AWSRequest UpdateDataSet where
  type
    AWSResponse UpdateDataSet =
      UpdateDataSetResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSetResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetType")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Origin")
            Prelude.<*> (x Data..?> "OriginDetails")
            Prelude.<*> (x Data..?> "SourceId")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataSet where
  hashWithSalt _salt UpdateDataSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData UpdateDataSet where
  rnf UpdateDataSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders UpdateDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataSet where
  toJSON UpdateDataSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateDataSet where
  toPath UpdateDataSet' {..} =
    Prelude.mconcat
      ["/v1/data-sets/", Data.toBS dataSetId]

instance Data.ToQuery UpdateDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSetResponse' smart constructor.
data UpdateDataSetResponse = UpdateDataSetResponse'
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
    -- | The date and time that the data set was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateDataSetResponse_arn' - The ARN for the data set.
--
-- 'assetType', 'updateDataSetResponse_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'updateDataSetResponse_createdAt' - The date and time that the data set was created, in ISO 8601 format.
--
-- 'description', 'updateDataSetResponse_description' - The description for the data set.
--
-- 'id', 'updateDataSetResponse_id' - The unique identifier for the data set.
--
-- 'name', 'updateDataSetResponse_name' - The name of the data set.
--
-- 'origin', 'updateDataSetResponse_origin' - A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
--
-- 'originDetails', 'updateDataSetResponse_originDetails' - If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
--
-- 'sourceId', 'updateDataSetResponse_sourceId' - The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
--
-- 'updatedAt', 'updateDataSetResponse_updatedAt' - The date and time that the data set was last updated, in ISO 8601
-- format.
--
-- 'httpStatus', 'updateDataSetResponse_httpStatus' - The response's http status code.
newUpdateDataSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataSetResponse
newUpdateDataSetResponse pHttpStatus_ =
  UpdateDataSetResponse'
    { arn = Prelude.Nothing,
      assetType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      origin = Prelude.Nothing,
      originDetails = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the data set.
updateDataSetResponse_arn :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_arn = Lens.lens (\UpdateDataSetResponse' {arn} -> arn) (\s@UpdateDataSetResponse' {} a -> s {arn = a} :: UpdateDataSetResponse)

-- | The type of asset that is added to a data set.
updateDataSetResponse_assetType :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe AssetType)
updateDataSetResponse_assetType = Lens.lens (\UpdateDataSetResponse' {assetType} -> assetType) (\s@UpdateDataSetResponse' {} a -> s {assetType = a} :: UpdateDataSetResponse)

-- | The date and time that the data set was created, in ISO 8601 format.
updateDataSetResponse_createdAt :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.UTCTime)
updateDataSetResponse_createdAt = Lens.lens (\UpdateDataSetResponse' {createdAt} -> createdAt) (\s@UpdateDataSetResponse' {} a -> s {createdAt = a} :: UpdateDataSetResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the data set.
updateDataSetResponse_description :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_description = Lens.lens (\UpdateDataSetResponse' {description} -> description) (\s@UpdateDataSetResponse' {} a -> s {description = a} :: UpdateDataSetResponse)

-- | The unique identifier for the data set.
updateDataSetResponse_id :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_id = Lens.lens (\UpdateDataSetResponse' {id} -> id) (\s@UpdateDataSetResponse' {} a -> s {id = a} :: UpdateDataSetResponse)

-- | The name of the data set.
updateDataSetResponse_name :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_name = Lens.lens (\UpdateDataSetResponse' {name} -> name) (\s@UpdateDataSetResponse' {} a -> s {name = a} :: UpdateDataSetResponse)

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
updateDataSetResponse_origin :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Origin)
updateDataSetResponse_origin = Lens.lens (\UpdateDataSetResponse' {origin} -> origin) (\s@UpdateDataSetResponse' {} a -> s {origin = a} :: UpdateDataSetResponse)

-- | If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
updateDataSetResponse_originDetails :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe OriginDetails)
updateDataSetResponse_originDetails = Lens.lens (\UpdateDataSetResponse' {originDetails} -> originDetails) (\s@UpdateDataSetResponse' {} a -> s {originDetails = a} :: UpdateDataSetResponse)

-- | The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
updateDataSetResponse_sourceId :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_sourceId = Lens.lens (\UpdateDataSetResponse' {sourceId} -> sourceId) (\s@UpdateDataSetResponse' {} a -> s {sourceId = a} :: UpdateDataSetResponse)

-- | The date and time that the data set was last updated, in ISO 8601
-- format.
updateDataSetResponse_updatedAt :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.UTCTime)
updateDataSetResponse_updatedAt = Lens.lens (\UpdateDataSetResponse' {updatedAt} -> updatedAt) (\s@UpdateDataSetResponse' {} a -> s {updatedAt = a} :: UpdateDataSetResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateDataSetResponse_httpStatus :: Lens.Lens' UpdateDataSetResponse Prelude.Int
updateDataSetResponse_httpStatus = Lens.lens (\UpdateDataSetResponse' {httpStatus} -> httpStatus) (\s@UpdateDataSetResponse' {} a -> s {httpStatus = a} :: UpdateDataSetResponse)

instance Prelude.NFData UpdateDataSetResponse where
  rnf UpdateDataSetResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf originDetails
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.IoTSiteWise.BatchPutAssetPropertyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a list of asset property values to IoT SiteWise. Each value is a
-- timestamp-quality-value (TQV) data point. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/ingest-api.html Ingesting data using the API>
-- in the /IoT SiteWise User Guide/.
--
-- To identify an asset property, you must specify one of the following:
--
-- -   The @assetId@ and @propertyId@ of an asset property.
--
-- -   A @propertyAlias@, which is a data stream alias (for example,
--     @\/company\/windfarm\/3\/turbine\/7\/temperature@). To define an
--     asset property\'s alias, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
--
-- With respect to Unix epoch time, IoT SiteWise accepts only TQVs that
-- have a timestamp of no more than 7 days in the past and no more than 10
-- minutes in the future. IoT SiteWise rejects timestamps outside of the
-- inclusive range of [-7 days, +10 minutes] and returns a
-- @TimestampOutOfRangeException@ error.
--
-- For each asset property, IoT SiteWise overwrites TQVs with duplicate
-- timestamps unless the newer TQV has a different quality. For example, if
-- you store a TQV @{T1, GOOD, V1}@, then storing @{T1, GOOD, V2}@ replaces
-- the existing TQV.
--
-- IoT SiteWise authorizes access to each @BatchPutAssetPropertyValue@
-- entry individually. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/security_iam_service-with-iam.html#security_iam_service-with-iam-id-based-policies-batchputassetpropertyvalue-action BatchPutAssetPropertyValue authorization>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.BatchPutAssetPropertyValue
  ( -- * Creating a Request
    BatchPutAssetPropertyValue (..),
    newBatchPutAssetPropertyValue,

    -- * Request Lenses
    batchPutAssetPropertyValue_entries,

    -- * Destructuring the Response
    BatchPutAssetPropertyValueResponse (..),
    newBatchPutAssetPropertyValueResponse,

    -- * Response Lenses
    batchPutAssetPropertyValueResponse_httpStatus,
    batchPutAssetPropertyValueResponse_errorEntries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutAssetPropertyValue' smart constructor.
data BatchPutAssetPropertyValue = BatchPutAssetPropertyValue'
  { -- | The list of asset property value entries for the batch put request. You
    -- can specify up to 10 entries per request.
    entries :: [PutAssetPropertyValueEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAssetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'batchPutAssetPropertyValue_entries' - The list of asset property value entries for the batch put request. You
-- can specify up to 10 entries per request.
newBatchPutAssetPropertyValue ::
  BatchPutAssetPropertyValue
newBatchPutAssetPropertyValue =
  BatchPutAssetPropertyValue'
    { entries =
        Prelude.mempty
    }

-- | The list of asset property value entries for the batch put request. You
-- can specify up to 10 entries per request.
batchPutAssetPropertyValue_entries :: Lens.Lens' BatchPutAssetPropertyValue [PutAssetPropertyValueEntry]
batchPutAssetPropertyValue_entries = Lens.lens (\BatchPutAssetPropertyValue' {entries} -> entries) (\s@BatchPutAssetPropertyValue' {} a -> s {entries = a} :: BatchPutAssetPropertyValue) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutAssetPropertyValue where
  type
    AWSResponse BatchPutAssetPropertyValue =
      BatchPutAssetPropertyValueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutAssetPropertyValueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchPutAssetPropertyValue where
  hashWithSalt _salt BatchPutAssetPropertyValue' {..} =
    _salt `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchPutAssetPropertyValue where
  rnf BatchPutAssetPropertyValue' {..} =
    Prelude.rnf entries

instance Data.ToHeaders BatchPutAssetPropertyValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchPutAssetPropertyValue where
  toJSON BatchPutAssetPropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("entries" Data..= entries)]
      )

instance Data.ToPath BatchPutAssetPropertyValue where
  toPath = Prelude.const "/properties"

instance Data.ToQuery BatchPutAssetPropertyValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutAssetPropertyValueResponse' smart constructor.
data BatchPutAssetPropertyValueResponse = BatchPutAssetPropertyValueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the errors (if any) associated with the batch put request.
    -- Each error entry contains the @entryId@ of the entry that failed.
    errorEntries :: [BatchPutAssetPropertyErrorEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAssetPropertyValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchPutAssetPropertyValueResponse_httpStatus' - The response's http status code.
--
-- 'errorEntries', 'batchPutAssetPropertyValueResponse_errorEntries' - A list of the errors (if any) associated with the batch put request.
-- Each error entry contains the @entryId@ of the entry that failed.
newBatchPutAssetPropertyValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutAssetPropertyValueResponse
newBatchPutAssetPropertyValueResponse pHttpStatus_ =
  BatchPutAssetPropertyValueResponse'
    { httpStatus =
        pHttpStatus_,
      errorEntries = Prelude.mempty
    }

-- | The response's http status code.
batchPutAssetPropertyValueResponse_httpStatus :: Lens.Lens' BatchPutAssetPropertyValueResponse Prelude.Int
batchPutAssetPropertyValueResponse_httpStatus = Lens.lens (\BatchPutAssetPropertyValueResponse' {httpStatus} -> httpStatus) (\s@BatchPutAssetPropertyValueResponse' {} a -> s {httpStatus = a} :: BatchPutAssetPropertyValueResponse)

-- | A list of the errors (if any) associated with the batch put request.
-- Each error entry contains the @entryId@ of the entry that failed.
batchPutAssetPropertyValueResponse_errorEntries :: Lens.Lens' BatchPutAssetPropertyValueResponse [BatchPutAssetPropertyErrorEntry]
batchPutAssetPropertyValueResponse_errorEntries = Lens.lens (\BatchPutAssetPropertyValueResponse' {errorEntries} -> errorEntries) (\s@BatchPutAssetPropertyValueResponse' {} a -> s {errorEntries = a} :: BatchPutAssetPropertyValueResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchPutAssetPropertyValueResponse
  where
  rnf BatchPutAssetPropertyValueResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf errorEntries

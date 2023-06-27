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
-- Module      : Amazonka.QuickSight.PutDataSetRefreshProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the dataset refresh properties for the dataset.
module Amazonka.QuickSight.PutDataSetRefreshProperties
  ( -- * Creating a Request
    PutDataSetRefreshProperties (..),
    newPutDataSetRefreshProperties,

    -- * Request Lenses
    putDataSetRefreshProperties_awsAccountId,
    putDataSetRefreshProperties_dataSetId,
    putDataSetRefreshProperties_dataSetRefreshProperties,

    -- * Destructuring the Response
    PutDataSetRefreshPropertiesResponse (..),
    newPutDataSetRefreshPropertiesResponse,

    -- * Response Lenses
    putDataSetRefreshPropertiesResponse_requestId,
    putDataSetRefreshPropertiesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDataSetRefreshProperties' smart constructor.
data PutDataSetRefreshProperties = PutDataSetRefreshProperties'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Text,
    -- | The dataset refresh properties.
    dataSetRefreshProperties :: DataSetRefreshProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataSetRefreshProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'putDataSetRefreshProperties_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'putDataSetRefreshProperties_dataSetId' - The ID of the dataset.
--
-- 'dataSetRefreshProperties', 'putDataSetRefreshProperties_dataSetRefreshProperties' - The dataset refresh properties.
newPutDataSetRefreshProperties ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'dataSetRefreshProperties'
  DataSetRefreshProperties ->
  PutDataSetRefreshProperties
newPutDataSetRefreshProperties
  pAwsAccountId_
  pDataSetId_
  pDataSetRefreshProperties_ =
    PutDataSetRefreshProperties'
      { awsAccountId =
          pAwsAccountId_,
        dataSetId = pDataSetId_,
        dataSetRefreshProperties =
          pDataSetRefreshProperties_
      }

-- | The Amazon Web Services account ID.
putDataSetRefreshProperties_awsAccountId :: Lens.Lens' PutDataSetRefreshProperties Prelude.Text
putDataSetRefreshProperties_awsAccountId = Lens.lens (\PutDataSetRefreshProperties' {awsAccountId} -> awsAccountId) (\s@PutDataSetRefreshProperties' {} a -> s {awsAccountId = a} :: PutDataSetRefreshProperties)

-- | The ID of the dataset.
putDataSetRefreshProperties_dataSetId :: Lens.Lens' PutDataSetRefreshProperties Prelude.Text
putDataSetRefreshProperties_dataSetId = Lens.lens (\PutDataSetRefreshProperties' {dataSetId} -> dataSetId) (\s@PutDataSetRefreshProperties' {} a -> s {dataSetId = a} :: PutDataSetRefreshProperties)

-- | The dataset refresh properties.
putDataSetRefreshProperties_dataSetRefreshProperties :: Lens.Lens' PutDataSetRefreshProperties DataSetRefreshProperties
putDataSetRefreshProperties_dataSetRefreshProperties = Lens.lens (\PutDataSetRefreshProperties' {dataSetRefreshProperties} -> dataSetRefreshProperties) (\s@PutDataSetRefreshProperties' {} a -> s {dataSetRefreshProperties = a} :: PutDataSetRefreshProperties)

instance Core.AWSRequest PutDataSetRefreshProperties where
  type
    AWSResponse PutDataSetRefreshProperties =
      PutDataSetRefreshPropertiesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDataSetRefreshPropertiesResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDataSetRefreshProperties where
  hashWithSalt _salt PutDataSetRefreshProperties' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` dataSetRefreshProperties

instance Prelude.NFData PutDataSetRefreshProperties where
  rnf PutDataSetRefreshProperties' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf dataSetRefreshProperties

instance Data.ToHeaders PutDataSetRefreshProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDataSetRefreshProperties where
  toJSON PutDataSetRefreshProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DataSetRefreshProperties"
                  Data..= dataSetRefreshProperties
              )
          ]
      )

instance Data.ToPath PutDataSetRefreshProperties where
  toPath PutDataSetRefreshProperties' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-properties"
      ]

instance Data.ToQuery PutDataSetRefreshProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDataSetRefreshPropertiesResponse' smart constructor.
data PutDataSetRefreshPropertiesResponse = PutDataSetRefreshPropertiesResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataSetRefreshPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'putDataSetRefreshPropertiesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'putDataSetRefreshPropertiesResponse_status' - The HTTP status of the request.
newPutDataSetRefreshPropertiesResponse ::
  -- | 'status'
  Prelude.Int ->
  PutDataSetRefreshPropertiesResponse
newPutDataSetRefreshPropertiesResponse pStatus_ =
  PutDataSetRefreshPropertiesResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
putDataSetRefreshPropertiesResponse_requestId :: Lens.Lens' PutDataSetRefreshPropertiesResponse (Prelude.Maybe Prelude.Text)
putDataSetRefreshPropertiesResponse_requestId = Lens.lens (\PutDataSetRefreshPropertiesResponse' {requestId} -> requestId) (\s@PutDataSetRefreshPropertiesResponse' {} a -> s {requestId = a} :: PutDataSetRefreshPropertiesResponse)

-- | The HTTP status of the request.
putDataSetRefreshPropertiesResponse_status :: Lens.Lens' PutDataSetRefreshPropertiesResponse Prelude.Int
putDataSetRefreshPropertiesResponse_status = Lens.lens (\PutDataSetRefreshPropertiesResponse' {status} -> status) (\s@PutDataSetRefreshPropertiesResponse' {} a -> s {status = a} :: PutDataSetRefreshPropertiesResponse)

instance
  Prelude.NFData
    PutDataSetRefreshPropertiesResponse
  where
  rnf PutDataSetRefreshPropertiesResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

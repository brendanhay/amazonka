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
-- Module      : Amazonka.FinSpaceData.GetExternalDataViewAccessDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the credentials to access the external Dataview from an S3
-- location. To call this API:
--
-- -   You must retrieve the programmatic credentials.
--
-- -   You must be a member of a FinSpace user group, where the dataset
--     that you want to access has @Read Dataset Data@ permissions.
module Amazonka.FinSpaceData.GetExternalDataViewAccessDetails
  ( -- * Creating a Request
    GetExternalDataViewAccessDetails (..),
    newGetExternalDataViewAccessDetails,

    -- * Request Lenses
    getExternalDataViewAccessDetails_dataViewId,
    getExternalDataViewAccessDetails_datasetId,

    -- * Destructuring the Response
    GetExternalDataViewAccessDetailsResponse (..),
    newGetExternalDataViewAccessDetailsResponse,

    -- * Response Lenses
    getExternalDataViewAccessDetailsResponse_credentials,
    getExternalDataViewAccessDetailsResponse_s3Location,
    getExternalDataViewAccessDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExternalDataViewAccessDetails' smart constructor.
data GetExternalDataViewAccessDetails = GetExternalDataViewAccessDetails'
  { -- | The unique identifier for the Dataview that you want to access.
    dataViewId :: Prelude.Text,
    -- | The unique identifier for the Dataset.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExternalDataViewAccessDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataViewId', 'getExternalDataViewAccessDetails_dataViewId' - The unique identifier for the Dataview that you want to access.
--
-- 'datasetId', 'getExternalDataViewAccessDetails_datasetId' - The unique identifier for the Dataset.
newGetExternalDataViewAccessDetails ::
  -- | 'dataViewId'
  Prelude.Text ->
  -- | 'datasetId'
  Prelude.Text ->
  GetExternalDataViewAccessDetails
newGetExternalDataViewAccessDetails
  pDataViewId_
  pDatasetId_ =
    GetExternalDataViewAccessDetails'
      { dataViewId =
          pDataViewId_,
        datasetId = pDatasetId_
      }

-- | The unique identifier for the Dataview that you want to access.
getExternalDataViewAccessDetails_dataViewId :: Lens.Lens' GetExternalDataViewAccessDetails Prelude.Text
getExternalDataViewAccessDetails_dataViewId = Lens.lens (\GetExternalDataViewAccessDetails' {dataViewId} -> dataViewId) (\s@GetExternalDataViewAccessDetails' {} a -> s {dataViewId = a} :: GetExternalDataViewAccessDetails)

-- | The unique identifier for the Dataset.
getExternalDataViewAccessDetails_datasetId :: Lens.Lens' GetExternalDataViewAccessDetails Prelude.Text
getExternalDataViewAccessDetails_datasetId = Lens.lens (\GetExternalDataViewAccessDetails' {datasetId} -> datasetId) (\s@GetExternalDataViewAccessDetails' {} a -> s {datasetId = a} :: GetExternalDataViewAccessDetails)

instance
  Core.AWSRequest
    GetExternalDataViewAccessDetails
  where
  type
    AWSResponse GetExternalDataViewAccessDetails =
      GetExternalDataViewAccessDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExternalDataViewAccessDetailsResponse'
            Prelude.<$> (x Data..?> "credentials")
            Prelude.<*> (x Data..?> "s3Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetExternalDataViewAccessDetails
  where
  hashWithSalt
    _salt
    GetExternalDataViewAccessDetails' {..} =
      _salt
        `Prelude.hashWithSalt` dataViewId
        `Prelude.hashWithSalt` datasetId

instance
  Prelude.NFData
    GetExternalDataViewAccessDetails
  where
  rnf GetExternalDataViewAccessDetails' {..} =
    Prelude.rnf dataViewId `Prelude.seq`
      Prelude.rnf datasetId

instance
  Data.ToHeaders
    GetExternalDataViewAccessDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExternalDataViewAccessDetails where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetExternalDataViewAccessDetails where
  toPath GetExternalDataViewAccessDetails' {..} =
    Prelude.mconcat
      [ "/datasets/",
        Data.toBS datasetId,
        "/dataviewsv2/",
        Data.toBS dataViewId,
        "/external-access-details"
      ]

instance
  Data.ToQuery
    GetExternalDataViewAccessDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExternalDataViewAccessDetailsResponse' smart constructor.
data GetExternalDataViewAccessDetailsResponse = GetExternalDataViewAccessDetailsResponse'
  { -- | The credentials required to access the external Dataview from the S3
    -- location.
    credentials :: Prelude.Maybe AwsCredentials,
    -- | The location where the external Dataview is stored.
    s3Location :: Prelude.Maybe S3Location,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExternalDataViewAccessDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getExternalDataViewAccessDetailsResponse_credentials' - The credentials required to access the external Dataview from the S3
-- location.
--
-- 's3Location', 'getExternalDataViewAccessDetailsResponse_s3Location' - The location where the external Dataview is stored.
--
-- 'httpStatus', 'getExternalDataViewAccessDetailsResponse_httpStatus' - The response's http status code.
newGetExternalDataViewAccessDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExternalDataViewAccessDetailsResponse
newGetExternalDataViewAccessDetailsResponse
  pHttpStatus_ =
    GetExternalDataViewAccessDetailsResponse'
      { credentials =
          Prelude.Nothing,
        s3Location = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The credentials required to access the external Dataview from the S3
-- location.
getExternalDataViewAccessDetailsResponse_credentials :: Lens.Lens' GetExternalDataViewAccessDetailsResponse (Prelude.Maybe AwsCredentials)
getExternalDataViewAccessDetailsResponse_credentials = Lens.lens (\GetExternalDataViewAccessDetailsResponse' {credentials} -> credentials) (\s@GetExternalDataViewAccessDetailsResponse' {} a -> s {credentials = a} :: GetExternalDataViewAccessDetailsResponse)

-- | The location where the external Dataview is stored.
getExternalDataViewAccessDetailsResponse_s3Location :: Lens.Lens' GetExternalDataViewAccessDetailsResponse (Prelude.Maybe S3Location)
getExternalDataViewAccessDetailsResponse_s3Location = Lens.lens (\GetExternalDataViewAccessDetailsResponse' {s3Location} -> s3Location) (\s@GetExternalDataViewAccessDetailsResponse' {} a -> s {s3Location = a} :: GetExternalDataViewAccessDetailsResponse)

-- | The response's http status code.
getExternalDataViewAccessDetailsResponse_httpStatus :: Lens.Lens' GetExternalDataViewAccessDetailsResponse Prelude.Int
getExternalDataViewAccessDetailsResponse_httpStatus = Lens.lens (\GetExternalDataViewAccessDetailsResponse' {httpStatus} -> httpStatus) (\s@GetExternalDataViewAccessDetailsResponse' {} a -> s {httpStatus = a} :: GetExternalDataViewAccessDetailsResponse)

instance
  Prelude.NFData
    GetExternalDataViewAccessDetailsResponse
  where
  rnf GetExternalDataViewAccessDetailsResponse' {..} =
    Prelude.rnf credentials `Prelude.seq`
      Prelude.rnf s3Location `Prelude.seq`
        Prelude.rnf httpStatus

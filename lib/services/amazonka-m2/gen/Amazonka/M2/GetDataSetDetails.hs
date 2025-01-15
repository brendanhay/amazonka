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
-- Module      : Amazonka.M2.GetDataSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a specific data set.
module Amazonka.M2.GetDataSetDetails
  ( -- * Creating a Request
    GetDataSetDetails (..),
    newGetDataSetDetails,

    -- * Request Lenses
    getDataSetDetails_applicationId,
    getDataSetDetails_dataSetName,

    -- * Destructuring the Response
    GetDataSetDetailsResponse (..),
    newGetDataSetDetailsResponse,

    -- * Response Lenses
    getDataSetDetailsResponse_blocksize,
    getDataSetDetailsResponse_creationTime,
    getDataSetDetailsResponse_dataSetOrg,
    getDataSetDetailsResponse_lastReferencedTime,
    getDataSetDetailsResponse_lastUpdatedTime,
    getDataSetDetailsResponse_location,
    getDataSetDetailsResponse_recordLength,
    getDataSetDetailsResponse_httpStatus,
    getDataSetDetailsResponse_dataSetName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataSetDetails' smart constructor.
data GetDataSetDetails = GetDataSetDetails'
  { -- | The unique identifier of the application that this data set is
    -- associated with.
    applicationId :: Prelude.Text,
    -- | The name of the data set.
    dataSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getDataSetDetails_applicationId' - The unique identifier of the application that this data set is
-- associated with.
--
-- 'dataSetName', 'getDataSetDetails_dataSetName' - The name of the data set.
newGetDataSetDetails ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'dataSetName'
  Prelude.Text ->
  GetDataSetDetails
newGetDataSetDetails pApplicationId_ pDataSetName_ =
  GetDataSetDetails'
    { applicationId = pApplicationId_,
      dataSetName = pDataSetName_
    }

-- | The unique identifier of the application that this data set is
-- associated with.
getDataSetDetails_applicationId :: Lens.Lens' GetDataSetDetails Prelude.Text
getDataSetDetails_applicationId = Lens.lens (\GetDataSetDetails' {applicationId} -> applicationId) (\s@GetDataSetDetails' {} a -> s {applicationId = a} :: GetDataSetDetails)

-- | The name of the data set.
getDataSetDetails_dataSetName :: Lens.Lens' GetDataSetDetails Prelude.Text
getDataSetDetails_dataSetName = Lens.lens (\GetDataSetDetails' {dataSetName} -> dataSetName) (\s@GetDataSetDetails' {} a -> s {dataSetName = a} :: GetDataSetDetails)

instance Core.AWSRequest GetDataSetDetails where
  type
    AWSResponse GetDataSetDetails =
      GetDataSetDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataSetDetailsResponse'
            Prelude.<$> (x Data..?> "blocksize")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "dataSetOrg")
            Prelude.<*> (x Data..?> "lastReferencedTime")
            Prelude.<*> (x Data..?> "lastUpdatedTime")
            Prelude.<*> (x Data..?> "location")
            Prelude.<*> (x Data..?> "recordLength")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "dataSetName")
      )

instance Prelude.Hashable GetDataSetDetails where
  hashWithSalt _salt GetDataSetDetails' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` dataSetName

instance Prelude.NFData GetDataSetDetails where
  rnf GetDataSetDetails' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf dataSetName

instance Data.ToHeaders GetDataSetDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataSetDetails where
  toPath GetDataSetDetails' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/datasets/",
        Data.toBS dataSetName
      ]

instance Data.ToQuery GetDataSetDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataSetDetailsResponse' smart constructor.
data GetDataSetDetailsResponse = GetDataSetDetailsResponse'
  { -- | The size of the block on disk.
    blocksize :: Prelude.Maybe Prelude.Int,
    -- | The timestamp when the data set was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The type of data set. The only supported value is VSAM.
    dataSetOrg :: Prelude.Maybe DatasetDetailOrgAttributes,
    -- | The last time the data set was referenced.
    lastReferencedTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the data set was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The location where the data set is stored.
    location :: Prelude.Maybe Prelude.Text,
    -- | The length of records in the data set.
    recordLength :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the data set.
    dataSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataSetDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blocksize', 'getDataSetDetailsResponse_blocksize' - The size of the block on disk.
--
-- 'creationTime', 'getDataSetDetailsResponse_creationTime' - The timestamp when the data set was created.
--
-- 'dataSetOrg', 'getDataSetDetailsResponse_dataSetOrg' - The type of data set. The only supported value is VSAM.
--
-- 'lastReferencedTime', 'getDataSetDetailsResponse_lastReferencedTime' - The last time the data set was referenced.
--
-- 'lastUpdatedTime', 'getDataSetDetailsResponse_lastUpdatedTime' - The last time the data set was updated.
--
-- 'location', 'getDataSetDetailsResponse_location' - The location where the data set is stored.
--
-- 'recordLength', 'getDataSetDetailsResponse_recordLength' - The length of records in the data set.
--
-- 'httpStatus', 'getDataSetDetailsResponse_httpStatus' - The response's http status code.
--
-- 'dataSetName', 'getDataSetDetailsResponse_dataSetName' - The name of the data set.
newGetDataSetDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'dataSetName'
  Prelude.Text ->
  GetDataSetDetailsResponse
newGetDataSetDetailsResponse
  pHttpStatus_
  pDataSetName_ =
    GetDataSetDetailsResponse'
      { blocksize =
          Prelude.Nothing,
        creationTime = Prelude.Nothing,
        dataSetOrg = Prelude.Nothing,
        lastReferencedTime = Prelude.Nothing,
        lastUpdatedTime = Prelude.Nothing,
        location = Prelude.Nothing,
        recordLength = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        dataSetName = pDataSetName_
      }

-- | The size of the block on disk.
getDataSetDetailsResponse_blocksize :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.Int)
getDataSetDetailsResponse_blocksize = Lens.lens (\GetDataSetDetailsResponse' {blocksize} -> blocksize) (\s@GetDataSetDetailsResponse' {} a -> s {blocksize = a} :: GetDataSetDetailsResponse)

-- | The timestamp when the data set was created.
getDataSetDetailsResponse_creationTime :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.UTCTime)
getDataSetDetailsResponse_creationTime = Lens.lens (\GetDataSetDetailsResponse' {creationTime} -> creationTime) (\s@GetDataSetDetailsResponse' {} a -> s {creationTime = a} :: GetDataSetDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | The type of data set. The only supported value is VSAM.
getDataSetDetailsResponse_dataSetOrg :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe DatasetDetailOrgAttributes)
getDataSetDetailsResponse_dataSetOrg = Lens.lens (\GetDataSetDetailsResponse' {dataSetOrg} -> dataSetOrg) (\s@GetDataSetDetailsResponse' {} a -> s {dataSetOrg = a} :: GetDataSetDetailsResponse)

-- | The last time the data set was referenced.
getDataSetDetailsResponse_lastReferencedTime :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.UTCTime)
getDataSetDetailsResponse_lastReferencedTime = Lens.lens (\GetDataSetDetailsResponse' {lastReferencedTime} -> lastReferencedTime) (\s@GetDataSetDetailsResponse' {} a -> s {lastReferencedTime = a} :: GetDataSetDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | The last time the data set was updated.
getDataSetDetailsResponse_lastUpdatedTime :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.UTCTime)
getDataSetDetailsResponse_lastUpdatedTime = Lens.lens (\GetDataSetDetailsResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@GetDataSetDetailsResponse' {} a -> s {lastUpdatedTime = a} :: GetDataSetDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | The location where the data set is stored.
getDataSetDetailsResponse_location :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.Text)
getDataSetDetailsResponse_location = Lens.lens (\GetDataSetDetailsResponse' {location} -> location) (\s@GetDataSetDetailsResponse' {} a -> s {location = a} :: GetDataSetDetailsResponse)

-- | The length of records in the data set.
getDataSetDetailsResponse_recordLength :: Lens.Lens' GetDataSetDetailsResponse (Prelude.Maybe Prelude.Int)
getDataSetDetailsResponse_recordLength = Lens.lens (\GetDataSetDetailsResponse' {recordLength} -> recordLength) (\s@GetDataSetDetailsResponse' {} a -> s {recordLength = a} :: GetDataSetDetailsResponse)

-- | The response's http status code.
getDataSetDetailsResponse_httpStatus :: Lens.Lens' GetDataSetDetailsResponse Prelude.Int
getDataSetDetailsResponse_httpStatus = Lens.lens (\GetDataSetDetailsResponse' {httpStatus} -> httpStatus) (\s@GetDataSetDetailsResponse' {} a -> s {httpStatus = a} :: GetDataSetDetailsResponse)

-- | The name of the data set.
getDataSetDetailsResponse_dataSetName :: Lens.Lens' GetDataSetDetailsResponse Prelude.Text
getDataSetDetailsResponse_dataSetName = Lens.lens (\GetDataSetDetailsResponse' {dataSetName} -> dataSetName) (\s@GetDataSetDetailsResponse' {} a -> s {dataSetName = a} :: GetDataSetDetailsResponse)

instance Prelude.NFData GetDataSetDetailsResponse where
  rnf GetDataSetDetailsResponse' {..} =
    Prelude.rnf blocksize `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf dataSetOrg `Prelude.seq`
          Prelude.rnf lastReferencedTime `Prelude.seq`
            Prelude.rnf lastUpdatedTime `Prelude.seq`
              Prelude.rnf location `Prelude.seq`
                Prelude.rnf recordLength `Prelude.seq`
                  Prelude.rnf httpStatus `Prelude.seq`
                    Prelude.rnf dataSetName

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
-- Module      : Amazonka.QuickSight.ListRefreshSchedules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the refresh schedules of a dataset. Each dataset can have up to 5
-- schedules.
module Amazonka.QuickSight.ListRefreshSchedules
  ( -- * Creating a Request
    ListRefreshSchedules (..),
    newListRefreshSchedules,

    -- * Request Lenses
    listRefreshSchedules_awsAccountId,
    listRefreshSchedules_dataSetId,

    -- * Destructuring the Response
    ListRefreshSchedulesResponse (..),
    newListRefreshSchedulesResponse,

    -- * Response Lenses
    listRefreshSchedulesResponse_refreshSchedules,
    listRefreshSchedulesResponse_requestId,
    listRefreshSchedulesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRefreshSchedules' smart constructor.
data ListRefreshSchedules = ListRefreshSchedules'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRefreshSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'listRefreshSchedules_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'listRefreshSchedules_dataSetId' - The ID of the dataset.
newListRefreshSchedules ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  ListRefreshSchedules
newListRefreshSchedules pAwsAccountId_ pDataSetId_ =
  ListRefreshSchedules'
    { awsAccountId =
        pAwsAccountId_,
      dataSetId = pDataSetId_
    }

-- | The Amazon Web Services account ID.
listRefreshSchedules_awsAccountId :: Lens.Lens' ListRefreshSchedules Prelude.Text
listRefreshSchedules_awsAccountId = Lens.lens (\ListRefreshSchedules' {awsAccountId} -> awsAccountId) (\s@ListRefreshSchedules' {} a -> s {awsAccountId = a} :: ListRefreshSchedules)

-- | The ID of the dataset.
listRefreshSchedules_dataSetId :: Lens.Lens' ListRefreshSchedules Prelude.Text
listRefreshSchedules_dataSetId = Lens.lens (\ListRefreshSchedules' {dataSetId} -> dataSetId) (\s@ListRefreshSchedules' {} a -> s {dataSetId = a} :: ListRefreshSchedules)

instance Core.AWSRequest ListRefreshSchedules where
  type
    AWSResponse ListRefreshSchedules =
      ListRefreshSchedulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRefreshSchedulesResponse'
            Prelude.<$> ( x
                            Data..?> "RefreshSchedules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRefreshSchedules where
  hashWithSalt _salt ListRefreshSchedules' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData ListRefreshSchedules where
  rnf ListRefreshSchedules' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders ListRefreshSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRefreshSchedules where
  toPath ListRefreshSchedules' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-schedules"
      ]

instance Data.ToQuery ListRefreshSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRefreshSchedulesResponse' smart constructor.
data ListRefreshSchedulesResponse = ListRefreshSchedulesResponse'
  { -- | The list of refresh schedules for the dataset.
    refreshSchedules :: Prelude.Maybe [RefreshSchedule],
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRefreshSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshSchedules', 'listRefreshSchedulesResponse_refreshSchedules' - The list of refresh schedules for the dataset.
--
-- 'requestId', 'listRefreshSchedulesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listRefreshSchedulesResponse_status' - The HTTP status of the request.
newListRefreshSchedulesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListRefreshSchedulesResponse
newListRefreshSchedulesResponse pStatus_ =
  ListRefreshSchedulesResponse'
    { refreshSchedules =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The list of refresh schedules for the dataset.
listRefreshSchedulesResponse_refreshSchedules :: Lens.Lens' ListRefreshSchedulesResponse (Prelude.Maybe [RefreshSchedule])
listRefreshSchedulesResponse_refreshSchedules = Lens.lens (\ListRefreshSchedulesResponse' {refreshSchedules} -> refreshSchedules) (\s@ListRefreshSchedulesResponse' {} a -> s {refreshSchedules = a} :: ListRefreshSchedulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
listRefreshSchedulesResponse_requestId :: Lens.Lens' ListRefreshSchedulesResponse (Prelude.Maybe Prelude.Text)
listRefreshSchedulesResponse_requestId = Lens.lens (\ListRefreshSchedulesResponse' {requestId} -> requestId) (\s@ListRefreshSchedulesResponse' {} a -> s {requestId = a} :: ListRefreshSchedulesResponse)

-- | The HTTP status of the request.
listRefreshSchedulesResponse_status :: Lens.Lens' ListRefreshSchedulesResponse Prelude.Int
listRefreshSchedulesResponse_status = Lens.lens (\ListRefreshSchedulesResponse' {status} -> status) (\s@ListRefreshSchedulesResponse' {} a -> s {status = a} :: ListRefreshSchedulesResponse)

instance Prelude.NFData ListRefreshSchedulesResponse where
  rnf ListRefreshSchedulesResponse' {..} =
    Prelude.rnf refreshSchedules
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

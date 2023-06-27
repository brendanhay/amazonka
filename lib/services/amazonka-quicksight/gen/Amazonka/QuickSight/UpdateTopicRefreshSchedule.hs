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
-- Module      : Amazonka.QuickSight.UpdateTopicRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a topic refresh schedule.
module Amazonka.QuickSight.UpdateTopicRefreshSchedule
  ( -- * Creating a Request
    UpdateTopicRefreshSchedule (..),
    newUpdateTopicRefreshSchedule,

    -- * Request Lenses
    updateTopicRefreshSchedule_awsAccountId,
    updateTopicRefreshSchedule_topicId,
    updateTopicRefreshSchedule_datasetId,
    updateTopicRefreshSchedule_refreshSchedule,

    -- * Destructuring the Response
    UpdateTopicRefreshScheduleResponse (..),
    newUpdateTopicRefreshScheduleResponse,

    -- * Response Lenses
    updateTopicRefreshScheduleResponse_datasetArn,
    updateTopicRefreshScheduleResponse_requestId,
    updateTopicRefreshScheduleResponse_topicArn,
    updateTopicRefreshScheduleResponse_topicId,
    updateTopicRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTopicRefreshSchedule' smart constructor.
data UpdateTopicRefreshSchedule = UpdateTopicRefreshSchedule'
  { -- | The ID of the Amazon Web Services account that contains the topic whose
    -- refresh schedule you want to update.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The ID of the dataset.
    datasetId :: Prelude.Text,
    -- | The definition of a refresh schedule.
    refreshSchedule :: TopicRefreshSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'updateTopicRefreshSchedule_awsAccountId' - The ID of the Amazon Web Services account that contains the topic whose
-- refresh schedule you want to update.
--
-- 'topicId', 'updateTopicRefreshSchedule_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'datasetId', 'updateTopicRefreshSchedule_datasetId' - The ID of the dataset.
--
-- 'refreshSchedule', 'updateTopicRefreshSchedule_refreshSchedule' - The definition of a refresh schedule.
newUpdateTopicRefreshSchedule ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'refreshSchedule'
  TopicRefreshSchedule ->
  UpdateTopicRefreshSchedule
newUpdateTopicRefreshSchedule
  pAwsAccountId_
  pTopicId_
  pDatasetId_
  pRefreshSchedule_ =
    UpdateTopicRefreshSchedule'
      { awsAccountId =
          pAwsAccountId_,
        topicId = pTopicId_,
        datasetId = pDatasetId_,
        refreshSchedule = pRefreshSchedule_
      }

-- | The ID of the Amazon Web Services account that contains the topic whose
-- refresh schedule you want to update.
updateTopicRefreshSchedule_awsAccountId :: Lens.Lens' UpdateTopicRefreshSchedule Prelude.Text
updateTopicRefreshSchedule_awsAccountId = Lens.lens (\UpdateTopicRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@UpdateTopicRefreshSchedule' {} a -> s {awsAccountId = a} :: UpdateTopicRefreshSchedule)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopicRefreshSchedule_topicId :: Lens.Lens' UpdateTopicRefreshSchedule Prelude.Text
updateTopicRefreshSchedule_topicId = Lens.lens (\UpdateTopicRefreshSchedule' {topicId} -> topicId) (\s@UpdateTopicRefreshSchedule' {} a -> s {topicId = a} :: UpdateTopicRefreshSchedule)

-- | The ID of the dataset.
updateTopicRefreshSchedule_datasetId :: Lens.Lens' UpdateTopicRefreshSchedule Prelude.Text
updateTopicRefreshSchedule_datasetId = Lens.lens (\UpdateTopicRefreshSchedule' {datasetId} -> datasetId) (\s@UpdateTopicRefreshSchedule' {} a -> s {datasetId = a} :: UpdateTopicRefreshSchedule)

-- | The definition of a refresh schedule.
updateTopicRefreshSchedule_refreshSchedule :: Lens.Lens' UpdateTopicRefreshSchedule TopicRefreshSchedule
updateTopicRefreshSchedule_refreshSchedule = Lens.lens (\UpdateTopicRefreshSchedule' {refreshSchedule} -> refreshSchedule) (\s@UpdateTopicRefreshSchedule' {} a -> s {refreshSchedule = a} :: UpdateTopicRefreshSchedule)

instance Core.AWSRequest UpdateTopicRefreshSchedule where
  type
    AWSResponse UpdateTopicRefreshSchedule =
      UpdateTopicRefreshScheduleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTopicRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTopicRefreshSchedule where
  hashWithSalt _salt UpdateTopicRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` refreshSchedule

instance Prelude.NFData UpdateTopicRefreshSchedule where
  rnf UpdateTopicRefreshSchedule' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf refreshSchedule

instance Data.ToHeaders UpdateTopicRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTopicRefreshSchedule where
  toJSON UpdateTopicRefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RefreshSchedule" Data..= refreshSchedule)
          ]
      )

instance Data.ToPath UpdateTopicRefreshSchedule where
  toPath UpdateTopicRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/schedules/",
        Data.toBS datasetId
      ]

instance Data.ToQuery UpdateTopicRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTopicRefreshScheduleResponse' smart constructor.
data UpdateTopicRefreshScheduleResponse = UpdateTopicRefreshScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'updateTopicRefreshScheduleResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'requestId', 'updateTopicRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'updateTopicRefreshScheduleResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'updateTopicRefreshScheduleResponse_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'updateTopicRefreshScheduleResponse_status' - The HTTP status of the request.
newUpdateTopicRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTopicRefreshScheduleResponse
newUpdateTopicRefreshScheduleResponse pStatus_ =
  UpdateTopicRefreshScheduleResponse'
    { datasetArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
updateTopicRefreshScheduleResponse_datasetArn :: Lens.Lens' UpdateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateTopicRefreshScheduleResponse_datasetArn = Lens.lens (\UpdateTopicRefreshScheduleResponse' {datasetArn} -> datasetArn) (\s@UpdateTopicRefreshScheduleResponse' {} a -> s {datasetArn = a} :: UpdateTopicRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
updateTopicRefreshScheduleResponse_requestId :: Lens.Lens' UpdateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateTopicRefreshScheduleResponse_requestId = Lens.lens (\UpdateTopicRefreshScheduleResponse' {requestId} -> requestId) (\s@UpdateTopicRefreshScheduleResponse' {} a -> s {requestId = a} :: UpdateTopicRefreshScheduleResponse)

-- | The Amazon Resource Name (ARN) of the topic.
updateTopicRefreshScheduleResponse_topicArn :: Lens.Lens' UpdateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateTopicRefreshScheduleResponse_topicArn = Lens.lens (\UpdateTopicRefreshScheduleResponse' {topicArn} -> topicArn) (\s@UpdateTopicRefreshScheduleResponse' {} a -> s {topicArn = a} :: UpdateTopicRefreshScheduleResponse)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopicRefreshScheduleResponse_topicId :: Lens.Lens' UpdateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateTopicRefreshScheduleResponse_topicId = Lens.lens (\UpdateTopicRefreshScheduleResponse' {topicId} -> topicId) (\s@UpdateTopicRefreshScheduleResponse' {} a -> s {topicId = a} :: UpdateTopicRefreshScheduleResponse)

-- | The HTTP status of the request.
updateTopicRefreshScheduleResponse_status :: Lens.Lens' UpdateTopicRefreshScheduleResponse Prelude.Int
updateTopicRefreshScheduleResponse_status = Lens.lens (\UpdateTopicRefreshScheduleResponse' {status} -> status) (\s@UpdateTopicRefreshScheduleResponse' {} a -> s {status = a} :: UpdateTopicRefreshScheduleResponse)

instance
  Prelude.NFData
    UpdateTopicRefreshScheduleResponse
  where
  rnf UpdateTopicRefreshScheduleResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

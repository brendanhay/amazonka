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
-- Module      : Amazonka.QuickSight.DeleteTopicRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic refresh schedule.
module Amazonka.QuickSight.DeleteTopicRefreshSchedule
  ( -- * Creating a Request
    DeleteTopicRefreshSchedule (..),
    newDeleteTopicRefreshSchedule,

    -- * Request Lenses
    deleteTopicRefreshSchedule_awsAccountId,
    deleteTopicRefreshSchedule_topicId,
    deleteTopicRefreshSchedule_datasetId,

    -- * Destructuring the Response
    DeleteTopicRefreshScheduleResponse (..),
    newDeleteTopicRefreshScheduleResponse,

    -- * Response Lenses
    deleteTopicRefreshScheduleResponse_datasetArn,
    deleteTopicRefreshScheduleResponse_requestId,
    deleteTopicRefreshScheduleResponse_topicArn,
    deleteTopicRefreshScheduleResponse_topicId,
    deleteTopicRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTopicRefreshSchedule' smart constructor.
data DeleteTopicRefreshSchedule = DeleteTopicRefreshSchedule'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The ID of the dataset.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteTopicRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'topicId', 'deleteTopicRefreshSchedule_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'datasetId', 'deleteTopicRefreshSchedule_datasetId' - The ID of the dataset.
newDeleteTopicRefreshSchedule ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'datasetId'
  Prelude.Text ->
  DeleteTopicRefreshSchedule
newDeleteTopicRefreshSchedule
  pAwsAccountId_
  pTopicId_
  pDatasetId_ =
    DeleteTopicRefreshSchedule'
      { awsAccountId =
          pAwsAccountId_,
        topicId = pTopicId_,
        datasetId = pDatasetId_
      }

-- | The Amazon Web Services account ID.
deleteTopicRefreshSchedule_awsAccountId :: Lens.Lens' DeleteTopicRefreshSchedule Prelude.Text
deleteTopicRefreshSchedule_awsAccountId = Lens.lens (\DeleteTopicRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@DeleteTopicRefreshSchedule' {} a -> s {awsAccountId = a} :: DeleteTopicRefreshSchedule)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
deleteTopicRefreshSchedule_topicId :: Lens.Lens' DeleteTopicRefreshSchedule Prelude.Text
deleteTopicRefreshSchedule_topicId = Lens.lens (\DeleteTopicRefreshSchedule' {topicId} -> topicId) (\s@DeleteTopicRefreshSchedule' {} a -> s {topicId = a} :: DeleteTopicRefreshSchedule)

-- | The ID of the dataset.
deleteTopicRefreshSchedule_datasetId :: Lens.Lens' DeleteTopicRefreshSchedule Prelude.Text
deleteTopicRefreshSchedule_datasetId = Lens.lens (\DeleteTopicRefreshSchedule' {datasetId} -> datasetId) (\s@DeleteTopicRefreshSchedule' {} a -> s {datasetId = a} :: DeleteTopicRefreshSchedule)

instance Core.AWSRequest DeleteTopicRefreshSchedule where
  type
    AWSResponse DeleteTopicRefreshSchedule =
      DeleteTopicRefreshScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTopicRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTopicRefreshSchedule where
  hashWithSalt _salt DeleteTopicRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData DeleteTopicRefreshSchedule where
  rnf DeleteTopicRefreshSchedule' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders DeleteTopicRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTopicRefreshSchedule where
  toPath DeleteTopicRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/schedules/",
        Data.toBS datasetId
      ]

instance Data.ToQuery DeleteTopicRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTopicRefreshScheduleResponse' smart constructor.
data DeleteTopicRefreshScheduleResponse = DeleteTopicRefreshScheduleResponse'
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
-- Create a value of 'DeleteTopicRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'deleteTopicRefreshScheduleResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'requestId', 'deleteTopicRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'deleteTopicRefreshScheduleResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'deleteTopicRefreshScheduleResponse_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'deleteTopicRefreshScheduleResponse_status' - The HTTP status of the request.
newDeleteTopicRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteTopicRefreshScheduleResponse
newDeleteTopicRefreshScheduleResponse pStatus_ =
  DeleteTopicRefreshScheduleResponse'
    { datasetArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
deleteTopicRefreshScheduleResponse_datasetArn :: Lens.Lens' DeleteTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteTopicRefreshScheduleResponse_datasetArn = Lens.lens (\DeleteTopicRefreshScheduleResponse' {datasetArn} -> datasetArn) (\s@DeleteTopicRefreshScheduleResponse' {} a -> s {datasetArn = a} :: DeleteTopicRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
deleteTopicRefreshScheduleResponse_requestId :: Lens.Lens' DeleteTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteTopicRefreshScheduleResponse_requestId = Lens.lens (\DeleteTopicRefreshScheduleResponse' {requestId} -> requestId) (\s@DeleteTopicRefreshScheduleResponse' {} a -> s {requestId = a} :: DeleteTopicRefreshScheduleResponse)

-- | The Amazon Resource Name (ARN) of the topic.
deleteTopicRefreshScheduleResponse_topicArn :: Lens.Lens' DeleteTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteTopicRefreshScheduleResponse_topicArn = Lens.lens (\DeleteTopicRefreshScheduleResponse' {topicArn} -> topicArn) (\s@DeleteTopicRefreshScheduleResponse' {} a -> s {topicArn = a} :: DeleteTopicRefreshScheduleResponse)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
deleteTopicRefreshScheduleResponse_topicId :: Lens.Lens' DeleteTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteTopicRefreshScheduleResponse_topicId = Lens.lens (\DeleteTopicRefreshScheduleResponse' {topicId} -> topicId) (\s@DeleteTopicRefreshScheduleResponse' {} a -> s {topicId = a} :: DeleteTopicRefreshScheduleResponse)

-- | The HTTP status of the request.
deleteTopicRefreshScheduleResponse_status :: Lens.Lens' DeleteTopicRefreshScheduleResponse Prelude.Int
deleteTopicRefreshScheduleResponse_status = Lens.lens (\DeleteTopicRefreshScheduleResponse' {status} -> status) (\s@DeleteTopicRefreshScheduleResponse' {} a -> s {status = a} :: DeleteTopicRefreshScheduleResponse)

instance
  Prelude.NFData
    DeleteTopicRefreshScheduleResponse
  where
  rnf DeleteTopicRefreshScheduleResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

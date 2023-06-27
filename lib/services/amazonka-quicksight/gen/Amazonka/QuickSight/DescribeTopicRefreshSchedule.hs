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
-- Module      : Amazonka.QuickSight.DescribeTopicRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic refresh schedule.
module Amazonka.QuickSight.DescribeTopicRefreshSchedule
  ( -- * Creating a Request
    DescribeTopicRefreshSchedule (..),
    newDescribeTopicRefreshSchedule,

    -- * Request Lenses
    describeTopicRefreshSchedule_awsAccountId,
    describeTopicRefreshSchedule_topicId,
    describeTopicRefreshSchedule_datasetId,

    -- * Destructuring the Response
    DescribeTopicRefreshScheduleResponse (..),
    newDescribeTopicRefreshScheduleResponse,

    -- * Response Lenses
    describeTopicRefreshScheduleResponse_datasetArn,
    describeTopicRefreshScheduleResponse_refreshSchedule,
    describeTopicRefreshScheduleResponse_requestId,
    describeTopicRefreshScheduleResponse_topicArn,
    describeTopicRefreshScheduleResponse_topicId,
    describeTopicRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTopicRefreshSchedule' smart constructor.
data DescribeTopicRefreshSchedule = DescribeTopicRefreshSchedule'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that contains the refresh schedule that you want to
    -- describe. This ID is unique per Amazon Web Services Region for each
    -- Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The ID of the dataset.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTopicRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'topicId', 'describeTopicRefreshSchedule_topicId' - The ID of the topic that contains the refresh schedule that you want to
-- describe. This ID is unique per Amazon Web Services Region for each
-- Amazon Web Services account.
--
-- 'datasetId', 'describeTopicRefreshSchedule_datasetId' - The ID of the dataset.
newDescribeTopicRefreshSchedule ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'datasetId'
  Prelude.Text ->
  DescribeTopicRefreshSchedule
newDescribeTopicRefreshSchedule
  pAwsAccountId_
  pTopicId_
  pDatasetId_ =
    DescribeTopicRefreshSchedule'
      { awsAccountId =
          pAwsAccountId_,
        topicId = pTopicId_,
        datasetId = pDatasetId_
      }

-- | The Amazon Web Services account ID.
describeTopicRefreshSchedule_awsAccountId :: Lens.Lens' DescribeTopicRefreshSchedule Prelude.Text
describeTopicRefreshSchedule_awsAccountId = Lens.lens (\DescribeTopicRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@DescribeTopicRefreshSchedule' {} a -> s {awsAccountId = a} :: DescribeTopicRefreshSchedule)

-- | The ID of the topic that contains the refresh schedule that you want to
-- describe. This ID is unique per Amazon Web Services Region for each
-- Amazon Web Services account.
describeTopicRefreshSchedule_topicId :: Lens.Lens' DescribeTopicRefreshSchedule Prelude.Text
describeTopicRefreshSchedule_topicId = Lens.lens (\DescribeTopicRefreshSchedule' {topicId} -> topicId) (\s@DescribeTopicRefreshSchedule' {} a -> s {topicId = a} :: DescribeTopicRefreshSchedule)

-- | The ID of the dataset.
describeTopicRefreshSchedule_datasetId :: Lens.Lens' DescribeTopicRefreshSchedule Prelude.Text
describeTopicRefreshSchedule_datasetId = Lens.lens (\DescribeTopicRefreshSchedule' {datasetId} -> datasetId) (\s@DescribeTopicRefreshSchedule' {} a -> s {datasetId = a} :: DescribeTopicRefreshSchedule)

instance Core.AWSRequest DescribeTopicRefreshSchedule where
  type
    AWSResponse DescribeTopicRefreshSchedule =
      DescribeTopicRefreshScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "RefreshSchedule")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTopicRefreshSchedule
  where
  hashWithSalt _salt DescribeTopicRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData DescribeTopicRefreshSchedule where
  rnf DescribeTopicRefreshSchedule' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders DescribeTopicRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTopicRefreshSchedule where
  toPath DescribeTopicRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/schedules/",
        Data.toBS datasetId
      ]

instance Data.ToQuery DescribeTopicRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTopicRefreshScheduleResponse' smart constructor.
data DescribeTopicRefreshScheduleResponse = DescribeTopicRefreshScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The definition of a refresh schedule.
    refreshSchedule :: Prelude.Maybe TopicRefreshSchedule,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that contains the refresh schedule that you want to
    -- describe. This ID is unique per Amazon Web Services Region for each
    -- Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'describeTopicRefreshScheduleResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'refreshSchedule', 'describeTopicRefreshScheduleResponse_refreshSchedule' - The definition of a refresh schedule.
--
-- 'requestId', 'describeTopicRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'describeTopicRefreshScheduleResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'describeTopicRefreshScheduleResponse_topicId' - The ID of the topic that contains the refresh schedule that you want to
-- describe. This ID is unique per Amazon Web Services Region for each
-- Amazon Web Services account.
--
-- 'status', 'describeTopicRefreshScheduleResponse_status' - The HTTP status of the request.
newDescribeTopicRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTopicRefreshScheduleResponse
newDescribeTopicRefreshScheduleResponse pStatus_ =
  DescribeTopicRefreshScheduleResponse'
    { datasetArn =
        Prelude.Nothing,
      refreshSchedule = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
describeTopicRefreshScheduleResponse_datasetArn :: Lens.Lens' DescribeTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeTopicRefreshScheduleResponse_datasetArn = Lens.lens (\DescribeTopicRefreshScheduleResponse' {datasetArn} -> datasetArn) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {datasetArn = a} :: DescribeTopicRefreshScheduleResponse)

-- | The definition of a refresh schedule.
describeTopicRefreshScheduleResponse_refreshSchedule :: Lens.Lens' DescribeTopicRefreshScheduleResponse (Prelude.Maybe TopicRefreshSchedule)
describeTopicRefreshScheduleResponse_refreshSchedule = Lens.lens (\DescribeTopicRefreshScheduleResponse' {refreshSchedule} -> refreshSchedule) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {refreshSchedule = a} :: DescribeTopicRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
describeTopicRefreshScheduleResponse_requestId :: Lens.Lens' DescribeTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeTopicRefreshScheduleResponse_requestId = Lens.lens (\DescribeTopicRefreshScheduleResponse' {requestId} -> requestId) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {requestId = a} :: DescribeTopicRefreshScheduleResponse)

-- | The Amazon Resource Name (ARN) of the topic.
describeTopicRefreshScheduleResponse_topicArn :: Lens.Lens' DescribeTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeTopicRefreshScheduleResponse_topicArn = Lens.lens (\DescribeTopicRefreshScheduleResponse' {topicArn} -> topicArn) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {topicArn = a} :: DescribeTopicRefreshScheduleResponse)

-- | The ID of the topic that contains the refresh schedule that you want to
-- describe. This ID is unique per Amazon Web Services Region for each
-- Amazon Web Services account.
describeTopicRefreshScheduleResponse_topicId :: Lens.Lens' DescribeTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeTopicRefreshScheduleResponse_topicId = Lens.lens (\DescribeTopicRefreshScheduleResponse' {topicId} -> topicId) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {topicId = a} :: DescribeTopicRefreshScheduleResponse)

-- | The HTTP status of the request.
describeTopicRefreshScheduleResponse_status :: Lens.Lens' DescribeTopicRefreshScheduleResponse Prelude.Int
describeTopicRefreshScheduleResponse_status = Lens.lens (\DescribeTopicRefreshScheduleResponse' {status} -> status) (\s@DescribeTopicRefreshScheduleResponse' {} a -> s {status = a} :: DescribeTopicRefreshScheduleResponse)

instance
  Prelude.NFData
    DescribeTopicRefreshScheduleResponse
  where
  rnf DescribeTopicRefreshScheduleResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf refreshSchedule
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

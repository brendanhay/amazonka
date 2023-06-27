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
-- Module      : Amazonka.QuickSight.CreateTopicRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic refresh schedule.
module Amazonka.QuickSight.CreateTopicRefreshSchedule
  ( -- * Creating a Request
    CreateTopicRefreshSchedule (..),
    newCreateTopicRefreshSchedule,

    -- * Request Lenses
    createTopicRefreshSchedule_datasetName,
    createTopicRefreshSchedule_awsAccountId,
    createTopicRefreshSchedule_topicId,
    createTopicRefreshSchedule_datasetArn,
    createTopicRefreshSchedule_refreshSchedule,

    -- * Destructuring the Response
    CreateTopicRefreshScheduleResponse (..),
    newCreateTopicRefreshScheduleResponse,

    -- * Response Lenses
    createTopicRefreshScheduleResponse_datasetArn,
    createTopicRefreshScheduleResponse_requestId,
    createTopicRefreshScheduleResponse_topicArn,
    createTopicRefreshScheduleResponse_topicId,
    createTopicRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTopicRefreshSchedule' smart constructor.
data CreateTopicRefreshSchedule = CreateTopicRefreshSchedule'
  { -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the topic
    -- you\'re creating a refresh schedule for.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Text,
    -- | The definition of a refresh schedule.
    refreshSchedule :: TopicRefreshSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetName', 'createTopicRefreshSchedule_datasetName' - The name of the dataset.
--
-- 'awsAccountId', 'createTopicRefreshSchedule_awsAccountId' - The ID of the Amazon Web Services account that contains the topic
-- you\'re creating a refresh schedule for.
--
-- 'topicId', 'createTopicRefreshSchedule_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'datasetArn', 'createTopicRefreshSchedule_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'refreshSchedule', 'createTopicRefreshSchedule_refreshSchedule' - The definition of a refresh schedule.
newCreateTopicRefreshSchedule ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'refreshSchedule'
  TopicRefreshSchedule ->
  CreateTopicRefreshSchedule
newCreateTopicRefreshSchedule
  pAwsAccountId_
  pTopicId_
  pDatasetArn_
  pRefreshSchedule_ =
    CreateTopicRefreshSchedule'
      { datasetName =
          Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        topicId = pTopicId_,
        datasetArn = pDatasetArn_,
        refreshSchedule = pRefreshSchedule_
      }

-- | The name of the dataset.
createTopicRefreshSchedule_datasetName :: Lens.Lens' CreateTopicRefreshSchedule (Prelude.Maybe Prelude.Text)
createTopicRefreshSchedule_datasetName = Lens.lens (\CreateTopicRefreshSchedule' {datasetName} -> datasetName) (\s@CreateTopicRefreshSchedule' {} a -> s {datasetName = a} :: CreateTopicRefreshSchedule)

-- | The ID of the Amazon Web Services account that contains the topic
-- you\'re creating a refresh schedule for.
createTopicRefreshSchedule_awsAccountId :: Lens.Lens' CreateTopicRefreshSchedule Prelude.Text
createTopicRefreshSchedule_awsAccountId = Lens.lens (\CreateTopicRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@CreateTopicRefreshSchedule' {} a -> s {awsAccountId = a} :: CreateTopicRefreshSchedule)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
createTopicRefreshSchedule_topicId :: Lens.Lens' CreateTopicRefreshSchedule Prelude.Text
createTopicRefreshSchedule_topicId = Lens.lens (\CreateTopicRefreshSchedule' {topicId} -> topicId) (\s@CreateTopicRefreshSchedule' {} a -> s {topicId = a} :: CreateTopicRefreshSchedule)

-- | The Amazon Resource Name (ARN) of the dataset.
createTopicRefreshSchedule_datasetArn :: Lens.Lens' CreateTopicRefreshSchedule Prelude.Text
createTopicRefreshSchedule_datasetArn = Lens.lens (\CreateTopicRefreshSchedule' {datasetArn} -> datasetArn) (\s@CreateTopicRefreshSchedule' {} a -> s {datasetArn = a} :: CreateTopicRefreshSchedule)

-- | The definition of a refresh schedule.
createTopicRefreshSchedule_refreshSchedule :: Lens.Lens' CreateTopicRefreshSchedule TopicRefreshSchedule
createTopicRefreshSchedule_refreshSchedule = Lens.lens (\CreateTopicRefreshSchedule' {refreshSchedule} -> refreshSchedule) (\s@CreateTopicRefreshSchedule' {} a -> s {refreshSchedule = a} :: CreateTopicRefreshSchedule)

instance Core.AWSRequest CreateTopicRefreshSchedule where
  type
    AWSResponse CreateTopicRefreshSchedule =
      CreateTopicRefreshScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTopicRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "DatasetArn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTopicRefreshSchedule where
  hashWithSalt _salt CreateTopicRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` refreshSchedule

instance Prelude.NFData CreateTopicRefreshSchedule where
  rnf CreateTopicRefreshSchedule' {..} =
    Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf refreshSchedule

instance Data.ToHeaders CreateTopicRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTopicRefreshSchedule where
  toJSON CreateTopicRefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatasetName" Data..=) Prelude.<$> datasetName,
            Prelude.Just ("DatasetArn" Data..= datasetArn),
            Prelude.Just
              ("RefreshSchedule" Data..= refreshSchedule)
          ]
      )

instance Data.ToPath CreateTopicRefreshSchedule where
  toPath CreateTopicRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/schedules"
      ]

instance Data.ToQuery CreateTopicRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTopicRefreshScheduleResponse' smart constructor.
data CreateTopicRefreshScheduleResponse = CreateTopicRefreshScheduleResponse'
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
-- Create a value of 'CreateTopicRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'createTopicRefreshScheduleResponse_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'requestId', 'createTopicRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'createTopicRefreshScheduleResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'createTopicRefreshScheduleResponse_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'createTopicRefreshScheduleResponse_status' - The HTTP status of the request.
newCreateTopicRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateTopicRefreshScheduleResponse
newCreateTopicRefreshScheduleResponse pStatus_ =
  CreateTopicRefreshScheduleResponse'
    { datasetArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the dataset.
createTopicRefreshScheduleResponse_datasetArn :: Lens.Lens' CreateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createTopicRefreshScheduleResponse_datasetArn = Lens.lens (\CreateTopicRefreshScheduleResponse' {datasetArn} -> datasetArn) (\s@CreateTopicRefreshScheduleResponse' {} a -> s {datasetArn = a} :: CreateTopicRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
createTopicRefreshScheduleResponse_requestId :: Lens.Lens' CreateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createTopicRefreshScheduleResponse_requestId = Lens.lens (\CreateTopicRefreshScheduleResponse' {requestId} -> requestId) (\s@CreateTopicRefreshScheduleResponse' {} a -> s {requestId = a} :: CreateTopicRefreshScheduleResponse)

-- | The Amazon Resource Name (ARN) of the topic.
createTopicRefreshScheduleResponse_topicArn :: Lens.Lens' CreateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createTopicRefreshScheduleResponse_topicArn = Lens.lens (\CreateTopicRefreshScheduleResponse' {topicArn} -> topicArn) (\s@CreateTopicRefreshScheduleResponse' {} a -> s {topicArn = a} :: CreateTopicRefreshScheduleResponse)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
createTopicRefreshScheduleResponse_topicId :: Lens.Lens' CreateTopicRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createTopicRefreshScheduleResponse_topicId = Lens.lens (\CreateTopicRefreshScheduleResponse' {topicId} -> topicId) (\s@CreateTopicRefreshScheduleResponse' {} a -> s {topicId = a} :: CreateTopicRefreshScheduleResponse)

-- | The HTTP status of the request.
createTopicRefreshScheduleResponse_status :: Lens.Lens' CreateTopicRefreshScheduleResponse Prelude.Int
createTopicRefreshScheduleResponse_status = Lens.lens (\CreateTopicRefreshScheduleResponse' {status} -> status) (\s@CreateTopicRefreshScheduleResponse' {} a -> s {status = a} :: CreateTopicRefreshScheduleResponse)

instance
  Prelude.NFData
    CreateTopicRefreshScheduleResponse
  where
  rnf CreateTopicRefreshScheduleResponse' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

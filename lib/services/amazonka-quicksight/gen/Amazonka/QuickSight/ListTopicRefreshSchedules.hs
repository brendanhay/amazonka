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
-- Module      : Amazonka.QuickSight.ListTopicRefreshSchedules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the refresh schedules for a topic.
module Amazonka.QuickSight.ListTopicRefreshSchedules
  ( -- * Creating a Request
    ListTopicRefreshSchedules (..),
    newListTopicRefreshSchedules,

    -- * Request Lenses
    listTopicRefreshSchedules_awsAccountId,
    listTopicRefreshSchedules_topicId,

    -- * Destructuring the Response
    ListTopicRefreshSchedulesResponse (..),
    newListTopicRefreshSchedulesResponse,

    -- * Response Lenses
    listTopicRefreshSchedulesResponse_refreshSchedules,
    listTopicRefreshSchedulesResponse_requestId,
    listTopicRefreshSchedulesResponse_topicArn,
    listTopicRefreshSchedulesResponse_topicId,
    listTopicRefreshSchedulesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTopicRefreshSchedules' smart constructor.
data ListTopicRefreshSchedules = ListTopicRefreshSchedules'
  { -- | The ID of the Amazon Web Services account that contains the topic whose
    -- refresh schedule you want described.
    awsAccountId :: Prelude.Text,
    -- | The ID for the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRefreshSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'listTopicRefreshSchedules_awsAccountId' - The ID of the Amazon Web Services account that contains the topic whose
-- refresh schedule you want described.
--
-- 'topicId', 'listTopicRefreshSchedules_topicId' - The ID for the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newListTopicRefreshSchedules ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  ListTopicRefreshSchedules
newListTopicRefreshSchedules pAwsAccountId_ pTopicId_ =
  ListTopicRefreshSchedules'
    { awsAccountId =
        pAwsAccountId_,
      topicId = pTopicId_
    }

-- | The ID of the Amazon Web Services account that contains the topic whose
-- refresh schedule you want described.
listTopicRefreshSchedules_awsAccountId :: Lens.Lens' ListTopicRefreshSchedules Prelude.Text
listTopicRefreshSchedules_awsAccountId = Lens.lens (\ListTopicRefreshSchedules' {awsAccountId} -> awsAccountId) (\s@ListTopicRefreshSchedules' {} a -> s {awsAccountId = a} :: ListTopicRefreshSchedules)

-- | The ID for the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
listTopicRefreshSchedules_topicId :: Lens.Lens' ListTopicRefreshSchedules Prelude.Text
listTopicRefreshSchedules_topicId = Lens.lens (\ListTopicRefreshSchedules' {topicId} -> topicId) (\s@ListTopicRefreshSchedules' {} a -> s {topicId = a} :: ListTopicRefreshSchedules)

instance Core.AWSRequest ListTopicRefreshSchedules where
  type
    AWSResponse ListTopicRefreshSchedules =
      ListTopicRefreshSchedulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRefreshSchedulesResponse'
            Prelude.<$> ( x
                            Data..?> "RefreshSchedules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopicRefreshSchedules where
  hashWithSalt _salt ListTopicRefreshSchedules' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData ListTopicRefreshSchedules where
  rnf ListTopicRefreshSchedules' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId

instance Data.ToHeaders ListTopicRefreshSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTopicRefreshSchedules where
  toPath ListTopicRefreshSchedules' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/schedules"
      ]

instance Data.ToQuery ListTopicRefreshSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTopicRefreshSchedulesResponse' smart constructor.
data ListTopicRefreshSchedulesResponse = ListTopicRefreshSchedulesResponse'
  { -- | The list of topic refresh schedules.
    refreshSchedules :: Prelude.Maybe [TopicRefreshScheduleSummary],
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRefreshSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshSchedules', 'listTopicRefreshSchedulesResponse_refreshSchedules' - The list of topic refresh schedules.
--
-- 'requestId', 'listTopicRefreshSchedulesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'listTopicRefreshSchedulesResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'listTopicRefreshSchedulesResponse_topicId' - The ID for the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'listTopicRefreshSchedulesResponse_status' - The HTTP status of the request.
newListTopicRefreshSchedulesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListTopicRefreshSchedulesResponse
newListTopicRefreshSchedulesResponse pStatus_ =
  ListTopicRefreshSchedulesResponse'
    { refreshSchedules =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The list of topic refresh schedules.
listTopicRefreshSchedulesResponse_refreshSchedules :: Lens.Lens' ListTopicRefreshSchedulesResponse (Prelude.Maybe [TopicRefreshScheduleSummary])
listTopicRefreshSchedulesResponse_refreshSchedules = Lens.lens (\ListTopicRefreshSchedulesResponse' {refreshSchedules} -> refreshSchedules) (\s@ListTopicRefreshSchedulesResponse' {} a -> s {refreshSchedules = a} :: ListTopicRefreshSchedulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
listTopicRefreshSchedulesResponse_requestId :: Lens.Lens' ListTopicRefreshSchedulesResponse (Prelude.Maybe Prelude.Text)
listTopicRefreshSchedulesResponse_requestId = Lens.lens (\ListTopicRefreshSchedulesResponse' {requestId} -> requestId) (\s@ListTopicRefreshSchedulesResponse' {} a -> s {requestId = a} :: ListTopicRefreshSchedulesResponse)

-- | The Amazon Resource Name (ARN) of the topic.
listTopicRefreshSchedulesResponse_topicArn :: Lens.Lens' ListTopicRefreshSchedulesResponse (Prelude.Maybe Prelude.Text)
listTopicRefreshSchedulesResponse_topicArn = Lens.lens (\ListTopicRefreshSchedulesResponse' {topicArn} -> topicArn) (\s@ListTopicRefreshSchedulesResponse' {} a -> s {topicArn = a} :: ListTopicRefreshSchedulesResponse)

-- | The ID for the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
listTopicRefreshSchedulesResponse_topicId :: Lens.Lens' ListTopicRefreshSchedulesResponse (Prelude.Maybe Prelude.Text)
listTopicRefreshSchedulesResponse_topicId = Lens.lens (\ListTopicRefreshSchedulesResponse' {topicId} -> topicId) (\s@ListTopicRefreshSchedulesResponse' {} a -> s {topicId = a} :: ListTopicRefreshSchedulesResponse)

-- | The HTTP status of the request.
listTopicRefreshSchedulesResponse_status :: Lens.Lens' ListTopicRefreshSchedulesResponse Prelude.Int
listTopicRefreshSchedulesResponse_status = Lens.lens (\ListTopicRefreshSchedulesResponse' {status} -> status) (\s@ListTopicRefreshSchedulesResponse' {} a -> s {status = a} :: ListTopicRefreshSchedulesResponse)

instance
  Prelude.NFData
    ListTopicRefreshSchedulesResponse
  where
  rnf ListTopicRefreshSchedulesResponse' {..} =
    Prelude.rnf refreshSchedules
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

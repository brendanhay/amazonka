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
-- Module      : Amazonka.QuickSight.UpdateTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a topic.
module Amazonka.QuickSight.UpdateTopic
  ( -- * Creating a Request
    UpdateTopic (..),
    newUpdateTopic,

    -- * Request Lenses
    updateTopic_awsAccountId,
    updateTopic_topicId,
    updateTopic_topic,

    -- * Destructuring the Response
    UpdateTopicResponse (..),
    newUpdateTopicResponse,

    -- * Response Lenses
    updateTopicResponse_arn,
    updateTopicResponse_refreshArn,
    updateTopicResponse_requestId,
    updateTopicResponse_topicId,
    updateTopicResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTopic' smart constructor.
data UpdateTopic = UpdateTopic'
  { -- | The ID of the Amazon Web Services account that contains the topic that
    -- you want to update.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The definition of the topic that you want to update.
    topic :: TopicDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'updateTopic_awsAccountId' - The ID of the Amazon Web Services account that contains the topic that
-- you want to update.
--
-- 'topicId', 'updateTopic_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'topic', 'updateTopic_topic' - The definition of the topic that you want to update.
newUpdateTopic ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'topic'
  TopicDetails ->
  UpdateTopic
newUpdateTopic pAwsAccountId_ pTopicId_ pTopic_ =
  UpdateTopic'
    { awsAccountId = pAwsAccountId_,
      topicId = pTopicId_,
      topic = pTopic_
    }

-- | The ID of the Amazon Web Services account that contains the topic that
-- you want to update.
updateTopic_awsAccountId :: Lens.Lens' UpdateTopic Prelude.Text
updateTopic_awsAccountId = Lens.lens (\UpdateTopic' {awsAccountId} -> awsAccountId) (\s@UpdateTopic' {} a -> s {awsAccountId = a} :: UpdateTopic)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopic_topicId :: Lens.Lens' UpdateTopic Prelude.Text
updateTopic_topicId = Lens.lens (\UpdateTopic' {topicId} -> topicId) (\s@UpdateTopic' {} a -> s {topicId = a} :: UpdateTopic)

-- | The definition of the topic that you want to update.
updateTopic_topic :: Lens.Lens' UpdateTopic TopicDetails
updateTopic_topic = Lens.lens (\UpdateTopic' {topic} -> topic) (\s@UpdateTopic' {} a -> s {topic = a} :: UpdateTopic)

instance Core.AWSRequest UpdateTopic where
  type AWSResponse UpdateTopic = UpdateTopicResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTopicResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RefreshArn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTopic where
  hashWithSalt _salt UpdateTopic' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` topic

instance Prelude.NFData UpdateTopic where
  rnf UpdateTopic' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf topic

instance Data.ToHeaders UpdateTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTopic where
  toJSON UpdateTopic' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Topic" Data..= topic)]
      )

instance Data.ToPath UpdateTopic where
  toPath UpdateTopic' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId
      ]

instance Data.ToQuery UpdateTopic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTopicResponse' smart constructor.
data UpdateTopicResponse = UpdateTopicResponse'
  { -- | The Amazon Resource Name (ARN) of the topic.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic refresh.
    refreshArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateTopicResponse_arn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'refreshArn', 'updateTopicResponse_refreshArn' - The Amazon Resource Name (ARN) of the topic refresh.
--
-- 'requestId', 'updateTopicResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicId', 'updateTopicResponse_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'updateTopicResponse_status' - The HTTP status of the request.
newUpdateTopicResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTopicResponse
newUpdateTopicResponse pStatus_ =
  UpdateTopicResponse'
    { arn = Prelude.Nothing,
      refreshArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the topic.
updateTopicResponse_arn :: Lens.Lens' UpdateTopicResponse (Prelude.Maybe Prelude.Text)
updateTopicResponse_arn = Lens.lens (\UpdateTopicResponse' {arn} -> arn) (\s@UpdateTopicResponse' {} a -> s {arn = a} :: UpdateTopicResponse)

-- | The Amazon Resource Name (ARN) of the topic refresh.
updateTopicResponse_refreshArn :: Lens.Lens' UpdateTopicResponse (Prelude.Maybe Prelude.Text)
updateTopicResponse_refreshArn = Lens.lens (\UpdateTopicResponse' {refreshArn} -> refreshArn) (\s@UpdateTopicResponse' {} a -> s {refreshArn = a} :: UpdateTopicResponse)

-- | The Amazon Web Services request ID for this operation.
updateTopicResponse_requestId :: Lens.Lens' UpdateTopicResponse (Prelude.Maybe Prelude.Text)
updateTopicResponse_requestId = Lens.lens (\UpdateTopicResponse' {requestId} -> requestId) (\s@UpdateTopicResponse' {} a -> s {requestId = a} :: UpdateTopicResponse)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopicResponse_topicId :: Lens.Lens' UpdateTopicResponse (Prelude.Maybe Prelude.Text)
updateTopicResponse_topicId = Lens.lens (\UpdateTopicResponse' {topicId} -> topicId) (\s@UpdateTopicResponse' {} a -> s {topicId = a} :: UpdateTopicResponse)

-- | The HTTP status of the request.
updateTopicResponse_status :: Lens.Lens' UpdateTopicResponse Prelude.Int
updateTopicResponse_status = Lens.lens (\UpdateTopicResponse' {status} -> status) (\s@UpdateTopicResponse' {} a -> s {status = a} :: UpdateTopicResponse)

instance Prelude.NFData UpdateTopicResponse where
  rnf UpdateTopicResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf refreshArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

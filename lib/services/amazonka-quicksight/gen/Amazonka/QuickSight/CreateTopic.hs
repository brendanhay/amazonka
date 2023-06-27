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
-- Module      : Amazonka.QuickSight.CreateTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Q topic.
module Amazonka.QuickSight.CreateTopic
  ( -- * Creating a Request
    CreateTopic (..),
    newCreateTopic,

    -- * Request Lenses
    createTopic_tags,
    createTopic_awsAccountId,
    createTopic_topicId,
    createTopic_topic,

    -- * Destructuring the Response
    CreateTopicResponse (..),
    newCreateTopicResponse,

    -- * Response Lenses
    createTopicResponse_arn,
    createTopicResponse_refreshArn,
    createTopicResponse_requestId,
    createTopicResponse_topicId,
    createTopicResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTopic' smart constructor.
data CreateTopic = CreateTopic'
  { -- | Contains a map of the key-value pairs for the resource tag or tags that
    -- are assigned to the dataset.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the Amazon Web Services account that you want to create a
    -- topic in.
    awsAccountId :: Prelude.Text,
    -- | The ID for the topic that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The definition of a topic to create.
    topic :: TopicDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTopic_tags' - Contains a map of the key-value pairs for the resource tag or tags that
-- are assigned to the dataset.
--
-- 'awsAccountId', 'createTopic_awsAccountId' - The ID of the Amazon Web Services account that you want to create a
-- topic in.
--
-- 'topicId', 'createTopic_topicId' - The ID for the topic that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'topic', 'createTopic_topic' - The definition of a topic to create.
newCreateTopic ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'topic'
  TopicDetails ->
  CreateTopic
newCreateTopic pAwsAccountId_ pTopicId_ pTopic_ =
  CreateTopic'
    { tags = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      topicId = pTopicId_,
      topic = pTopic_
    }

-- | Contains a map of the key-value pairs for the resource tag or tags that
-- are assigned to the dataset.
createTopic_tags :: Lens.Lens' CreateTopic (Prelude.Maybe (Prelude.NonEmpty Tag))
createTopic_tags = Lens.lens (\CreateTopic' {tags} -> tags) (\s@CreateTopic' {} a -> s {tags = a} :: CreateTopic) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that you want to create a
-- topic in.
createTopic_awsAccountId :: Lens.Lens' CreateTopic Prelude.Text
createTopic_awsAccountId = Lens.lens (\CreateTopic' {awsAccountId} -> awsAccountId) (\s@CreateTopic' {} a -> s {awsAccountId = a} :: CreateTopic)

-- | The ID for the topic that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
createTopic_topicId :: Lens.Lens' CreateTopic Prelude.Text
createTopic_topicId = Lens.lens (\CreateTopic' {topicId} -> topicId) (\s@CreateTopic' {} a -> s {topicId = a} :: CreateTopic)

-- | The definition of a topic to create.
createTopic_topic :: Lens.Lens' CreateTopic TopicDetails
createTopic_topic = Lens.lens (\CreateTopic' {topic} -> topic) (\s@CreateTopic' {} a -> s {topic = a} :: CreateTopic)

instance Core.AWSRequest CreateTopic where
  type AWSResponse CreateTopic = CreateTopicResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTopicResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RefreshArn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTopic where
  hashWithSalt _salt CreateTopic' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` topic

instance Prelude.NFData CreateTopic where
  rnf CreateTopic' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf topic

instance Data.ToHeaders CreateTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTopic where
  toJSON CreateTopic' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("TopicId" Data..= topicId),
            Prelude.Just ("Topic" Data..= topic)
          ]
      )

instance Data.ToPath CreateTopic where
  toPath CreateTopic' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/topics"]

instance Data.ToQuery CreateTopic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { -- | The Amazon Resource Name (ARN) of the topic.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic refresh.
    refreshArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID for the topic that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createTopicResponse_arn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'refreshArn', 'createTopicResponse_refreshArn' - The Amazon Resource Name (ARN) of the topic refresh.
--
-- 'requestId', 'createTopicResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicId', 'createTopicResponse_topicId' - The ID for the topic that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'createTopicResponse_status' - The HTTP status of the request.
newCreateTopicResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateTopicResponse
newCreateTopicResponse pStatus_ =
  CreateTopicResponse'
    { arn = Prelude.Nothing,
      refreshArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the topic.
createTopicResponse_arn :: Lens.Lens' CreateTopicResponse (Prelude.Maybe Prelude.Text)
createTopicResponse_arn = Lens.lens (\CreateTopicResponse' {arn} -> arn) (\s@CreateTopicResponse' {} a -> s {arn = a} :: CreateTopicResponse)

-- | The Amazon Resource Name (ARN) of the topic refresh.
createTopicResponse_refreshArn :: Lens.Lens' CreateTopicResponse (Prelude.Maybe Prelude.Text)
createTopicResponse_refreshArn = Lens.lens (\CreateTopicResponse' {refreshArn} -> refreshArn) (\s@CreateTopicResponse' {} a -> s {refreshArn = a} :: CreateTopicResponse)

-- | The Amazon Web Services request ID for this operation.
createTopicResponse_requestId :: Lens.Lens' CreateTopicResponse (Prelude.Maybe Prelude.Text)
createTopicResponse_requestId = Lens.lens (\CreateTopicResponse' {requestId} -> requestId) (\s@CreateTopicResponse' {} a -> s {requestId = a} :: CreateTopicResponse)

-- | The ID for the topic that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
createTopicResponse_topicId :: Lens.Lens' CreateTopicResponse (Prelude.Maybe Prelude.Text)
createTopicResponse_topicId = Lens.lens (\CreateTopicResponse' {topicId} -> topicId) (\s@CreateTopicResponse' {} a -> s {topicId = a} :: CreateTopicResponse)

-- | The HTTP status of the request.
createTopicResponse_status :: Lens.Lens' CreateTopicResponse Prelude.Int
createTopicResponse_status = Lens.lens (\CreateTopicResponse' {status} -> status) (\s@CreateTopicResponse' {} a -> s {status = a} :: CreateTopicResponse)

instance Prelude.NFData CreateTopicResponse where
  rnf CreateTopicResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf refreshArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

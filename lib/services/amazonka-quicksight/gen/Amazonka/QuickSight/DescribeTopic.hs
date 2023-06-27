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
-- Module      : Amazonka.QuickSight.DescribeTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a topic.
module Amazonka.QuickSight.DescribeTopic
  ( -- * Creating a Request
    DescribeTopic (..),
    newDescribeTopic,

    -- * Request Lenses
    describeTopic_awsAccountId,
    describeTopic_topicId,

    -- * Destructuring the Response
    DescribeTopicResponse (..),
    newDescribeTopicResponse,

    -- * Response Lenses
    describeTopicResponse_arn,
    describeTopicResponse_requestId,
    describeTopicResponse_topic,
    describeTopicResponse_topicId,
    describeTopicResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTopic' smart constructor.
data DescribeTopic = DescribeTopic'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTopic_awsAccountId' - The Amazon Web Services account ID.
--
-- 'topicId', 'describeTopic_topicId' - The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newDescribeTopic ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  DescribeTopic
newDescribeTopic pAwsAccountId_ pTopicId_ =
  DescribeTopic'
    { awsAccountId = pAwsAccountId_,
      topicId = pTopicId_
    }

-- | The Amazon Web Services account ID.
describeTopic_awsAccountId :: Lens.Lens' DescribeTopic Prelude.Text
describeTopic_awsAccountId = Lens.lens (\DescribeTopic' {awsAccountId} -> awsAccountId) (\s@DescribeTopic' {} a -> s {awsAccountId = a} :: DescribeTopic)

-- | The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeTopic_topicId :: Lens.Lens' DescribeTopic Prelude.Text
describeTopic_topicId = Lens.lens (\DescribeTopic' {topicId} -> topicId) (\s@DescribeTopic' {} a -> s {topicId = a} :: DescribeTopic)

instance Core.AWSRequest DescribeTopic where
  type
    AWSResponse DescribeTopic =
      DescribeTopicResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Topic")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTopic where
  hashWithSalt _salt DescribeTopic' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData DescribeTopic where
  rnf DescribeTopic' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId

instance Data.ToHeaders DescribeTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTopic where
  toPath DescribeTopic' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId
      ]

instance Data.ToQuery DescribeTopic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTopicResponse' smart constructor.
data DescribeTopicResponse = DescribeTopicResponse'
  { -- | The Amazon Resource Name (ARN) of the topic.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The definition of a topic.
    topic :: Prelude.Maybe TopicDetails,
    -- | The ID of the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeTopicResponse_arn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'requestId', 'describeTopicResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topic', 'describeTopicResponse_topic' - The definition of a topic.
--
-- 'topicId', 'describeTopicResponse_topicId' - The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'describeTopicResponse_status' - The HTTP status of the request.
newDescribeTopicResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTopicResponse
newDescribeTopicResponse pStatus_ =
  DescribeTopicResponse'
    { arn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topic = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the topic.
describeTopicResponse_arn :: Lens.Lens' DescribeTopicResponse (Prelude.Maybe Prelude.Text)
describeTopicResponse_arn = Lens.lens (\DescribeTopicResponse' {arn} -> arn) (\s@DescribeTopicResponse' {} a -> s {arn = a} :: DescribeTopicResponse)

-- | The Amazon Web Services request ID for this operation.
describeTopicResponse_requestId :: Lens.Lens' DescribeTopicResponse (Prelude.Maybe Prelude.Text)
describeTopicResponse_requestId = Lens.lens (\DescribeTopicResponse' {requestId} -> requestId) (\s@DescribeTopicResponse' {} a -> s {requestId = a} :: DescribeTopicResponse)

-- | The definition of a topic.
describeTopicResponse_topic :: Lens.Lens' DescribeTopicResponse (Prelude.Maybe TopicDetails)
describeTopicResponse_topic = Lens.lens (\DescribeTopicResponse' {topic} -> topic) (\s@DescribeTopicResponse' {} a -> s {topic = a} :: DescribeTopicResponse)

-- | The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeTopicResponse_topicId :: Lens.Lens' DescribeTopicResponse (Prelude.Maybe Prelude.Text)
describeTopicResponse_topicId = Lens.lens (\DescribeTopicResponse' {topicId} -> topicId) (\s@DescribeTopicResponse' {} a -> s {topicId = a} :: DescribeTopicResponse)

-- | The HTTP status of the request.
describeTopicResponse_status :: Lens.Lens' DescribeTopicResponse Prelude.Int
describeTopicResponse_status = Lens.lens (\DescribeTopicResponse' {status} -> status) (\s@DescribeTopicResponse' {} a -> s {status = a} :: DescribeTopicResponse)

instance Prelude.NFData DescribeTopicResponse where
  rnf DescribeTopicResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topic
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

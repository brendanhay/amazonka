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
-- Module      : Amazonka.QuickSight.DeleteTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic.
module Amazonka.QuickSight.DeleteTopic
  ( -- * Creating a Request
    DeleteTopic (..),
    newDeleteTopic,

    -- * Request Lenses
    deleteTopic_awsAccountId,
    deleteTopic_topicId,

    -- * Destructuring the Response
    DeleteTopicResponse (..),
    newDeleteTopicResponse,

    -- * Response Lenses
    deleteTopicResponse_arn,
    deleteTopicResponse_requestId,
    deleteTopicResponse_topicId,
    deleteTopicResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTopic' smart constructor.
data DeleteTopic = DeleteTopic'
  { -- | The ID of the Amazon Web Services account that contains the topic that
    -- you want to delete.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to delete. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteTopic_awsAccountId' - The ID of the Amazon Web Services account that contains the topic that
-- you want to delete.
--
-- 'topicId', 'deleteTopic_topicId' - The ID of the topic that you want to delete. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newDeleteTopic ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  DeleteTopic
newDeleteTopic pAwsAccountId_ pTopicId_ =
  DeleteTopic'
    { awsAccountId = pAwsAccountId_,
      topicId = pTopicId_
    }

-- | The ID of the Amazon Web Services account that contains the topic that
-- you want to delete.
deleteTopic_awsAccountId :: Lens.Lens' DeleteTopic Prelude.Text
deleteTopic_awsAccountId = Lens.lens (\DeleteTopic' {awsAccountId} -> awsAccountId) (\s@DeleteTopic' {} a -> s {awsAccountId = a} :: DeleteTopic)

-- | The ID of the topic that you want to delete. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
deleteTopic_topicId :: Lens.Lens' DeleteTopic Prelude.Text
deleteTopic_topicId = Lens.lens (\DeleteTopic' {topicId} -> topicId) (\s@DeleteTopic' {} a -> s {topicId = a} :: DeleteTopic)

instance Core.AWSRequest DeleteTopic where
  type AWSResponse DeleteTopic = DeleteTopicResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTopicResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTopic where
  hashWithSalt _salt DeleteTopic' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData DeleteTopic where
  rnf DeleteTopic' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId

instance Data.ToHeaders DeleteTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTopic where
  toPath DeleteTopic' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId
      ]

instance Data.ToQuery DeleteTopic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse'
  { -- | The Amazon Resource Name (ARN) of the topic.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that you want to delete. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteTopicResponse_arn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'requestId', 'deleteTopicResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicId', 'deleteTopicResponse_topicId' - The ID of the topic that you want to delete. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'deleteTopicResponse_status' - The HTTP status of the request.
newDeleteTopicResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteTopicResponse
newDeleteTopicResponse pStatus_ =
  DeleteTopicResponse'
    { arn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the topic.
deleteTopicResponse_arn :: Lens.Lens' DeleteTopicResponse (Prelude.Maybe Prelude.Text)
deleteTopicResponse_arn = Lens.lens (\DeleteTopicResponse' {arn} -> arn) (\s@DeleteTopicResponse' {} a -> s {arn = a} :: DeleteTopicResponse)

-- | The Amazon Web Services request ID for this operation.
deleteTopicResponse_requestId :: Lens.Lens' DeleteTopicResponse (Prelude.Maybe Prelude.Text)
deleteTopicResponse_requestId = Lens.lens (\DeleteTopicResponse' {requestId} -> requestId) (\s@DeleteTopicResponse' {} a -> s {requestId = a} :: DeleteTopicResponse)

-- | The ID of the topic that you want to delete. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
deleteTopicResponse_topicId :: Lens.Lens' DeleteTopicResponse (Prelude.Maybe Prelude.Text)
deleteTopicResponse_topicId = Lens.lens (\DeleteTopicResponse' {topicId} -> topicId) (\s@DeleteTopicResponse' {} a -> s {topicId = a} :: DeleteTopicResponse)

-- | The HTTP status of the request.
deleteTopicResponse_status :: Lens.Lens' DeleteTopicResponse Prelude.Int
deleteTopicResponse_status = Lens.lens (\DeleteTopicResponse' {status} -> status) (\s@DeleteTopicResponse' {} a -> s {status = a} :: DeleteTopicResponse)

instance Prelude.NFData DeleteTopicResponse where
  rnf DeleteTopicResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status

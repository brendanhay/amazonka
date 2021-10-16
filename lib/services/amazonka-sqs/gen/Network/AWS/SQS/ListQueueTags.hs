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
-- Module      : Network.AWS.SQS.ListQueueTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all cost allocation tags added to the specified Amazon SQS queue.
-- For an overview, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon SQS Developer Guide/.
module Network.AWS.SQS.ListQueueTags
  ( -- * Creating a Request
    ListQueueTags (..),
    newListQueueTags,

    -- * Request Lenses
    listQueueTags_queueUrl,

    -- * Destructuring the Response
    ListQueueTagsResponse (..),
    newListQueueTagsResponse,

    -- * Response Lenses
    listQueueTagsResponse_tags,
    listQueueTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- | /See:/ 'newListQueueTags' smart constructor.
data ListQueueTags = ListQueueTags'
  { -- | The URL of the queue.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueueTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'listQueueTags_queueUrl' - The URL of the queue.
newListQueueTags ::
  -- | 'queueUrl'
  Prelude.Text ->
  ListQueueTags
newListQueueTags pQueueUrl_ =
  ListQueueTags' {queueUrl = pQueueUrl_}

-- | The URL of the queue.
listQueueTags_queueUrl :: Lens.Lens' ListQueueTags Prelude.Text
listQueueTags_queueUrl = Lens.lens (\ListQueueTags' {queueUrl} -> queueUrl) (\s@ListQueueTags' {} a -> s {queueUrl = a} :: ListQueueTags)

instance Core.AWSRequest ListQueueTags where
  type
    AWSResponse ListQueueTags =
      ListQueueTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListQueueTagsResult"
      ( \s h x ->
          ListQueueTagsResponse'
            Prelude.<$> (Core.may (Core.parseXMLMap "Tag" "Key" "Value") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueueTags

instance Prelude.NFData ListQueueTags

instance Core.ToHeaders ListQueueTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListQueueTags where
  toPath = Prelude.const "/"

instance Core.ToQuery ListQueueTags where
  toQuery ListQueueTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListQueueTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Core.=: queueUrl
      ]

-- | /See:/ 'newListQueueTagsResponse' smart constructor.
data ListQueueTagsResponse = ListQueueTagsResponse'
  { -- | The list of all tags added to the specified queue.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueueTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listQueueTagsResponse_tags' - The list of all tags added to the specified queue.
--
-- 'httpStatus', 'listQueueTagsResponse_httpStatus' - The response's http status code.
newListQueueTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueueTagsResponse
newListQueueTagsResponse pHttpStatus_ =
  ListQueueTagsResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of all tags added to the specified queue.
listQueueTagsResponse_tags :: Lens.Lens' ListQueueTagsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listQueueTagsResponse_tags = Lens.lens (\ListQueueTagsResponse' {tags} -> tags) (\s@ListQueueTagsResponse' {} a -> s {tags = a} :: ListQueueTagsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listQueueTagsResponse_httpStatus :: Lens.Lens' ListQueueTagsResponse Prelude.Int
listQueueTagsResponse_httpStatus = Lens.lens (\ListQueueTagsResponse' {httpStatus} -> httpStatus) (\s@ListQueueTagsResponse' {} a -> s {httpStatus = a} :: ListQueueTagsResponse)

instance Prelude.NFData ListQueueTagsResponse

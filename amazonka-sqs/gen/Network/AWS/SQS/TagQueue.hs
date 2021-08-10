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
-- Module      : Network.AWS.SQS.TagQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add cost allocation tags to the specified Amazon SQS queue. For an
-- overview, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- When you use queue tags, keep the following guidelines in mind:
--
-- -   Adding more than 50 tags to a queue isn\'t recommended.
--
-- -   Tags don\'t have any semantic meaning. Amazon SQS interprets tags as
--     character strings.
--
-- -   Tags are case-sensitive.
--
-- -   A new tag with a key identical to that of an existing tag overwrites
--     the existing tag.
--
-- For a full list of tag restrictions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Limits Related to Queues>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon Simple Queue Service Developer Guide/.
module Network.AWS.SQS.TagQueue
  ( -- * Creating a Request
    TagQueue (..),
    newTagQueue,

    -- * Request Lenses
    tagQueue_queueUrl,
    tagQueue_tags,

    -- * Destructuring the Response
    TagQueueResponse (..),
    newTagQueueResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- | /See:/ 'newTagQueue' smart constructor.
data TagQueue = TagQueue'
  { -- | The URL of the queue.
    queueUrl :: Prelude.Text,
    -- | The list of tags to be added to the specified queue.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'tagQueue_queueUrl' - The URL of the queue.
--
-- 'tags', 'tagQueue_tags' - The list of tags to be added to the specified queue.
newTagQueue ::
  -- | 'queueUrl'
  Prelude.Text ->
  TagQueue
newTagQueue pQueueUrl_ =
  TagQueue'
    { queueUrl = pQueueUrl_,
      tags = Prelude.mempty
    }

-- | The URL of the queue.
tagQueue_queueUrl :: Lens.Lens' TagQueue Prelude.Text
tagQueue_queueUrl = Lens.lens (\TagQueue' {queueUrl} -> queueUrl) (\s@TagQueue' {} a -> s {queueUrl = a} :: TagQueue)

-- | The list of tags to be added to the specified queue.
tagQueue_tags :: Lens.Lens' TagQueue (Prelude.HashMap Prelude.Text Prelude.Text)
tagQueue_tags = Lens.lens (\TagQueue' {tags} -> tags) (\s@TagQueue' {} a -> s {tags = a} :: TagQueue) Prelude.. Lens._Coerce

instance Core.AWSRequest TagQueue where
  type AWSResponse TagQueue = TagQueueResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull TagQueueResponse'

instance Prelude.Hashable TagQueue

instance Prelude.NFData TagQueue

instance Core.ToHeaders TagQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath TagQueue where
  toPath = Prelude.const "/"

instance Core.ToQuery TagQueue where
  toQuery TagQueue' {..} =
    Prelude.mconcat
      [ "Action" Core.=: ("TagQueue" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Core.=: queueUrl,
        Core.toQueryMap "Tags" "Key" "Value" tags
      ]

-- | /See:/ 'newTagQueueResponse' smart constructor.
data TagQueueResponse = TagQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagQueueResponse ::
  TagQueueResponse
newTagQueueResponse = TagQueueResponse'

instance Prelude.NFData TagQueueResponse

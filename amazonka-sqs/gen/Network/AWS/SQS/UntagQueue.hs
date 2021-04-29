{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.UntagQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove cost allocation tags from the specified Amazon SQS queue. For an
-- overview, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
-- in the /Amazon Simple Queue Service Developer Guide/.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon Simple Queue Service Developer Guide/.
module Network.AWS.SQS.UntagQueue
  ( -- * Creating a Request
    UntagQueue (..),
    newUntagQueue,

    -- * Request Lenses
    untagQueue_queueUrl,
    untagQueue_tagKeys,

    -- * Destructuring the Response
    UntagQueueResponse (..),
    newUntagQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- | /See:/ 'newUntagQueue' smart constructor.
data UntagQueue = UntagQueue'
  { -- | The URL of the queue.
    queueUrl :: Prelude.Text,
    -- | The list of tags to be removed from the specified queue.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'untagQueue_queueUrl' - The URL of the queue.
--
-- 'tagKeys', 'untagQueue_tagKeys' - The list of tags to be removed from the specified queue.
newUntagQueue ::
  -- | 'queueUrl'
  Prelude.Text ->
  UntagQueue
newUntagQueue pQueueUrl_ =
  UntagQueue'
    { queueUrl = pQueueUrl_,
      tagKeys = Prelude.mempty
    }

-- | The URL of the queue.
untagQueue_queueUrl :: Lens.Lens' UntagQueue Prelude.Text
untagQueue_queueUrl = Lens.lens (\UntagQueue' {queueUrl} -> queueUrl) (\s@UntagQueue' {} a -> s {queueUrl = a} :: UntagQueue)

-- | The list of tags to be removed from the specified queue.
untagQueue_tagKeys :: Lens.Lens' UntagQueue [Prelude.Text]
untagQueue_tagKeys = Lens.lens (\UntagQueue' {tagKeys} -> tagKeys) (\s@UntagQueue' {} a -> s {tagKeys = a} :: UntagQueue) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagQueue where
  type Rs UntagQueue = UntagQueueResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull UntagQueueResponse'

instance Prelude.Hashable UntagQueue

instance Prelude.NFData UntagQueue

instance Prelude.ToHeaders UntagQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagQueue where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagQueue where
  toQuery UntagQueue' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagQueue" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl,
        Prelude.toQueryList "TagKey" tagKeys
      ]

-- | /See:/ 'newUntagQueueResponse' smart constructor.
data UntagQueueResponse = UntagQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagQueueResponse ::
  UntagQueueResponse
newUntagQueueResponse = UntagQueueResponse'

instance Prelude.NFData UntagQueueResponse

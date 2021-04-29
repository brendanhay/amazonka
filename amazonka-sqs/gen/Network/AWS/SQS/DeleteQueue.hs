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
-- Module      : Network.AWS.SQS.DeleteQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queue specified by the @QueueUrl@, regardless of the
-- queue\'s contents.
--
-- Be careful with the @DeleteQueue@ action: When you delete a queue, any
-- messages in the queue are no longer available.
--
-- When you delete a queue, the deletion process takes up to 60 seconds.
-- Requests you send involving that queue during the 60 seconds might
-- succeed. For example, a @ SendMessage @ request might succeed, but after
-- 60 seconds the queue and the message you sent no longer exist.
--
-- When you delete a queue, you must wait at least 60 seconds before
-- creating a queue with the same name.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon Simple Queue Service Developer Guide/.
module Network.AWS.SQS.DeleteQueue
  ( -- * Creating a Request
    DeleteQueue (..),
    newDeleteQueue,

    -- * Request Lenses
    deleteQueue_queueUrl,

    -- * Destructuring the Response
    DeleteQueueResponse (..),
    newDeleteQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newDeleteQueue' smart constructor.
data DeleteQueue = DeleteQueue'
  { -- | The URL of the Amazon SQS queue to delete.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'deleteQueue_queueUrl' - The URL of the Amazon SQS queue to delete.
--
-- Queue URLs and names are case-sensitive.
newDeleteQueue ::
  -- | 'queueUrl'
  Prelude.Text ->
  DeleteQueue
newDeleteQueue pQueueUrl_ =
  DeleteQueue' {queueUrl = pQueueUrl_}

-- | The URL of the Amazon SQS queue to delete.
--
-- Queue URLs and names are case-sensitive.
deleteQueue_queueUrl :: Lens.Lens' DeleteQueue Prelude.Text
deleteQueue_queueUrl = Lens.lens (\DeleteQueue' {queueUrl} -> queueUrl) (\s@DeleteQueue' {} a -> s {queueUrl = a} :: DeleteQueue)

instance Prelude.AWSRequest DeleteQueue where
  type Rs DeleteQueue = DeleteQueueResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteQueueResponse'

instance Prelude.Hashable DeleteQueue

instance Prelude.NFData DeleteQueue

instance Prelude.ToHeaders DeleteQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteQueue where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteQueue where
  toQuery DeleteQueue' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteQueue" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl
      ]

-- | /See:/ 'newDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteQueueResponse ::
  DeleteQueueResponse
newDeleteQueueResponse = DeleteQueueResponse'

instance Prelude.NFData DeleteQueueResponse

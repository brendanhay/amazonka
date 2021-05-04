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
-- Module      : Network.AWS.SQS.PurgeQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the messages in a queue specified by the @QueueURL@ parameter.
--
-- When you use the @PurgeQueue@ action, you can\'t retrieve any messages
-- deleted from a queue.
--
-- The message deletion process takes up to 60 seconds. We recommend
-- waiting for 60 seconds regardless of your queue\'s size.
--
-- Messages sent to the queue /before/ you call @PurgeQueue@ might be
-- received but are deleted within the next minute.
--
-- Messages sent to the queue /after/ you call @PurgeQueue@ might be
-- deleted while the queue is being purged.
module Network.AWS.SQS.PurgeQueue
  ( -- * Creating a Request
    PurgeQueue (..),
    newPurgeQueue,

    -- * Request Lenses
    purgeQueue_queueUrl,

    -- * Destructuring the Response
    PurgeQueueResponse (..),
    newPurgeQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newPurgeQueue' smart constructor.
data PurgeQueue = PurgeQueue'
  { -- | The URL of the queue from which the @PurgeQueue@ action deletes
    -- messages.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PurgeQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'purgeQueue_queueUrl' - The URL of the queue from which the @PurgeQueue@ action deletes
-- messages.
--
-- Queue URLs and names are case-sensitive.
newPurgeQueue ::
  -- | 'queueUrl'
  Prelude.Text ->
  PurgeQueue
newPurgeQueue pQueueUrl_ =
  PurgeQueue' {queueUrl = pQueueUrl_}

-- | The URL of the queue from which the @PurgeQueue@ action deletes
-- messages.
--
-- Queue URLs and names are case-sensitive.
purgeQueue_queueUrl :: Lens.Lens' PurgeQueue Prelude.Text
purgeQueue_queueUrl = Lens.lens (\PurgeQueue' {queueUrl} -> queueUrl) (\s@PurgeQueue' {} a -> s {queueUrl = a} :: PurgeQueue)

instance Prelude.AWSRequest PurgeQueue where
  type Rs PurgeQueue = PurgeQueueResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull PurgeQueueResponse'

instance Prelude.Hashable PurgeQueue

instance Prelude.NFData PurgeQueue

instance Prelude.ToHeaders PurgeQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PurgeQueue where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PurgeQueue where
  toQuery PurgeQueue' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PurgeQueue" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl
      ]

-- | /See:/ 'newPurgeQueueResponse' smart constructor.
data PurgeQueueResponse = PurgeQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PurgeQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPurgeQueueResponse ::
  PurgeQueueResponse
newPurgeQueueResponse = PurgeQueueResponse'

instance Prelude.NFData PurgeQueueResponse

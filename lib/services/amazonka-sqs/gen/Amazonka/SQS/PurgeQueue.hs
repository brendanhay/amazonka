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
-- Module      : Amazonka.SQS.PurgeQueue
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SQS.PurgeQueue
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PurgeQueue where
  type AWSResponse PurgeQueue = PurgeQueueResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull PurgeQueueResponse'

instance Prelude.Hashable PurgeQueue where
  hashWithSalt _salt PurgeQueue' {..} =
    _salt `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData PurgeQueue where
  rnf PurgeQueue' {..} = Prelude.rnf queueUrl

instance Core.ToHeaders PurgeQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath PurgeQueue where
  toPath = Prelude.const "/"

instance Core.ToQuery PurgeQueue where
  toQuery PurgeQueue' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("PurgeQueue" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Core.=: queueUrl
      ]

-- | /See:/ 'newPurgeQueueResponse' smart constructor.
data PurgeQueueResponse = PurgeQueueResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurgeQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPurgeQueueResponse ::
  PurgeQueueResponse
newPurgeQueueResponse = PurgeQueueResponse'

instance Prelude.NFData PurgeQueueResponse where
  rnf _ = ()

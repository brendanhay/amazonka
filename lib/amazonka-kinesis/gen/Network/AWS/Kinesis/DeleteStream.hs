{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis data stream and all its shards and data. You must shut down any applications that are operating on the stream before you delete the stream. If an application attempts to operate on a deleted stream, it receives the exception @ResourceNotFoundException@ .
--
-- If the stream is in the @ACTIVE@ state, you can delete it. After a @DeleteStream@ request, the specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.
-- __Note:__ Kinesis Data Streams might continue to accept data read and write operations, such as 'PutRecord' , 'PutRecords' , and 'GetRecords' , on a stream in the @DELETING@ state until the stream deletion is complete.
-- When you delete a stream, any shards in that stream are also deleted, and any tags are dissociated from the stream.
-- You can use the 'DescribeStream' operation to check the state of the stream, which is returned in @StreamStatus@ .
-- 'DeleteStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.DeleteStream
  ( -- * Creating a request
    DeleteStream (..),
    mkDeleteStream,

    -- ** Request lenses
    dEnforceConsumerDeletion,
    dStreamName,

    -- * Destructuring the response
    DeleteStreamResponse (..),
    mkDeleteStreamResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for 'DeleteStream' .
--
-- /See:/ 'mkDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | If this parameter is unset (@null@ ) or if you set it to @false@ , and the stream has registered consumers, the call to @DeleteStream@ fails with a @ResourceInUseException@ .
    enforceConsumerDeletion :: Lude.Maybe Lude.Bool,
    -- | The name of the stream to delete.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStream' with the minimum fields required to make a request.
--
-- * 'enforceConsumerDeletion' - If this parameter is unset (@null@ ) or if you set it to @false@ , and the stream has registered consumers, the call to @DeleteStream@ fails with a @ResourceInUseException@ .
-- * 'streamName' - The name of the stream to delete.
mkDeleteStream ::
  -- | 'streamName'
  Lude.Text ->
  DeleteStream
mkDeleteStream pStreamName_ =
  DeleteStream'
    { enforceConsumerDeletion = Lude.Nothing,
      streamName = pStreamName_
    }

-- | If this parameter is unset (@null@ ) or if you set it to @false@ , and the stream has registered consumers, the call to @DeleteStream@ fails with a @ResourceInUseException@ .
--
-- /Note:/ Consider using 'enforceConsumerDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnforceConsumerDeletion :: Lens.Lens' DeleteStream (Lude.Maybe Lude.Bool)
dEnforceConsumerDeletion = Lens.lens (enforceConsumerDeletion :: DeleteStream -> Lude.Maybe Lude.Bool) (\s a -> s {enforceConsumerDeletion = a} :: DeleteStream)
{-# DEPRECATED dEnforceConsumerDeletion "Use generic-lens or generic-optics with 'enforceConsumerDeletion' instead." #-}

-- | The name of the stream to delete.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamName :: Lens.Lens' DeleteStream Lude.Text
dStreamName = Lens.lens (streamName :: DeleteStream -> Lude.Text) (\s a -> s {streamName = a} :: DeleteStream)
{-# DEPRECATED dStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull DeleteStreamResponse'

instance Lude.ToHeaders DeleteStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DeleteStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStream where
  toJSON DeleteStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EnforceConsumerDeletion" Lude..=)
              Lude.<$> enforceConsumerDeletion,
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath DeleteStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStreamResponse' with the minimum fields required to make a request.
mkDeleteStreamResponse ::
  DeleteStreamResponse
mkDeleteStreamResponse = DeleteStreamResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summarized description of the specified Kinesis data stream without the shard list.
--
-- The information returned includes the stream name, Amazon Resource Name (ARN), status, record retention period, approximate creation time, monitoring, encryption details, and open shard count.
-- 'DescribeStreamSummary' has a limit of 20 transactions per second per account.
module Network.AWS.Kinesis.DescribeStreamSummary
  ( -- * Creating a request
    DescribeStreamSummary (..),
    mkDescribeStreamSummary,

    -- ** Request lenses
    dssStreamName,

    -- * Destructuring the response
    DescribeStreamSummaryResponse (..),
    mkDescribeStreamSummaryResponse,

    -- ** Response lenses
    dssrsResponseStatus,
    dssrsStreamDescriptionSummary,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStreamSummary' smart constructor.
newtype DescribeStreamSummary = DescribeStreamSummary'
  { streamName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamSummary' with the minimum fields required to make a request.
--
-- * 'streamName' - The name of the stream to describe.
mkDescribeStreamSummary ::
  -- | 'streamName'
  Lude.Text ->
  DescribeStreamSummary
mkDescribeStreamSummary pStreamName_ =
  DescribeStreamSummary' {streamName = pStreamName_}

-- | The name of the stream to describe.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStreamName :: Lens.Lens' DescribeStreamSummary Lude.Text
dssStreamName = Lens.lens (streamName :: DescribeStreamSummary -> Lude.Text) (\s a -> s {streamName = a} :: DescribeStreamSummary)
{-# DEPRECATED dssStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest DescribeStreamSummary where
  type Rs DescribeStreamSummary = DescribeStreamSummaryResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamSummaryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "StreamDescriptionSummary")
      )

instance Lude.ToHeaders DescribeStreamSummary where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DescribeStreamSummary" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStreamSummary where
  toJSON DescribeStreamSummary' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StreamName" Lude..= streamName)])

instance Lude.ToPath DescribeStreamSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStreamSummary where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStreamSummaryResponse' smart constructor.
data DescribeStreamSummaryResponse = DescribeStreamSummaryResponse'
  { responseStatus ::
      Lude.Int,
    streamDescriptionSummary ::
      StreamDescriptionSummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamSummaryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'streamDescriptionSummary' - A 'StreamDescriptionSummary' containing information about the stream.
mkDescribeStreamSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'streamDescriptionSummary'
  StreamDescriptionSummary ->
  DescribeStreamSummaryResponse
mkDescribeStreamSummaryResponse
  pResponseStatus_
  pStreamDescriptionSummary_ =
    DescribeStreamSummaryResponse'
      { responseStatus = pResponseStatus_,
        streamDescriptionSummary = pStreamDescriptionSummary_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeStreamSummaryResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeStreamSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamSummaryResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A 'StreamDescriptionSummary' containing information about the stream.
--
-- /Note:/ Consider using 'streamDescriptionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsStreamDescriptionSummary :: Lens.Lens' DescribeStreamSummaryResponse StreamDescriptionSummary
dssrsStreamDescriptionSummary = Lens.lens (streamDescriptionSummary :: DescribeStreamSummaryResponse -> StreamDescriptionSummary) (\s a -> s {streamDescriptionSummary = a} :: DescribeStreamSummaryResponse)
{-# DEPRECATED dssrsStreamDescriptionSummary "Use generic-lens or generic-optics with 'streamDescriptionSummary' instead." #-}

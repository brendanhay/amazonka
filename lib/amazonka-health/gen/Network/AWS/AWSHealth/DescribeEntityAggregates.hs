{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEntityAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.
module Network.AWS.AWSHealth.DescribeEntityAggregates
  ( -- * Creating a request
    DescribeEntityAggregates (..),
    mkDescribeEntityAggregates,

    -- ** Request lenses
    deaEventARNs,

    -- * Destructuring the response
    DescribeEntityAggregatesResponse (..),
    mkDescribeEntityAggregatesResponse,

    -- ** Response lenses
    dearsEntityAggregates,
    dearsResponseStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEntityAggregates' smart constructor.
newtype DescribeEntityAggregates = DescribeEntityAggregates'
  { eventARNs ::
      Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEntityAggregates' with the minimum fields required to make a request.
--
-- * 'eventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
mkDescribeEntityAggregates ::
  DescribeEntityAggregates
mkDescribeEntityAggregates =
  DescribeEntityAggregates' {eventARNs = Lude.Nothing}

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- /Note:/ Consider using 'eventARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaEventARNs :: Lens.Lens' DescribeEntityAggregates (Lude.Maybe (Lude.NonEmpty Lude.Text))
deaEventARNs = Lens.lens (eventARNs :: DescribeEntityAggregates -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {eventARNs = a} :: DescribeEntityAggregates)
{-# DEPRECATED deaEventARNs "Use generic-lens or generic-optics with 'eventARNs' instead." #-}

instance Lude.AWSRequest DescribeEntityAggregates where
  type Rs DescribeEntityAggregates = DescribeEntityAggregatesResponse
  request = Req.postJSON awsHealthService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEntityAggregatesResponse'
            Lude.<$> (x Lude..?> "entityAggregates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEntityAggregates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSHealth_20160804.DescribeEntityAggregates" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEntityAggregates where
  toJSON DescribeEntityAggregates' {..} =
    Lude.object
      (Lude.catMaybes [("eventArns" Lude..=) Lude.<$> eventARNs])

instance Lude.ToPath DescribeEntityAggregates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEntityAggregates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEntityAggregatesResponse' smart constructor.
data DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse'
  { entityAggregates ::
      Lude.Maybe
        [EntityAggregate],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEntityAggregatesResponse' with the minimum fields required to make a request.
--
-- * 'entityAggregates' - The number of entities that are affected by each of the specified events.
-- * 'responseStatus' - The response status code.
mkDescribeEntityAggregatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEntityAggregatesResponse
mkDescribeEntityAggregatesResponse pResponseStatus_ =
  DescribeEntityAggregatesResponse'
    { entityAggregates =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of entities that are affected by each of the specified events.
--
-- /Note:/ Consider using 'entityAggregates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dearsEntityAggregates :: Lens.Lens' DescribeEntityAggregatesResponse (Lude.Maybe [EntityAggregate])
dearsEntityAggregates = Lens.lens (entityAggregates :: DescribeEntityAggregatesResponse -> Lude.Maybe [EntityAggregate]) (\s a -> s {entityAggregates = a} :: DescribeEntityAggregatesResponse)
{-# DEPRECATED dearsEntityAggregates "Use generic-lens or generic-optics with 'entityAggregates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dearsResponseStatus :: Lens.Lens' DescribeEntityAggregatesResponse Lude.Int
dearsResponseStatus = Lens.lens (responseStatus :: DescribeEntityAggregatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEntityAggregatesResponse)
{-# DEPRECATED dearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

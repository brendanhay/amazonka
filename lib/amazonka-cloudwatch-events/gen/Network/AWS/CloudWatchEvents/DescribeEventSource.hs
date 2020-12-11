{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared with your account.
module Network.AWS.CloudWatchEvents.DescribeEventSource
  ( -- * Creating a request
    DescribeEventSource (..),
    mkDescribeEventSource,

    -- ** Request lenses
    deseName,

    -- * Destructuring the response
    DescribeEventSourceResponse (..),
    mkDescribeEventSourceResponse,

    -- ** Response lenses
    desrsCreationTime,
    desrsState,
    desrsARN,
    desrsCreatedBy,
    desrsName,
    desrsExpirationTime,
    desrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventSource' smart constructor.
newtype DescribeEventSource = DescribeEventSource'
  { name ::
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

-- | Creates a value of 'DescribeEventSource' with the minimum fields required to make a request.
--
-- * 'name' - The name of the partner event source to display the details of.
mkDescribeEventSource ::
  -- | 'name'
  Lude.Text ->
  DescribeEventSource
mkDescribeEventSource pName_ = DescribeEventSource' {name = pName_}

-- | The name of the partner event source to display the details of.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseName :: Lens.Lens' DescribeEventSource Lude.Text
deseName = Lens.lens (name :: DescribeEventSource -> Lude.Text) (\s a -> s {name = a} :: DescribeEventSource)
{-# DEPRECATED deseName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeEventSource where
  type Rs DescribeEventSource = DescribeEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventSourceResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "CreatedBy")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "ExpirationTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribeEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventSource where
  toJSON DescribeEventSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DescribeEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    state ::
      Lude.Maybe EventSourceState,
    arn :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    expirationTime ::
      Lude.Maybe Lude.Timestamp,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventSourceResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the partner event source.
-- * 'createdBy' - The name of the SaaS partner that created the event source.
-- * 'creationTime' - The date and time that the event source was created.
-- * 'expirationTime' - The date and time that the event source will expire if you do not create a matching event bus.
-- * 'name' - The name of the partner event source.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
mkDescribeEventSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventSourceResponse
mkDescribeEventSourceResponse pResponseStatus_ =
  DescribeEventSourceResponse'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      arn = Lude.Nothing,
      createdBy = Lude.Nothing,
      name = Lude.Nothing,
      expirationTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time that the event source was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsCreationTime :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe Lude.Timestamp)
desrsCreationTime = Lens.lens (creationTime :: DescribeEventSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsState :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe EventSourceState)
desrsState = Lens.lens (state :: DescribeEventSourceResponse -> Lude.Maybe EventSourceState) (\s a -> s {state = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsARN :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe Lude.Text)
desrsARN = Lens.lens (arn :: DescribeEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the SaaS partner that created the event source.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsCreatedBy :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe Lude.Text)
desrsCreatedBy = Lens.lens (createdBy :: DescribeEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsName :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe Lude.Text)
desrsName = Lens.lens (name :: DescribeEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time that the event source will expire if you do not create a matching event bus.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsExpirationTime :: Lens.Lens' DescribeEventSourceResponse (Lude.Maybe Lude.Timestamp)
desrsExpirationTime = Lens.lens (expirationTime :: DescribeEventSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeEventSourceResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeEventSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventSourceResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

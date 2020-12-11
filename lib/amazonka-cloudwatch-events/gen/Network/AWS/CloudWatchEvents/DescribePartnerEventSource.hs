{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list details about a partner event source that they have created. AWS customers do not use this operation. Instead, AWS customers can use 'DescribeEventSource' to see details about a partner event source that is shared with them.
module Network.AWS.CloudWatchEvents.DescribePartnerEventSource
  ( -- * Creating a request
    DescribePartnerEventSource (..),
    mkDescribePartnerEventSource,

    -- ** Request lenses
    dpespName,

    -- * Destructuring the response
    DescribePartnerEventSourceResponse (..),
    mkDescribePartnerEventSourceResponse,

    -- ** Response lenses
    dpesrsARN,
    dpesrsName,
    dpesrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePartnerEventSource' smart constructor.
newtype DescribePartnerEventSource = DescribePartnerEventSource'
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

-- | Creates a value of 'DescribePartnerEventSource' with the minimum fields required to make a request.
--
-- * 'name' - The name of the event source to display.
mkDescribePartnerEventSource ::
  -- | 'name'
  Lude.Text ->
  DescribePartnerEventSource
mkDescribePartnerEventSource pName_ =
  DescribePartnerEventSource' {name = pName_}

-- | The name of the event source to display.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpespName :: Lens.Lens' DescribePartnerEventSource Lude.Text
dpespName = Lens.lens (name :: DescribePartnerEventSource -> Lude.Text) (\s a -> s {name = a} :: DescribePartnerEventSource)
{-# DEPRECATED dpespName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribePartnerEventSource where
  type
    Rs DescribePartnerEventSource =
      DescribePartnerEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePartnerEventSourceResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePartnerEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribePartnerEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePartnerEventSource where
  toJSON DescribePartnerEventSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DescribePartnerEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePartnerEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePartnerEventSourceResponse' smart constructor.
data DescribePartnerEventSourceResponse = DescribePartnerEventSourceResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    name ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribePartnerEventSourceResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the event source.
-- * 'name' - The name of the event source.
-- * 'responseStatus' - The response status code.
mkDescribePartnerEventSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePartnerEventSourceResponse
mkDescribePartnerEventSourceResponse pResponseStatus_ =
  DescribePartnerEventSourceResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the event source.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrsARN :: Lens.Lens' DescribePartnerEventSourceResponse (Lude.Maybe Lude.Text)
dpesrsARN = Lens.lens (arn :: DescribePartnerEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribePartnerEventSourceResponse)
{-# DEPRECATED dpesrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrsName :: Lens.Lens' DescribePartnerEventSourceResponse (Lude.Maybe Lude.Text)
dpesrsName = Lens.lens (name :: DescribePartnerEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribePartnerEventSourceResponse)
{-# DEPRECATED dpesrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpesrsResponseStatus :: Lens.Lens' DescribePartnerEventSourceResponse Lude.Int
dpesrsResponseStatus = Lens.lens (responseStatus :: DescribePartnerEventSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePartnerEventSourceResponse)
{-# DEPRECATED dpesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

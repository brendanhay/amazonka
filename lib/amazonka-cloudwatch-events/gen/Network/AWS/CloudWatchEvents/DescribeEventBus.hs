{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an event bus in your account. This can include the external AWS accounts that are permitted to write events to your default event bus, and the associated policy. For custom event buses and partner event buses, it displays the name, ARN, policy, state, and creation time.
--
-- To enable your account to receive events from other accounts on its default event bus, use 'PutPermission' .
-- For more information about partner event buses, see 'CreateEventBus' .
module Network.AWS.CloudWatchEvents.DescribeEventBus
  ( -- * Creating a request
    DescribeEventBus (..),
    mkDescribeEventBus,

    -- ** Request lenses
    debName,

    -- * Destructuring the response
    DescribeEventBusResponse (..),
    mkDescribeEventBusResponse,

    -- ** Response lenses
    debrsARN,
    debrsName,
    debrsPolicy,
    debrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventBus' smart constructor.
newtype DescribeEventBus = DescribeEventBus'
  { -- | The name or ARN of the event bus to show details for. If you omit this, the default event bus is displayed.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventBus' with the minimum fields required to make a request.
--
-- * 'name' - The name or ARN of the event bus to show details for. If you omit this, the default event bus is displayed.
mkDescribeEventBus ::
  DescribeEventBus
mkDescribeEventBus = DescribeEventBus' {name = Lude.Nothing}

-- | The name or ARN of the event bus to show details for. If you omit this, the default event bus is displayed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debName :: Lens.Lens' DescribeEventBus (Lude.Maybe Lude.Text)
debName = Lens.lens (name :: DescribeEventBus -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeEventBus)
{-# DEPRECATED debName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeEventBus where
  type Rs DescribeEventBus = DescribeEventBusResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventBusResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Policy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventBus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DescribeEventBus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventBus where
  toJSON DescribeEventBus' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath DescribeEventBus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventBus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventBusResponse' smart constructor.
data DescribeEventBusResponse = DescribeEventBusResponse'
  { -- | The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the event bus. Currently, this is always @default@ .
    name :: Lude.Maybe Lude.Text,
    -- | The policy that enables the external account to send events to your account.
    policy :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventBusResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
-- * 'name' - The name of the event bus. Currently, this is always @default@ .
-- * 'policy' - The policy that enables the external account to send events to your account.
-- * 'responseStatus' - The response status code.
mkDescribeEventBusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventBusResponse
mkDescribeEventBusResponse pResponseStatus_ =
  DescribeEventBusResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrsARN :: Lens.Lens' DescribeEventBusResponse (Lude.Maybe Lude.Text)
debrsARN = Lens.lens (arn :: DescribeEventBusResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeEventBusResponse)
{-# DEPRECATED debrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the event bus. Currently, this is always @default@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrsName :: Lens.Lens' DescribeEventBusResponse (Lude.Maybe Lude.Text)
debrsName = Lens.lens (name :: DescribeEventBusResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeEventBusResponse)
{-# DEPRECATED debrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The policy that enables the external account to send events to your account.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrsPolicy :: Lens.Lens' DescribeEventBusResponse (Lude.Maybe Lude.Text)
debrsPolicy = Lens.lens (policy :: DescribeEventBusResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: DescribeEventBusResponse)
{-# DEPRECATED debrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debrsResponseStatus :: Lens.Lens' DescribeEventBusResponse Lude.Int
debrsResponseStatus = Lens.lens (responseStatus :: DescribeEventBusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventBusResponse)
{-# DEPRECATED debrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

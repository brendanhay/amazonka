{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeSubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a work team provided by a vendor. It returns details about the subscription with a vendor in the AWS Marketplace.
module Network.AWS.SageMaker.DescribeSubscribedWorkteam
  ( -- * Creating a request
    DescribeSubscribedWorkteam (..),
    mkDescribeSubscribedWorkteam,

    -- ** Request lenses
    dswWorkteamARN,

    -- * Destructuring the response
    DescribeSubscribedWorkteamResponse (..),
    mkDescribeSubscribedWorkteamResponse,

    -- ** Response lenses
    dswrsSubscribedWorkteam,
    dswrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeSubscribedWorkteam' smart constructor.
newtype DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { -- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
    workteamARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscribedWorkteam' with the minimum fields required to make a request.
--
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the subscribed work team to describe.
mkDescribeSubscribedWorkteam ::
  -- | 'workteamARN'
  Lude.Text ->
  DescribeSubscribedWorkteam
mkDescribeSubscribedWorkteam pWorkteamARN_ =
  DescribeSubscribedWorkteam' {workteamARN = pWorkteamARN_}

-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswWorkteamARN :: Lens.Lens' DescribeSubscribedWorkteam Lude.Text
dswWorkteamARN = Lens.lens (workteamARN :: DescribeSubscribedWorkteam -> Lude.Text) (\s a -> s {workteamARN = a} :: DescribeSubscribedWorkteam)
{-# DEPRECATED dswWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

instance Lude.AWSRequest DescribeSubscribedWorkteam where
  type
    Rs DescribeSubscribedWorkteam =
      DescribeSubscribedWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSubscribedWorkteamResponse'
            Lude.<$> (x Lude..:> "SubscribedWorkteam")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubscribedWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeSubscribedWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSubscribedWorkteam where
  toJSON DescribeSubscribedWorkteam' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkteamArn" Lude..= workteamARN)])

instance Lude.ToPath DescribeSubscribedWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubscribedWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { -- | A @Workteam@ instance that contains information about the work team.
    subscribedWorkteam :: SubscribedWorkteam,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscribedWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'subscribedWorkteam' - A @Workteam@ instance that contains information about the work team.
-- * 'responseStatus' - The response status code.
mkDescribeSubscribedWorkteamResponse ::
  -- | 'subscribedWorkteam'
  SubscribedWorkteam ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubscribedWorkteamResponse
mkDescribeSubscribedWorkteamResponse
  pSubscribedWorkteam_
  pResponseStatus_ =
    DescribeSubscribedWorkteamResponse'
      { subscribedWorkteam =
          pSubscribedWorkteam_,
        responseStatus = pResponseStatus_
      }

-- | A @Workteam@ instance that contains information about the work team.
--
-- /Note:/ Consider using 'subscribedWorkteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrsSubscribedWorkteam :: Lens.Lens' DescribeSubscribedWorkteamResponse SubscribedWorkteam
dswrsSubscribedWorkteam = Lens.lens (subscribedWorkteam :: DescribeSubscribedWorkteamResponse -> SubscribedWorkteam) (\s a -> s {subscribedWorkteam = a} :: DescribeSubscribedWorkteamResponse)
{-# DEPRECATED dswrsSubscribedWorkteam "Use generic-lens or generic-optics with 'subscribedWorkteam' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrsResponseStatus :: Lens.Lens' DescribeSubscribedWorkteamResponse Lude.Int
dswrsResponseStatus = Lens.lens (responseStatus :: DescribeSubscribedWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubscribedWorkteamResponse)
{-# DEPRECATED dswrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

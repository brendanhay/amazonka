{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified routing profile.
module Network.AWS.Connect.DescribeRoutingProfile
  ( -- * Creating a request
    DescribeRoutingProfile (..),
    mkDescribeRoutingProfile,

    -- ** Request lenses
    drpInstanceId,
    drpRoutingProfileId,

    -- * Destructuring the response
    DescribeRoutingProfileResponse (..),
    mkDescribeRoutingProfileResponse,

    -- ** Response lenses
    drprsRoutingProfile,
    drprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRoutingProfile' smart constructor.
data DescribeRoutingProfile = DescribeRoutingProfile'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRoutingProfile' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile.
mkDescribeRoutingProfile ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'routingProfileId'
  Lude.Text ->
  DescribeRoutingProfile
mkDescribeRoutingProfile pInstanceId_ pRoutingProfileId_ =
  DescribeRoutingProfile'
    { instanceId = pInstanceId_,
      routingProfileId = pRoutingProfileId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpInstanceId :: Lens.Lens' DescribeRoutingProfile Lude.Text
drpInstanceId = Lens.lens (instanceId :: DescribeRoutingProfile -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeRoutingProfile)
{-# DEPRECATED drpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the routing profile.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRoutingProfileId :: Lens.Lens' DescribeRoutingProfile Lude.Text
drpRoutingProfileId = Lens.lens (routingProfileId :: DescribeRoutingProfile -> Lude.Text) (\s a -> s {routingProfileId = a} :: DescribeRoutingProfile)
{-# DEPRECATED drpRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

instance Lude.AWSRequest DescribeRoutingProfile where
  type Rs DescribeRoutingProfile = DescribeRoutingProfileResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRoutingProfileResponse'
            Lude.<$> (x Lude..?> "RoutingProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRoutingProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeRoutingProfile where
  toPath DescribeRoutingProfile' {..} =
    Lude.mconcat
      [ "/routing-profiles/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS routingProfileId
      ]

instance Lude.ToQuery DescribeRoutingProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRoutingProfileResponse' smart constructor.
data DescribeRoutingProfileResponse = DescribeRoutingProfileResponse'
  { -- | The routing profile.
    routingProfile :: Lude.Maybe RoutingProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRoutingProfileResponse' with the minimum fields required to make a request.
--
-- * 'routingProfile' - The routing profile.
-- * 'responseStatus' - The response status code.
mkDescribeRoutingProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRoutingProfileResponse
mkDescribeRoutingProfileResponse pResponseStatus_ =
  DescribeRoutingProfileResponse'
    { routingProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The routing profile.
--
-- /Note:/ Consider using 'routingProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsRoutingProfile :: Lens.Lens' DescribeRoutingProfileResponse (Lude.Maybe RoutingProfile)
drprsRoutingProfile = Lens.lens (routingProfile :: DescribeRoutingProfileResponse -> Lude.Maybe RoutingProfile) (\s a -> s {routingProfile = a} :: DescribeRoutingProfileResponse)
{-# DEPRECATED drprsRoutingProfile "Use generic-lens or generic-optics with 'routingProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DescribeRoutingProfileResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DescribeRoutingProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRoutingProfileResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

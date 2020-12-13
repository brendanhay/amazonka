{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information such as the create date, the last updated date, membership information, and the work team's Amazon Resource Name (ARN).
module Network.AWS.SageMaker.DescribeWorkteam
  ( -- * Creating a request
    DescribeWorkteam (..),
    mkDescribeWorkteam,

    -- ** Request lenses
    dWorkteamName,

    -- * Destructuring the response
    DescribeWorkteamResponse (..),
    mkDescribeWorkteamResponse,

    -- ** Response lenses
    dwgrsWorkteam,
    dwgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeWorkteam' smart constructor.
newtype DescribeWorkteam = DescribeWorkteam'
  { -- | The name of the work team to return a description of.
    workteamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkteam' with the minimum fields required to make a request.
--
-- * 'workteamName' - The name of the work team to return a description of.
mkDescribeWorkteam ::
  -- | 'workteamName'
  Lude.Text ->
  DescribeWorkteam
mkDescribeWorkteam pWorkteamName_ =
  DescribeWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to return a description of.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkteamName :: Lens.Lens' DescribeWorkteam Lude.Text
dWorkteamName = Lens.lens (workteamName :: DescribeWorkteam -> Lude.Text) (\s a -> s {workteamName = a} :: DescribeWorkteam)
{-# DEPRECATED dWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

instance Lude.AWSRequest DescribeWorkteam where
  type Rs DescribeWorkteam = DescribeWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkteamResponse'
            Lude.<$> (x Lude..:> "Workteam") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkteam where
  toJSON DescribeWorkteam' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkteamName" Lude..= workteamName)])

instance Lude.ToPath DescribeWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { -- | A @Workteam@ instance that contains information about the work team.
    workteam :: Workteam,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'workteam' - A @Workteam@ instance that contains information about the work team.
-- * 'responseStatus' - The response status code.
mkDescribeWorkteamResponse ::
  -- | 'workteam'
  Workteam ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkteamResponse
mkDescribeWorkteamResponse pWorkteam_ pResponseStatus_ =
  DescribeWorkteamResponse'
    { workteam = pWorkteam_,
      responseStatus = pResponseStatus_
    }

-- | A @Workteam@ instance that contains information about the work team.
--
-- /Note:/ Consider using 'workteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgrsWorkteam :: Lens.Lens' DescribeWorkteamResponse Workteam
dwgrsWorkteam = Lens.lens (workteam :: DescribeWorkteamResponse -> Workteam) (\s a -> s {workteam = a} :: DescribeWorkteamResponse)
{-# DEPRECATED dwgrsWorkteam "Use generic-lens or generic-optics with 'workteam' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgrsResponseStatus :: Lens.Lens' DescribeWorkteamResponse Lude.Int
dwgrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkteamResponse)
{-# DEPRECATED dwgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

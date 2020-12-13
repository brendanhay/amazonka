{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the 'Cluster$VisibleToAllUsers' value, which determines whether the cluster is visible to all IAM users of the AWS account associated with the cluster. Only the IAM user who created the cluster or the AWS account root user can call this action. The default value, @true@ , indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. If set to @false@ , only the IAM user that created the cluster can perform actions. This action works on running clusters. You can override the default @true@ setting when you create a cluster by using the @VisibleToAllUsers@ parameter with @RunJobFlow@ .
module Network.AWS.EMR.SetVisibleToAllUsers
  ( -- * Creating a request
    SetVisibleToAllUsers (..),
    mkSetVisibleToAllUsers,

    -- ** Request lenses
    svtauJobFlowIds,
    svtauVisibleToAllUsers,

    -- * Destructuring the response
    SetVisibleToAllUsersResponse (..),
    mkSetVisibleToAllUsersResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'mkSetVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { -- | The unique identifier of the job flow (cluster).
    jobFlowIds :: [Lude.Text],
    -- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
    visibleToAllUsers :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetVisibleToAllUsers' with the minimum fields required to make a request.
--
-- * 'jobFlowIds' - The unique identifier of the job flow (cluster).
-- * 'visibleToAllUsers' - A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
mkSetVisibleToAllUsers ::
  -- | 'visibleToAllUsers'
  Lude.Bool ->
  SetVisibleToAllUsers
mkSetVisibleToAllUsers pVisibleToAllUsers_ =
  SetVisibleToAllUsers'
    { jobFlowIds = Lude.mempty,
      visibleToAllUsers = pVisibleToAllUsers_
    }

-- | The unique identifier of the job flow (cluster).
--
-- /Note:/ Consider using 'jobFlowIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svtauJobFlowIds :: Lens.Lens' SetVisibleToAllUsers [Lude.Text]
svtauJobFlowIds = Lens.lens (jobFlowIds :: SetVisibleToAllUsers -> [Lude.Text]) (\s a -> s {jobFlowIds = a} :: SetVisibleToAllUsers)
{-# DEPRECATED svtauJobFlowIds "Use generic-lens or generic-optics with 'jobFlowIds' instead." #-}

-- | A value of @true@ indicates that all IAM users in the AWS account can perform cluster actions if they have the proper IAM policy permissions. This is the default. A value of @false@ indicates that only the IAM user who created the cluster can perform actions.
--
-- /Note:/ Consider using 'visibleToAllUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svtauVisibleToAllUsers :: Lens.Lens' SetVisibleToAllUsers Lude.Bool
svtauVisibleToAllUsers = Lens.lens (visibleToAllUsers :: SetVisibleToAllUsers -> Lude.Bool) (\s a -> s {visibleToAllUsers = a} :: SetVisibleToAllUsers)
{-# DEPRECATED svtauVisibleToAllUsers "Use generic-lens or generic-optics with 'visibleToAllUsers' instead." #-}

instance Lude.AWSRequest SetVisibleToAllUsers where
  type Rs SetVisibleToAllUsers = SetVisibleToAllUsersResponse
  request = Req.postJSON emrService
  response = Res.receiveNull SetVisibleToAllUsersResponse'

instance Lude.ToHeaders SetVisibleToAllUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.SetVisibleToAllUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetVisibleToAllUsers where
  toJSON SetVisibleToAllUsers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobFlowIds" Lude..= jobFlowIds),
            Lude.Just ("VisibleToAllUsers" Lude..= visibleToAllUsers)
          ]
      )

instance Lude.ToPath SetVisibleToAllUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery SetVisibleToAllUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetVisibleToAllUsersResponse' with the minimum fields required to make a request.
mkSetVisibleToAllUsersResponse ::
  SetVisibleToAllUsersResponse
mkSetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'

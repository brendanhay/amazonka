{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DeleteEnvironmentMembership
  ( -- * Creating a request
    DeleteEnvironmentMembership (..),
    mkDeleteEnvironmentMembership,

    -- ** Request lenses
    demEnvironmentId,
    demUserARN,

    -- * Destructuring the response
    DeleteEnvironmentMembershipResponse (..),
    mkDeleteEnvironmentMembershipResponse,

    -- ** Response lenses
    demrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { environmentId ::
      Lude.Text,
    userARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEnvironmentMembership' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment to delete the environment member from.
-- * 'userARN' - The Amazon Resource Name (ARN) of the environment member to delete from the environment.
mkDeleteEnvironmentMembership ::
  -- | 'environmentId'
  Lude.Text ->
  -- | 'userARN'
  Lude.Text ->
  DeleteEnvironmentMembership
mkDeleteEnvironmentMembership pEnvironmentId_ pUserARN_ =
  DeleteEnvironmentMembership'
    { environmentId = pEnvironmentId_,
      userARN = pUserARN_
    }

-- | The ID of the environment to delete the environment member from.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demEnvironmentId :: Lens.Lens' DeleteEnvironmentMembership Lude.Text
demEnvironmentId = Lens.lens (environmentId :: DeleteEnvironmentMembership -> Lude.Text) (\s a -> s {environmentId = a} :: DeleteEnvironmentMembership)
{-# DEPRECATED demEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member to delete from the environment.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demUserARN :: Lens.Lens' DeleteEnvironmentMembership Lude.Text
demUserARN = Lens.lens (userARN :: DeleteEnvironmentMembership -> Lude.Text) (\s a -> s {userARN = a} :: DeleteEnvironmentMembership)
{-# DEPRECATED demUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest DeleteEnvironmentMembership where
  type
    Rs DeleteEnvironmentMembership =
      DeleteEnvironmentMembershipResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteEnvironmentMembershipResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEnvironmentMembership where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEnvironmentMembership where
  toJSON DeleteEnvironmentMembership' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("environmentId" Lude..= environmentId),
            Lude.Just ("userArn" Lude..= userARN)
          ]
      )

instance Lude.ToPath DeleteEnvironmentMembership where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEnvironmentMembership where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEnvironmentMembershipResponse' smart constructor.
newtype DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEnvironmentMembershipResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEnvironmentMembershipResponse
mkDeleteEnvironmentMembershipResponse pResponseStatus_ =
  DeleteEnvironmentMembershipResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrsResponseStatus :: Lens.Lens' DeleteEnvironmentMembershipResponse Lude.Int
demrsResponseStatus = Lens.lens (responseStatus :: DeleteEnvironmentMembershipResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEnvironmentMembershipResponse)
{-# DEPRECATED demrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

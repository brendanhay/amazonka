{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifySelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the self-service WorkSpace management capabilities for your users. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
module Network.AWS.WorkSpaces.ModifySelfservicePermissions
  ( -- * Creating a request
    ModifySelfservicePermissions (..),
    mkModifySelfservicePermissions,

    -- ** Request lenses
    mspResourceId,
    mspSelfservicePermissions,

    -- * Destructuring the response
    ModifySelfservicePermissionsResponse (..),
    mkModifySelfservicePermissionsResponse,

    -- ** Response lenses
    msprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifySelfservicePermissions' smart constructor.
data ModifySelfservicePermissions = ModifySelfservicePermissions'
  { resourceId ::
      Lude.Text,
    selfservicePermissions ::
      SelfservicePermissions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySelfservicePermissions' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the directory.
-- * 'selfservicePermissions' - The permissions to enable or disable self-service capabilities.
mkModifySelfservicePermissions ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'selfservicePermissions'
  SelfservicePermissions ->
  ModifySelfservicePermissions
mkModifySelfservicePermissions
  pResourceId_
  pSelfservicePermissions_ =
    ModifySelfservicePermissions'
      { resourceId = pResourceId_,
        selfservicePermissions = pSelfservicePermissions_
      }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspResourceId :: Lens.Lens' ModifySelfservicePermissions Lude.Text
mspResourceId = Lens.lens (resourceId :: ModifySelfservicePermissions -> Lude.Text) (\s a -> s {resourceId = a} :: ModifySelfservicePermissions)
{-# DEPRECATED mspResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The permissions to enable or disable self-service capabilities.
--
-- /Note:/ Consider using 'selfservicePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspSelfservicePermissions :: Lens.Lens' ModifySelfservicePermissions SelfservicePermissions
mspSelfservicePermissions = Lens.lens (selfservicePermissions :: ModifySelfservicePermissions -> SelfservicePermissions) (\s a -> s {selfservicePermissions = a} :: ModifySelfservicePermissions)
{-# DEPRECATED mspSelfservicePermissions "Use generic-lens or generic-optics with 'selfservicePermissions' instead." #-}

instance Lude.AWSRequest ModifySelfservicePermissions where
  type
    Rs ModifySelfservicePermissions =
      ModifySelfservicePermissionsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifySelfservicePermissionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifySelfservicePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.ModifySelfservicePermissions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifySelfservicePermissions where
  toJSON ModifySelfservicePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just
              ("SelfservicePermissions" Lude..= selfservicePermissions)
          ]
      )

instance Lude.ToPath ModifySelfservicePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySelfservicePermissions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifySelfservicePermissionsResponse' smart constructor.
newtype ModifySelfservicePermissionsResponse = ModifySelfservicePermissionsResponse'
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

-- | Creates a value of 'ModifySelfservicePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifySelfservicePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifySelfservicePermissionsResponse
mkModifySelfservicePermissionsResponse pResponseStatus_ =
  ModifySelfservicePermissionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msprsResponseStatus :: Lens.Lens' ModifySelfservicePermissionsResponse Lude.Int
msprsResponseStatus = Lens.lens (responseStatus :: ModifySelfservicePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifySelfservicePermissionsResponse)
{-# DEPRECATED msprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

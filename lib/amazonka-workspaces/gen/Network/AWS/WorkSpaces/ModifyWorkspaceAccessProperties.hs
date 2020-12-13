{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies which devices and operating systems users can use to access their WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html#control-device-access Control Device Access> .
module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
  ( -- * Creating a request
    ModifyWorkspaceAccessProperties (..),
    mkModifyWorkspaceAccessProperties,

    -- ** Request lenses
    mwapResourceId,
    mwapWorkspaceAccessProperties,

    -- * Destructuring the response
    ModifyWorkspaceAccessPropertiesResponse (..),
    mkModifyWorkspaceAccessPropertiesResponse,

    -- ** Response lenses
    mwaprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { -- | The identifier of the directory.
    resourceId :: Lude.Text,
    -- | The device types and operating systems to enable or disable for access.
    workspaceAccessProperties :: WorkspaceAccessProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceAccessProperties' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the directory.
-- * 'workspaceAccessProperties' - The device types and operating systems to enable or disable for access.
mkModifyWorkspaceAccessProperties ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'workspaceAccessProperties'
  WorkspaceAccessProperties ->
  ModifyWorkspaceAccessProperties
mkModifyWorkspaceAccessProperties
  pResourceId_
  pWorkspaceAccessProperties_ =
    ModifyWorkspaceAccessProperties'
      { resourceId = pResourceId_,
        workspaceAccessProperties = pWorkspaceAccessProperties_
      }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapResourceId :: Lens.Lens' ModifyWorkspaceAccessProperties Lude.Text
mwapResourceId = Lens.lens (resourceId :: ModifyWorkspaceAccessProperties -> Lude.Text) (\s a -> s {resourceId = a} :: ModifyWorkspaceAccessProperties)
{-# DEPRECATED mwapResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The device types and operating systems to enable or disable for access.
--
-- /Note:/ Consider using 'workspaceAccessProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapWorkspaceAccessProperties :: Lens.Lens' ModifyWorkspaceAccessProperties WorkspaceAccessProperties
mwapWorkspaceAccessProperties = Lens.lens (workspaceAccessProperties :: ModifyWorkspaceAccessProperties -> WorkspaceAccessProperties) (\s a -> s {workspaceAccessProperties = a} :: ModifyWorkspaceAccessProperties)
{-# DEPRECATED mwapWorkspaceAccessProperties "Use generic-lens or generic-optics with 'workspaceAccessProperties' instead." #-}

instance Lude.AWSRequest ModifyWorkspaceAccessProperties where
  type
    Rs ModifyWorkspaceAccessProperties =
      ModifyWorkspaceAccessPropertiesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyWorkspaceAccessProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.ModifyWorkspaceAccessProperties" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyWorkspaceAccessProperties where
  toJSON ModifyWorkspaceAccessProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just
              ("WorkspaceAccessProperties" Lude..= workspaceAccessProperties)
          ]
      )

instance Lude.ToPath ModifyWorkspaceAccessProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyWorkspaceAccessProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyWorkspaceAccessPropertiesResponse' smart constructor.
newtype ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceAccessPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyWorkspaceAccessPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyWorkspaceAccessPropertiesResponse
mkModifyWorkspaceAccessPropertiesResponse pResponseStatus_ =
  ModifyWorkspaceAccessPropertiesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwaprsResponseStatus :: Lens.Lens' ModifyWorkspaceAccessPropertiesResponse Lude.Int
mwaprsResponseStatus = Lens.lens (responseStatus :: ModifyWorkspaceAccessPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyWorkspaceAccessPropertiesResponse)
{-# DEPRECATED mwaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

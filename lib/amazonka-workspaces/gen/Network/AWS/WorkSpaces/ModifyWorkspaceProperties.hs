{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified WorkSpace properties. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
module Network.AWS.WorkSpaces.ModifyWorkspaceProperties
  ( -- * Creating a request
    ModifyWorkspaceProperties (..),
    mkModifyWorkspaceProperties,

    -- ** Request lenses
    mwpWorkspaceProperties,
    mwpWorkspaceId,

    -- * Destructuring the response
    ModifyWorkspacePropertiesResponse (..),
    mkModifyWorkspacePropertiesResponse,

    -- ** Response lenses
    mwprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyWorkspaceProperties' smart constructor.
data ModifyWorkspaceProperties = ModifyWorkspaceProperties'
  { -- | The properties of the WorkSpace.
    workspaceProperties :: WorkspaceProperties,
    -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceProperties' with the minimum fields required to make a request.
--
-- * 'workspaceProperties' - The properties of the WorkSpace.
-- * 'workspaceId' - The identifier of the WorkSpace.
mkModifyWorkspaceProperties ::
  -- | 'workspaceProperties'
  WorkspaceProperties ->
  -- | 'workspaceId'
  Lude.Text ->
  ModifyWorkspaceProperties
mkModifyWorkspaceProperties pWorkspaceProperties_ pWorkspaceId_ =
  ModifyWorkspaceProperties'
    { workspaceProperties =
        pWorkspaceProperties_,
      workspaceId = pWorkspaceId_
    }

-- | The properties of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwpWorkspaceProperties :: Lens.Lens' ModifyWorkspaceProperties WorkspaceProperties
mwpWorkspaceProperties = Lens.lens (workspaceProperties :: ModifyWorkspaceProperties -> WorkspaceProperties) (\s a -> s {workspaceProperties = a} :: ModifyWorkspaceProperties)
{-# DEPRECATED mwpWorkspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead." #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwpWorkspaceId :: Lens.Lens' ModifyWorkspaceProperties Lude.Text
mwpWorkspaceId = Lens.lens (workspaceId :: ModifyWorkspaceProperties -> Lude.Text) (\s a -> s {workspaceId = a} :: ModifyWorkspaceProperties)
{-# DEPRECATED mwpWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.AWSRequest ModifyWorkspaceProperties where
  type
    Rs ModifyWorkspaceProperties =
      ModifyWorkspacePropertiesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyWorkspacePropertiesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyWorkspaceProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.ModifyWorkspaceProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyWorkspaceProperties where
  toJSON ModifyWorkspaceProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WorkspaceProperties" Lude..= workspaceProperties),
            Lude.Just ("WorkspaceId" Lude..= workspaceId)
          ]
      )

instance Lude.ToPath ModifyWorkspaceProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyWorkspaceProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyWorkspacePropertiesResponse' smart constructor.
newtype ModifyWorkspacePropertiesResponse = ModifyWorkspacePropertiesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspacePropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyWorkspacePropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyWorkspacePropertiesResponse
mkModifyWorkspacePropertiesResponse pResponseStatus_ =
  ModifyWorkspacePropertiesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwprsResponseStatus :: Lens.Lens' ModifyWorkspacePropertiesResponse Lude.Int
mwprsResponseStatus = Lens.lens (responseStatus :: ModifyWorkspacePropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyWorkspacePropertiesResponse)
{-# DEPRECATED mwprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

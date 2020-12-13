{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the default properties used to create WorkSpaces.
module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
  ( -- * Creating a request
    ModifyWorkspaceCreationProperties (..),
    mkModifyWorkspaceCreationProperties,

    -- ** Request lenses
    mwcpResourceId,
    mwcpWorkspaceCreationProperties,

    -- * Destructuring the response
    ModifyWorkspaceCreationPropertiesResponse (..),
    mkModifyWorkspaceCreationPropertiesResponse,

    -- ** Response lenses
    mwcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { -- | The identifier of the directory.
    resourceId :: Lude.Text,
    -- | The default properties for creating WorkSpaces.
    workspaceCreationProperties :: WorkspaceCreationProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the directory.
-- * 'workspaceCreationProperties' - The default properties for creating WorkSpaces.
mkModifyWorkspaceCreationProperties ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'workspaceCreationProperties'
  WorkspaceCreationProperties ->
  ModifyWorkspaceCreationProperties
mkModifyWorkspaceCreationProperties
  pResourceId_
  pWorkspaceCreationProperties_ =
    ModifyWorkspaceCreationProperties'
      { resourceId = pResourceId_,
        workspaceCreationProperties = pWorkspaceCreationProperties_
      }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpResourceId :: Lens.Lens' ModifyWorkspaceCreationProperties Lude.Text
mwcpResourceId = Lens.lens (resourceId :: ModifyWorkspaceCreationProperties -> Lude.Text) (\s a -> s {resourceId = a} :: ModifyWorkspaceCreationProperties)
{-# DEPRECATED mwcpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The default properties for creating WorkSpaces.
--
-- /Note:/ Consider using 'workspaceCreationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpWorkspaceCreationProperties :: Lens.Lens' ModifyWorkspaceCreationProperties WorkspaceCreationProperties
mwcpWorkspaceCreationProperties = Lens.lens (workspaceCreationProperties :: ModifyWorkspaceCreationProperties -> WorkspaceCreationProperties) (\s a -> s {workspaceCreationProperties = a} :: ModifyWorkspaceCreationProperties)
{-# DEPRECATED mwcpWorkspaceCreationProperties "Use generic-lens or generic-optics with 'workspaceCreationProperties' instead." #-}

instance Lude.AWSRequest ModifyWorkspaceCreationProperties where
  type
    Rs ModifyWorkspaceCreationProperties =
      ModifyWorkspaceCreationPropertiesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceCreationPropertiesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyWorkspaceCreationProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.ModifyWorkspaceCreationProperties" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyWorkspaceCreationProperties where
  toJSON ModifyWorkspaceCreationProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just
              ( "WorkspaceCreationProperties"
                  Lude..= workspaceCreationProperties
              )
          ]
      )

instance Lude.ToPath ModifyWorkspaceCreationProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyWorkspaceCreationProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyWorkspaceCreationPropertiesResponse' smart constructor.
newtype ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceCreationPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyWorkspaceCreationPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyWorkspaceCreationPropertiesResponse
mkModifyWorkspaceCreationPropertiesResponse pResponseStatus_ =
  ModifyWorkspaceCreationPropertiesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcprsResponseStatus :: Lens.Lens' ModifyWorkspaceCreationPropertiesResponse Lude.Int
mwcprsResponseStatus = Lens.lens (responseStatus :: ModifyWorkspaceCreationPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyWorkspaceCreationPropertiesResponse)
{-# DEPRECATED mwcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

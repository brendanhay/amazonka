{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of the specified WorkSpace.
--
-- To maintain a WorkSpace without being interrupted, set the WorkSpace state to @ADMIN_MAINTENANCE@ . WorkSpaces in this state do not respond to requests to reboot, stop, start, rebuild, or restore. An AutoStop WorkSpace in this state is not stopped. Users cannot log into a WorkSpace in the @ADMIN_MAINTENANCE@ state.
module Network.AWS.WorkSpaces.ModifyWorkspaceState
  ( -- * Creating a request
    ModifyWorkspaceState (..),
    mkModifyWorkspaceState,

    -- ** Request lenses
    mwsWorkspaceState,
    mwsWorkspaceId,

    -- * Destructuring the response
    ModifyWorkspaceStateResponse (..),
    mkModifyWorkspaceStateResponse,

    -- ** Response lenses
    mwsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyWorkspaceState' smart constructor.
data ModifyWorkspaceState = ModifyWorkspaceState'
  { -- | The WorkSpace state.
    workspaceState :: TargetWorkspaceState,
    -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceState' with the minimum fields required to make a request.
--
-- * 'workspaceState' - The WorkSpace state.
-- * 'workspaceId' - The identifier of the WorkSpace.
mkModifyWorkspaceState ::
  -- | 'workspaceState'
  TargetWorkspaceState ->
  -- | 'workspaceId'
  Lude.Text ->
  ModifyWorkspaceState
mkModifyWorkspaceState pWorkspaceState_ pWorkspaceId_ =
  ModifyWorkspaceState'
    { workspaceState = pWorkspaceState_,
      workspaceId = pWorkspaceId_
    }

-- | The WorkSpace state.
--
-- /Note:/ Consider using 'workspaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceState :: Lens.Lens' ModifyWorkspaceState TargetWorkspaceState
mwsWorkspaceState = Lens.lens (workspaceState :: ModifyWorkspaceState -> TargetWorkspaceState) (\s a -> s {workspaceState = a} :: ModifyWorkspaceState)
{-# DEPRECATED mwsWorkspaceState "Use generic-lens or generic-optics with 'workspaceState' instead." #-}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceId :: Lens.Lens' ModifyWorkspaceState Lude.Text
mwsWorkspaceId = Lens.lens (workspaceId :: ModifyWorkspaceState -> Lude.Text) (\s a -> s {workspaceId = a} :: ModifyWorkspaceState)
{-# DEPRECATED mwsWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.AWSRequest ModifyWorkspaceState where
  type Rs ModifyWorkspaceState = ModifyWorkspaceStateResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyWorkspaceState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.ModifyWorkspaceState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyWorkspaceState where
  toJSON ModifyWorkspaceState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WorkspaceState" Lude..= workspaceState),
            Lude.Just ("WorkspaceId" Lude..= workspaceId)
          ]
      )

instance Lude.ToPath ModifyWorkspaceState where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyWorkspaceState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyWorkspaceStateResponse' smart constructor.
newtype ModifyWorkspaceStateResponse = ModifyWorkspaceStateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyWorkspaceStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyWorkspaceStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyWorkspaceStateResponse
mkModifyWorkspaceStateResponse pResponseStatus_ =
  ModifyWorkspaceStateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsrsResponseStatus :: Lens.Lens' ModifyWorkspaceStateResponse Lude.Int
mwsrsResponseStatus = Lens.lens (responseStatus :: ModifyWorkspaceStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyWorkspaceStateResponse)
{-# DEPRECATED mwsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

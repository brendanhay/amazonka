{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified project name, given the project ARN and a new name.
module Network.AWS.DeviceFarm.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upArn,
    upName,
    upDefaultJobTimeoutMinutes,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprsProject,
    uprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the update project operation.
--
-- /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | The Amazon Resource Name (ARN) of the project whose name to update.
    arn :: Lude.Text,
    -- | A string that represents the new name of the project that you are updating.
    name :: Lude.Maybe Lude.Text,
    -- | The number of minutes a test run in the project executes before it times out.
    defaultJobTimeoutMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project whose name to update.
-- * 'name' - A string that represents the new name of the project that you are updating.
-- * 'defaultJobTimeoutMinutes' - The number of minutes a test run in the project executes before it times out.
mkUpdateProject ::
  -- | 'arn'
  Lude.Text ->
  UpdateProject
mkUpdateProject pArn_ =
  UpdateProject'
    { arn = pArn_,
      name = Lude.Nothing,
      defaultJobTimeoutMinutes = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project whose name to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upArn :: Lens.Lens' UpdateProject Lude.Text
upArn = Lens.lens (arn :: UpdateProject -> Lude.Text) (\s a -> s {arn = a} :: UpdateProject)
{-# DEPRECATED upArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A string that represents the new name of the project that you are updating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UpdateProject -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateProject)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of minutes a test run in the project executes before it times out.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDefaultJobTimeoutMinutes :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Int)
upDefaultJobTimeoutMinutes = Lens.lens (defaultJobTimeoutMinutes :: UpdateProject -> Lude.Maybe Lude.Int) (\s a -> s {defaultJobTimeoutMinutes = a} :: UpdateProject)
{-# DEPRECATED upDefaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead." #-}

instance Lude.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Lude.<$> (x Lude..?> "project") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("name" Lude..=) Lude.<$> name,
            ("defaultJobTimeoutMinutes" Lude..=)
              Lude.<$> defaultJobTimeoutMinutes
          ]
      )

instance Lude.ToPath UpdateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProject where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of an update project request.
--
-- /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The project to update.
    project :: Lude.Maybe Project,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- * 'project' - The project to update.
-- * 'responseStatus' - The response status code.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse pResponseStatus_ =
  UpdateProjectResponse'
    { project = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The project to update.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsProject :: Lens.Lens' UpdateProjectResponse (Lude.Maybe Project)
uprsProject = Lens.lens (project :: UpdateProjectResponse -> Lude.Maybe Project) (\s a -> s {project = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdateProjectResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

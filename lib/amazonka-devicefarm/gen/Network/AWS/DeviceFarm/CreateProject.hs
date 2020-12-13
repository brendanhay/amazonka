{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project.
module Network.AWS.DeviceFarm.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpName,
    cpDefaultJobTimeoutMinutes,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprsProject,
    cprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the create project operation.
--
-- /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | The project's name.
    name :: Lude.Text,
    -- | Sets the execution timeout value (in minutes) for a project. All test runs in this project use the specified execution timeout value unless overridden when scheduling a run.
    defaultJobTimeoutMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- * 'name' - The project's name.
-- * 'defaultJobTimeoutMinutes' - Sets the execution timeout value (in minutes) for a project. All test runs in this project use the specified execution timeout value unless overridden when scheduling a run.
mkCreateProject ::
  -- | 'name'
  Lude.Text ->
  CreateProject
mkCreateProject pName_ =
  CreateProject'
    { name = pName_,
      defaultJobTimeoutMinutes = Lude.Nothing
    }

-- | The project's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject Lude.Text
cpName = Lens.lens (name :: CreateProject -> Lude.Text) (\s a -> s {name = a} :: CreateProject)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Sets the execution timeout value (in minutes) for a project. All test runs in this project use the specified execution timeout value unless overridden when scheduling a run.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDefaultJobTimeoutMinutes :: Lens.Lens' CreateProject (Lude.Maybe Lude.Int)
cpDefaultJobTimeoutMinutes = Lens.lens (defaultJobTimeoutMinutes :: CreateProject -> Lude.Maybe Lude.Int) (\s a -> s {defaultJobTimeoutMinutes = a} :: CreateProject)
{-# DEPRECATED cpDefaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead." #-}

instance Lude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Lude.<$> (x Lude..?> "project") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            ("defaultJobTimeoutMinutes" Lude..=)
              Lude.<$> defaultJobTimeoutMinutes
          ]
      )

instance Lude.ToPath CreateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProject where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a create project request.
--
-- /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The newly created project.
    project :: Lude.Maybe Project,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- * 'project' - The newly created project.
-- * 'responseStatus' - The response status code.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProjectResponse
mkCreateProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    { project = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly created project.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProject :: Lens.Lens' CreateProjectResponse (Lude.Maybe Project)
cprsProject = Lens.lens (project :: CreateProjectResponse -> Lude.Maybe Project) (\s a -> s {project = a} :: CreateProjectResponse)
{-# DEPRECATED cprsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProjectResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

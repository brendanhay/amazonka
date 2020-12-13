{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
module Network.AWS.DeviceFarm.GetProject
  ( -- * Creating a request
    GetProject (..),
    mkGetProject,

    -- ** Request lenses
    gpArn,

    -- * Destructuring the response
    GetProjectResponse (..),
    mkGetProjectResponse,

    -- ** Response lenses
    gprsProject,
    gprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get project operation.
--
-- /See:/ 'mkGetProject' smart constructor.
newtype GetProject = GetProject'
  { -- | The project's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProject' with the minimum fields required to make a request.
--
-- * 'arn' - The project's ARN.
mkGetProject ::
  -- | 'arn'
  Lude.Text ->
  GetProject
mkGetProject pArn_ = GetProject' {arn = pArn_}

-- | The project's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpArn :: Lens.Lens' GetProject Lude.Text
gpArn = Lens.lens (arn :: GetProject -> Lude.Text) (\s a -> s {arn = a} :: GetProject)
{-# DEPRECATED gpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetProject where
  type Rs GetProject = GetProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProjectResponse'
            Lude.<$> (x Lude..?> "project") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProject where
  toJSON GetProject' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetProject where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProject where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get project request.
--
-- /See:/ 'mkGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { -- | The project to get information about.
    project :: Lude.Maybe Project,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProjectResponse' with the minimum fields required to make a request.
--
-- * 'project' - The project to get information about.
-- * 'responseStatus' - The response status code.
mkGetProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProjectResponse
mkGetProjectResponse pResponseStatus_ =
  GetProjectResponse'
    { project = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The project to get information about.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsProject :: Lens.Lens' GetProjectResponse (Lude.Maybe Project)
gprsProject = Lens.lens (project :: GetProjectResponse -> Lude.Maybe Project) (\s a -> s {project = a} :: GetProjectResponse)
{-# DEPRECATED gprsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetProjectResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProjectResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

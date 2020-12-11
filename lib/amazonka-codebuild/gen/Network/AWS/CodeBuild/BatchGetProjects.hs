{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetProjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more build projects.
module Network.AWS.CodeBuild.BatchGetProjects
  ( -- * Creating a request
    BatchGetProjects (..),
    mkBatchGetProjects,

    -- ** Request lenses
    bgpNames,

    -- * Destructuring the response
    BatchGetProjectsResponse (..),
    mkBatchGetProjectsResponse,

    -- ** Response lenses
    bgprsProjectsNotFound,
    bgprsProjects,
    bgprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetProjects' smart constructor.
newtype BatchGetProjects = BatchGetProjects'
  { names ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetProjects' with the minimum fields required to make a request.
--
-- * 'names' - The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
mkBatchGetProjects ::
  -- | 'names'
  Lude.NonEmpty Lude.Text ->
  BatchGetProjects
mkBatchGetProjects pNames_ = BatchGetProjects' {names = pNames_}

-- | The names or ARNs of the build projects. To get information about a project shared with your AWS account, its ARN must be specified. You cannot specify a shared project using its name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgpNames :: Lens.Lens' BatchGetProjects (Lude.NonEmpty Lude.Text)
bgpNames = Lens.lens (names :: BatchGetProjects -> Lude.NonEmpty Lude.Text) (\s a -> s {names = a} :: BatchGetProjects)
{-# DEPRECATED bgpNames "Use generic-lens or generic-optics with 'names' instead." #-}

instance Lude.AWSRequest BatchGetProjects where
  type Rs BatchGetProjects = BatchGetProjectsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetProjectsResponse'
            Lude.<$> (x Lude..?> "projectsNotFound")
            Lude.<*> (x Lude..?> "projects" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetProjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchGetProjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetProjects where
  toJSON BatchGetProjects' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("names" Lude..= names)])

instance Lude.ToPath BatchGetProjects where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetProjects where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetProjectsResponse' smart constructor.
data BatchGetProjectsResponse = BatchGetProjectsResponse'
  { projectsNotFound ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    projects :: Lude.Maybe [Project],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetProjectsResponse' with the minimum fields required to make a request.
--
-- * 'projects' - Information about the requested build projects.
-- * 'projectsNotFound' - The names of build projects for which information could not be found.
-- * 'responseStatus' - The response status code.
mkBatchGetProjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetProjectsResponse
mkBatchGetProjectsResponse pResponseStatus_ =
  BatchGetProjectsResponse'
    { projectsNotFound = Lude.Nothing,
      projects = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of build projects for which information could not be found.
--
-- /Note:/ Consider using 'projectsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsProjectsNotFound :: Lens.Lens' BatchGetProjectsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgprsProjectsNotFound = Lens.lens (projectsNotFound :: BatchGetProjectsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {projectsNotFound = a} :: BatchGetProjectsResponse)
{-# DEPRECATED bgprsProjectsNotFound "Use generic-lens or generic-optics with 'projectsNotFound' instead." #-}

-- | Information about the requested build projects.
--
-- /Note:/ Consider using 'projects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsProjects :: Lens.Lens' BatchGetProjectsResponse (Lude.Maybe [Project])
bgprsProjects = Lens.lens (projects :: BatchGetProjectsResponse -> Lude.Maybe [Project]) (\s a -> s {projects = a} :: BatchGetProjectsResponse)
{-# DEPRECATED bgprsProjects "Use generic-lens or generic-optics with 'projects' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgprsResponseStatus :: Lens.Lens' BatchGetProjectsResponse Lude.Int
bgprsResponseStatus = Lens.lens (responseStatus :: BatchGetProjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetProjectsResponse)
{-# DEPRECATED bgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

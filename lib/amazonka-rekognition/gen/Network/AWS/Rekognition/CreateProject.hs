{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels project. A project is a logical grouping of resources (images, Labels, models) and operations (training, evaluation and detection).
--
-- This operation requires permissions to perform the @rekognition:CreateProject@ action.
module Network.AWS.Rekognition.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpProjectName,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprsProjectARN,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProject' smart constructor.
newtype CreateProject = CreateProject'
  { -- | The name of the project to create.
    projectName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- * 'projectName' - The name of the project to create.
mkCreateProject ::
  -- | 'projectName'
  Lude.Text ->
  CreateProject
mkCreateProject pProjectName_ =
  CreateProject' {projectName = pProjectName_}

-- | The name of the project to create.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProjectName :: Lens.Lens' CreateProject Lude.Text
cpProjectName = Lens.lens (projectName :: CreateProject -> Lude.Text) (\s a -> s {projectName = a} :: CreateProject)
{-# DEPRECATED cpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Lude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Lude.<$> (x Lude..?> "ProjectArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.CreateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ProjectName" Lude..= projectName)])

instance Lude.ToPath CreateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
    projectARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- * 'projectARN' - The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
-- * 'responseStatus' - The response status code.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProjectResponse
mkCreateProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    { projectARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProjectARN :: Lens.Lens' CreateProjectResponse (Lude.Maybe Lude.Text)
cprsProjectARN = Lens.lens (projectARN :: CreateProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectARN = a} :: CreateProjectResponse)
{-# DEPRECATED cprsProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProjectResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

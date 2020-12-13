{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a project, including project resources. Does not delete users associated with the project, but does delete the IAM roles that allowed access to the project.
module Network.AWS.CodeStar.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpDeleteStack,
    dpId,
    dpClientRequestToken,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprsProjectARN,
    dprsStackId,
    dprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
    deleteStack :: Lude.Maybe Lude.Bool,
    -- | The ID of the project to be deleted in AWS CodeStar.
    id :: Lude.Text,
    -- | A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- * 'deleteStack' - Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
-- * 'id' - The ID of the project to be deleted in AWS CodeStar.
-- * 'clientRequestToken' - A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
mkDeleteProject ::
  -- | 'id'
  Lude.Text ->
  DeleteProject
mkDeleteProject pId_ =
  DeleteProject'
    { deleteStack = Lude.Nothing,
      id = pId_,
      clientRequestToken = Lude.Nothing
    }

-- | Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
--
-- /Note:/ Consider using 'deleteStack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDeleteStack :: Lens.Lens' DeleteProject (Lude.Maybe Lude.Bool)
dpDeleteStack = Lens.lens (deleteStack :: DeleteProject -> Lude.Maybe Lude.Bool) (\s a -> s {deleteStack = a} :: DeleteProject)
{-# DEPRECATED dpDeleteStack "Use generic-lens or generic-optics with 'deleteStack' instead." #-}

-- | The ID of the project to be deleted in AWS CodeStar.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DeleteProject Lude.Text
dpId = Lens.lens (id :: DeleteProject -> Lude.Text) (\s a -> s {id = a} :: DeleteProject)
{-# DEPRECATED dpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpClientRequestToken :: Lens.Lens' DeleteProject (Lude.Maybe Lude.Text)
dpClientRequestToken = Lens.lens (clientRequestToken :: DeleteProject -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: DeleteProject)
{-# DEPRECATED dpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Lude.<$> (x Lude..?> "projectArn")
            Lude.<*> (x Lude..?> "stackId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.DeleteProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deleteStack" Lude..=) Lude.<$> deleteStack,
            Lude.Just ("id" Lude..= id),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath DeleteProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted project.
    projectARN :: Lude.Maybe Lude.Text,
    -- | The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
    stackId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- * 'projectARN' - The Amazon Resource Name (ARN) of the deleted project.
-- * 'stackId' - The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
-- * 'responseStatus' - The response status code.
mkDeleteProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProjectResponse
mkDeleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { projectARN = Lude.Nothing,
      stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted project.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsProjectARN :: Lens.Lens' DeleteProjectResponse (Lude.Maybe Lude.Text)
dprsProjectARN = Lens.lens (projectARN :: DeleteProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectARN = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsStackId :: Lens.Lens' DeleteProjectResponse (Lude.Maybe Lude.Text)
dprsStackId = Lens.lens (stackId :: DeleteProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteProjectResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

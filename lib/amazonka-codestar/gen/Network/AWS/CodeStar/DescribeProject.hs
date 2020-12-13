{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
module Network.AWS.CodeStar.DescribeProject
  ( -- * Creating a request
    DescribeProject (..),
    mkDescribeProject,

    -- ** Request lenses
    dId,

    -- * Destructuring the response
    DescribeProjectResponse (..),
    mkDescribeProjectResponse,

    -- ** Response lenses
    drsStatus,
    drsArn,
    drsProjectTemplateId,
    drsName,
    drsId,
    drsStackId,
    drsClientRequestToken,
    drsCreatedTimeStamp,
    drsDescription,
    drsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeProject' smart constructor.
newtype DescribeProject = DescribeProject'
  { -- | The ID of the project.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProject' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the project.
mkDescribeProject ::
  -- | 'id'
  Lude.Text ->
  DescribeProject
mkDescribeProject pId_ = DescribeProject' {id = pId_}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeProject Lude.Text
dId = Lens.lens (id :: DescribeProject -> Lude.Text) (\s a -> s {id = a} :: DescribeProject)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeProject where
  type Rs DescribeProject = DescribeProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "projectTemplateId")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "stackId")
            Lude.<*> (x Lude..?> "clientRequestToken")
            Lude.<*> (x Lude..?> "createdTimeStamp")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.DescribeProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProject where
  toJSON DescribeProject' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("id" Lude..= id)])

instance Lude.ToPath DescribeProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The project creation or deletion status.
    status :: Lude.Maybe ProjectStatus,
    -- | The Amazon Resource Name (ARN) for the project.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID for the AWS CodeStar project template used to create the project.
    projectTemplateId :: Lude.Maybe Lude.Text,
    -- | The display name for the project.
    name :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The ID of the project.
    id :: Lude.Maybe Lude.Text,
    -- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
    stackId :: Lude.Maybe Lude.Text,
    -- | A user- or system-generated token that identifies the entity that requested project creation.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The date and time the project was created, in timestamp format.
    createdTimeStamp :: Lude.Maybe Lude.Timestamp,
    -- | The description of the project, if any.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProjectResponse' with the minimum fields required to make a request.
--
-- * 'status' - The project creation or deletion status.
-- * 'arn' - The Amazon Resource Name (ARN) for the project.
-- * 'projectTemplateId' - The ID for the AWS CodeStar project template used to create the project.
-- * 'name' - The display name for the project.
-- * 'id' - The ID of the project.
-- * 'stackId' - The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
-- * 'clientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation.
-- * 'createdTimeStamp' - The date and time the project was created, in timestamp format.
-- * 'description' - The description of the project, if any.
-- * 'responseStatus' - The response status code.
mkDescribeProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProjectResponse
mkDescribeProjectResponse pResponseStatus_ =
  DescribeProjectResponse'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      projectTemplateId = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      stackId = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      createdTimeStamp = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The project creation or deletion status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatus :: Lens.Lens' DescribeProjectResponse (Lude.Maybe ProjectStatus)
drsStatus = Lens.lens (status :: DescribeProjectResponse -> Lude.Maybe ProjectStatus) (\s a -> s {status = a} :: DescribeProjectResponse)
{-# DEPRECATED drsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) for the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsArn :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Text)
drsArn = Lens.lens (arn :: DescribeProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeProjectResponse)
{-# DEPRECATED drsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID for the AWS CodeStar project template used to create the project.
--
-- /Note:/ Consider using 'projectTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProjectTemplateId :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Text)
drsProjectTemplateId = Lens.lens (projectTemplateId :: DescribeProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectTemplateId = a} :: DescribeProjectResponse)
{-# DEPRECATED drsProjectTemplateId "Use generic-lens or generic-optics with 'projectTemplateId' instead." #-}

-- | The display name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsName :: Lens.Lens' DescribeProjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
drsName = Lens.lens (name :: DescribeProjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {name = a} :: DescribeProjectResponse)
{-# DEPRECATED drsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsId :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Text)
drsId = Lens.lens (id :: DescribeProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeProjectResponse)
{-# DEPRECATED drsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the primary stack in AWS CloudFormation used to generate resources for the project.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStackId :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Text)
drsStackId = Lens.lens (stackId :: DescribeProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeProjectResponse)
{-# DEPRECATED drsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project creation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsClientRequestToken :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Text)
drsClientRequestToken = Lens.lens (clientRequestToken :: DescribeProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: DescribeProjectResponse)
{-# DEPRECATED drsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The date and time the project was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimeStamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCreatedTimeStamp :: Lens.Lens' DescribeProjectResponse (Lude.Maybe Lude.Timestamp)
drsCreatedTimeStamp = Lens.lens (createdTimeStamp :: DescribeProjectResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimeStamp = a} :: DescribeProjectResponse)
{-# DEPRECATED drsCreatedTimeStamp "Use generic-lens or generic-optics with 'createdTimeStamp' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeProjectResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
drsDescription = Lens.lens (description :: DescribeProjectResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: DescribeProjectResponse)
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProjectResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProjectResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project, including project resources. This action creates a project based on a submitted project request. A set of source code files and a toolchain template file can be included with the project request. If these are not provided, an empty project is created.
module Network.AWS.CodeStar.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpSourceCode,
    cpToolchain,
    cpClientRequestToken,
    cpDescription,
    cpTags,
    cpName,
    cpId,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprsProjectTemplateId,
    cprsClientRequestToken,
    cprsResponseStatus,
    cprsId,
    cprsArn,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { sourceCode ::
      Lude.Maybe [Code],
    toolchain :: Lude.Maybe Toolchain,
    clientRequestToken :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Sensitive Lude.Text,
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
-- * 'description' - The description of the project, if any.
-- * 'id' - The ID of the project to be created in AWS CodeStar.
-- * 'name' - The display name for the project to be created in AWS CodeStar.
-- * 'sourceCode' - A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
-- * 'tags' - The tags created for the project.
-- * 'toolchain' - The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
mkCreateProject ::
  -- | 'name'
  Lude.Sensitive Lude.Text ->
  -- | 'id'
  Lude.Text ->
  CreateProject
mkCreateProject pName_ pId_ =
  CreateProject'
    { sourceCode = Lude.Nothing,
      toolchain = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      id = pId_
    }

-- | A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
--
-- /Note:/ Consider using 'sourceCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSourceCode :: Lens.Lens' CreateProject (Lude.Maybe [Code])
cpSourceCode = Lens.lens (sourceCode :: CreateProject -> Lude.Maybe [Code]) (\s a -> s {sourceCode = a} :: CreateProject)
{-# DEPRECATED cpSourceCode "Use generic-lens or generic-optics with 'sourceCode' instead." #-}

-- | The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
--
-- /Note:/ Consider using 'toolchain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpToolchain :: Lens.Lens' CreateProject (Lude.Maybe Toolchain)
cpToolchain = Lens.lens (toolchain :: CreateProject -> Lude.Maybe Toolchain) (\s a -> s {toolchain = a} :: CreateProject)
{-# DEPRECATED cpToolchain "Use generic-lens or generic-optics with 'toolchain' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpClientRequestToken :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpClientRequestToken = Lens.lens (clientRequestToken :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateProject)
{-# DEPRECATED cpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description of the project, if any.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProject (Lude.Maybe (Lude.Sensitive Lude.Text))
cpDescription = Lens.lens (description :: CreateProject -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: CreateProject)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags created for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProject (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cpTags = Lens.lens (tags :: CreateProject -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateProject)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The display name for the project to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject (Lude.Sensitive Lude.Text)
cpName = Lens.lens (name :: CreateProject -> Lude.Sensitive Lude.Text) (\s a -> s {name = a} :: CreateProject)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the project to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpId :: Lens.Lens' CreateProject Lude.Text
cpId = Lens.lens (id :: CreateProject -> Lude.Text) (\s a -> s {id = a} :: CreateProject)
{-# DEPRECATED cpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Lude.<$> (x Lude..?> "projectTemplateId")
            Lude.<*> (x Lude..?> "clientRequestToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "id")
            Lude.<*> (x Lude..:> "arn")
      )

instance Lude.ToHeaders CreateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.CreateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceCode" Lude..=) Lude.<$> sourceCode,
            ("toolchain" Lude..=) Lude.<$> toolchain,
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("id" Lude..= id)
          ]
      )

instance Lude.ToPath CreateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { projectTemplateId ::
      Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    id :: Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the created project.
-- * 'clientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation.
-- * 'id' - The ID of the project.
-- * 'projectTemplateId' - Reserved for future use.
-- * 'responseStatus' - The response status code.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  CreateProjectResponse
mkCreateProjectResponse pResponseStatus_ pId_ pArn_ =
  CreateProjectResponse'
    { projectTemplateId = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      id = pId_,
      arn = pArn_
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'projectTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProjectTemplateId :: Lens.Lens' CreateProjectResponse (Lude.Maybe Lude.Text)
cprsProjectTemplateId = Lens.lens (projectTemplateId :: CreateProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {projectTemplateId = a} :: CreateProjectResponse)
{-# DEPRECATED cprsProjectTemplateId "Use generic-lens or generic-optics with 'projectTemplateId' instead." #-}

-- | A user- or system-generated token that identifies the entity that requested project creation.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsClientRequestToken :: Lens.Lens' CreateProjectResponse (Lude.Maybe Lude.Text)
cprsClientRequestToken = Lens.lens (clientRequestToken :: CreateProjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateProjectResponse)
{-# DEPRECATED cprsClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProjectResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsId :: Lens.Lens' CreateProjectResponse Lude.Text
cprsId = Lens.lens (id :: CreateProjectResponse -> Lude.Text) (\s a -> s {id = a} :: CreateProjectResponse)
{-# DEPRECATED cprsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the created project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsArn :: Lens.Lens' CreateProjectResponse Lude.Text
cprsArn = Lens.lens (arn :: CreateProjectResponse -> Lude.Text) (\s a -> s {arn = a} :: CreateProjectResponse)
{-# DEPRECATED cprsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.CreateWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, enables AWS CodeBuild to start rebuilding the source code every time a code change is pushed to the repository.
--
-- /Important:/ If you enable webhooks for an AWS CodeBuild project, and the project is used as a build step in AWS CodePipeline, then two identical builds are created for each commit. One build is triggered through webhooks, and one through AWS CodePipeline. Because billing is on a per-build basis, you are billed for both builds. Therefore, if you are using AWS CodePipeline, we recommend that you disable webhooks in AWS CodeBuild. In the AWS CodeBuild console, clear the Webhook box. For more information, see step 5 in <https://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console Change a Build Project's Settings> .
module Network.AWS.CodeBuild.CreateWebhook
  ( -- * Creating a request
    CreateWebhook (..),
    mkCreateWebhook,

    -- ** Request lenses
    cwBranchFilter,
    cwFilterGroups,
    cwProjectName,
    cwBuildType,

    -- * Destructuring the response
    CreateWebhookResponse (..),
    mkCreateWebhookResponse,

    -- ** Response lenses
    cwrsWebhook,
    cwrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateWebhook' smart constructor.
data CreateWebhook = CreateWebhook'
  { -- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
    branchFilter :: Lude.Maybe Lude.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
    --
    -- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
    filterGroups :: Lude.Maybe [[WebhookFilter]],
    -- | The name of the AWS CodeBuild project.
    projectName :: Lude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Lude.Maybe WebhookBuildType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWebhook' with the minimum fields required to make a request.
--
-- * 'branchFilter' - A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
-- * 'filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
--
-- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
-- * 'projectName' - The name of the AWS CodeBuild project.
-- * 'buildType' - Specifies the type of build this webhook will trigger.
mkCreateWebhook ::
  -- | 'projectName'
  Lude.Text ->
  CreateWebhook
mkCreateWebhook pProjectName_ =
  CreateWebhook'
    { branchFilter = Lude.Nothing,
      filterGroups = Lude.Nothing,
      projectName = pProjectName_,
      buildType = Lude.Nothing
    }

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- /Note:/ Consider using 'branchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwBranchFilter :: Lens.Lens' CreateWebhook (Lude.Maybe Lude.Text)
cwBranchFilter = Lens.lens (branchFilter :: CreateWebhook -> Lude.Maybe Lude.Text) (\s a -> s {branchFilter = a} :: CreateWebhook)
{-# DEPRECATED cwBranchFilter "Use generic-lens or generic-optics with 'branchFilter' instead." #-}

-- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
--
-- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
--
-- /Note:/ Consider using 'filterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwFilterGroups :: Lens.Lens' CreateWebhook (Lude.Maybe [[WebhookFilter]])
cwFilterGroups = Lens.lens (filterGroups :: CreateWebhook -> Lude.Maybe [[WebhookFilter]]) (\s a -> s {filterGroups = a} :: CreateWebhook)
{-# DEPRECATED cwFilterGroups "Use generic-lens or generic-optics with 'filterGroups' instead." #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwProjectName :: Lens.Lens' CreateWebhook Lude.Text
cwProjectName = Lens.lens (projectName :: CreateWebhook -> Lude.Text) (\s a -> s {projectName = a} :: CreateWebhook)
{-# DEPRECATED cwProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | Specifies the type of build this webhook will trigger.
--
-- /Note:/ Consider using 'buildType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwBuildType :: Lens.Lens' CreateWebhook (Lude.Maybe WebhookBuildType)
cwBuildType = Lens.lens (buildType :: CreateWebhook -> Lude.Maybe WebhookBuildType) (\s a -> s {buildType = a} :: CreateWebhook)
{-# DEPRECATED cwBuildType "Use generic-lens or generic-optics with 'buildType' instead." #-}

instance Lude.AWSRequest CreateWebhook where
  type Rs CreateWebhook = CreateWebhookResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWebhookResponse'
            Lude.<$> (x Lude..?> "webhook") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWebhook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.CreateWebhook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWebhook where
  toJSON CreateWebhook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("branchFilter" Lude..=) Lude.<$> branchFilter,
            ("filterGroups" Lude..=) Lude.<$> filterGroups,
            Lude.Just ("projectName" Lude..= projectName),
            ("buildType" Lude..=) Lude.<$> buildType
          ]
      )

instance Lude.ToPath CreateWebhook where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWebhook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWebhookResponse' smart constructor.
data CreateWebhookResponse = CreateWebhookResponse'
  { -- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
    webhook :: Lude.Maybe Webhook,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWebhookResponse' with the minimum fields required to make a request.
--
-- * 'webhook' - Information about a webhook that connects repository events to a build project in AWS CodeBuild.
-- * 'responseStatus' - The response status code.
mkCreateWebhookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWebhookResponse
mkCreateWebhookResponse pResponseStatus_ =
  CreateWebhookResponse'
    { webhook = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- /Note:/ Consider using 'webhook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsWebhook :: Lens.Lens' CreateWebhookResponse (Lude.Maybe Webhook)
cwrsWebhook = Lens.lens (webhook :: CreateWebhookResponse -> Lude.Maybe Webhook) (\s a -> s {webhook = a} :: CreateWebhookResponse)
{-# DEPRECATED cwrsWebhook "Use generic-lens or generic-optics with 'webhook' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrsResponseStatus :: Lens.Lens' CreateWebhookResponse Lude.Int
cwrsResponseStatus = Lens.lens (responseStatus :: CreateWebhookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWebhookResponse)
{-# DEPRECATED cwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

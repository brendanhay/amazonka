{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the webhook associated with an AWS CodeBuild build project.
module Network.AWS.CodeBuild.UpdateWebhook
  ( -- * Creating a request
    UpdateWebhook (..),
    mkUpdateWebhook,

    -- ** Request lenses
    uwBranchFilter,
    uwRotateSecret,
    uwFilterGroups,
    uwProjectName,
    uwBuildType,

    -- * Destructuring the response
    UpdateWebhookResponse (..),
    mkUpdateWebhookResponse,

    -- ** Response lenses
    uwrsWebhook,
    uwrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateWebhook' smart constructor.
data UpdateWebhook = UpdateWebhook'
  { -- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
    branchFilter :: Lude.Maybe Lude.Text,
    -- | A boolean value that specifies whether the associated GitHub repository's secret token should be updated. If you use Bitbucket for your repository, @rotateSecret@ is ignored.
    rotateSecret :: Lude.Maybe Lude.Bool,
    -- | An array of arrays of @WebhookFilter@ objects used to determine if a webhook event can trigger a build. A filter group must contain at least one @EVENT@ @WebhookFilter@ .
    filterGroups :: Lude.Maybe [[WebhookFilter]],
    -- | The name of the AWS CodeBuild project.
    projectName :: Lude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Lude.Maybe WebhookBuildType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWebhook' with the minimum fields required to make a request.
--
-- * 'branchFilter' - A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
-- * 'rotateSecret' - A boolean value that specifies whether the associated GitHub repository's secret token should be updated. If you use Bitbucket for your repository, @rotateSecret@ is ignored.
-- * 'filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine if a webhook event can trigger a build. A filter group must contain at least one @EVENT@ @WebhookFilter@ .
-- * 'projectName' - The name of the AWS CodeBuild project.
-- * 'buildType' - Specifies the type of build this webhook will trigger.
mkUpdateWebhook ::
  -- | 'projectName'
  Lude.Text ->
  UpdateWebhook
mkUpdateWebhook pProjectName_ =
  UpdateWebhook'
    { branchFilter = Lude.Nothing,
      rotateSecret = Lude.Nothing,
      filterGroups = Lude.Nothing,
      projectName = pProjectName_,
      buildType = Lude.Nothing
    }

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- /Note:/ Consider using 'branchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwBranchFilter :: Lens.Lens' UpdateWebhook (Lude.Maybe Lude.Text)
uwBranchFilter = Lens.lens (branchFilter :: UpdateWebhook -> Lude.Maybe Lude.Text) (\s a -> s {branchFilter = a} :: UpdateWebhook)
{-# DEPRECATED uwBranchFilter "Use generic-lens or generic-optics with 'branchFilter' instead." #-}

-- | A boolean value that specifies whether the associated GitHub repository's secret token should be updated. If you use Bitbucket for your repository, @rotateSecret@ is ignored.
--
-- /Note:/ Consider using 'rotateSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwRotateSecret :: Lens.Lens' UpdateWebhook (Lude.Maybe Lude.Bool)
uwRotateSecret = Lens.lens (rotateSecret :: UpdateWebhook -> Lude.Maybe Lude.Bool) (\s a -> s {rotateSecret = a} :: UpdateWebhook)
{-# DEPRECATED uwRotateSecret "Use generic-lens or generic-optics with 'rotateSecret' instead." #-}

-- | An array of arrays of @WebhookFilter@ objects used to determine if a webhook event can trigger a build. A filter group must contain at least one @EVENT@ @WebhookFilter@ .
--
-- /Note:/ Consider using 'filterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwFilterGroups :: Lens.Lens' UpdateWebhook (Lude.Maybe [[WebhookFilter]])
uwFilterGroups = Lens.lens (filterGroups :: UpdateWebhook -> Lude.Maybe [[WebhookFilter]]) (\s a -> s {filterGroups = a} :: UpdateWebhook)
{-# DEPRECATED uwFilterGroups "Use generic-lens or generic-optics with 'filterGroups' instead." #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwProjectName :: Lens.Lens' UpdateWebhook Lude.Text
uwProjectName = Lens.lens (projectName :: UpdateWebhook -> Lude.Text) (\s a -> s {projectName = a} :: UpdateWebhook)
{-# DEPRECATED uwProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | Specifies the type of build this webhook will trigger.
--
-- /Note:/ Consider using 'buildType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwBuildType :: Lens.Lens' UpdateWebhook (Lude.Maybe WebhookBuildType)
uwBuildType = Lens.lens (buildType :: UpdateWebhook -> Lude.Maybe WebhookBuildType) (\s a -> s {buildType = a} :: UpdateWebhook)
{-# DEPRECATED uwBuildType "Use generic-lens or generic-optics with 'buildType' instead." #-}

instance Lude.AWSRequest UpdateWebhook where
  type Rs UpdateWebhook = UpdateWebhookResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateWebhookResponse'
            Lude.<$> (x Lude..?> "webhook") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateWebhook where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.UpdateWebhook" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWebhook where
  toJSON UpdateWebhook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("branchFilter" Lude..=) Lude.<$> branchFilter,
            ("rotateSecret" Lude..=) Lude.<$> rotateSecret,
            ("filterGroups" Lude..=) Lude.<$> filterGroups,
            Lude.Just ("projectName" Lude..= projectName),
            ("buildType" Lude..=) Lude.<$> buildType
          ]
      )

instance Lude.ToPath UpdateWebhook where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWebhook where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWebhookResponse' smart constructor.
data UpdateWebhookResponse = UpdateWebhookResponse'
  { -- | Information about a repository's webhook that is associated with a project in AWS CodeBuild.
    webhook :: Lude.Maybe Webhook,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWebhookResponse' with the minimum fields required to make a request.
--
-- * 'webhook' - Information about a repository's webhook that is associated with a project in AWS CodeBuild.
-- * 'responseStatus' - The response status code.
mkUpdateWebhookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateWebhookResponse
mkUpdateWebhookResponse pResponseStatus_ =
  UpdateWebhookResponse'
    { webhook = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a repository's webhook that is associated with a project in AWS CodeBuild.
--
-- /Note:/ Consider using 'webhook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsWebhook :: Lens.Lens' UpdateWebhookResponse (Lude.Maybe Webhook)
uwrsWebhook = Lens.lens (webhook :: UpdateWebhookResponse -> Lude.Maybe Webhook) (\s a -> s {webhook = a} :: UpdateWebhookResponse)
{-# DEPRECATED uwrsWebhook "Use generic-lens or generic-optics with 'webhook' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsResponseStatus :: Lens.Lens' UpdateWebhookResponse Lude.Int
uwrsResponseStatus = Lens.lens (responseStatus :: UpdateWebhookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWebhookResponse)
{-# DEPRECATED uwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    cwProjectName,
    cwBranchFilter,
    cwBuildType,
    cwFilterGroups,

    -- * Destructuring the response
    CreateWebhookResponse (..),
    mkCreateWebhookResponse,

    -- ** Response lenses
    cwrrsWebhook,
    cwrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateWebhook' smart constructor.
data CreateWebhook = CreateWebhook'
  { -- | The name of the AWS CodeBuild project.
    projectName :: Types.ProjectName,
    -- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
    branchFilter :: Core.Maybe Types.String,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Core.Maybe Types.WebhookBuildType,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
    --
    -- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
    filterGroups :: Core.Maybe [[Types.WebhookFilter]]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWebhook' value with any optional fields omitted.
mkCreateWebhook ::
  -- | 'projectName'
  Types.ProjectName ->
  CreateWebhook
mkCreateWebhook projectName =
  CreateWebhook'
    { projectName,
      branchFilter = Core.Nothing,
      buildType = Core.Nothing,
      filterGroups = Core.Nothing
    }

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwProjectName :: Lens.Lens' CreateWebhook Types.ProjectName
cwProjectName = Lens.field @"projectName"
{-# DEPRECATED cwProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- /Note:/ Consider using 'branchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwBranchFilter :: Lens.Lens' CreateWebhook (Core.Maybe Types.String)
cwBranchFilter = Lens.field @"branchFilter"
{-# DEPRECATED cwBranchFilter "Use generic-lens or generic-optics with 'branchFilter' instead." #-}

-- | Specifies the type of build this webhook will trigger.
--
-- /Note:/ Consider using 'buildType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwBuildType :: Lens.Lens' CreateWebhook (Core.Maybe Types.WebhookBuildType)
cwBuildType = Lens.field @"buildType"
{-# DEPRECATED cwBuildType "Use generic-lens or generic-optics with 'buildType' instead." #-}

-- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
--
-- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
--
-- /Note:/ Consider using 'filterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwFilterGroups :: Lens.Lens' CreateWebhook (Core.Maybe [[Types.WebhookFilter]])
cwFilterGroups = Lens.field @"filterGroups"
{-# DEPRECATED cwFilterGroups "Use generic-lens or generic-optics with 'filterGroups' instead." #-}

instance Core.FromJSON CreateWebhook where
  toJSON CreateWebhook {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectName" Core..= projectName),
            ("branchFilter" Core..=) Core.<$> branchFilter,
            ("buildType" Core..=) Core.<$> buildType,
            ("filterGroups" Core..=) Core.<$> filterGroups
          ]
      )

instance Core.AWSRequest CreateWebhook where
  type Rs CreateWebhook = CreateWebhookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.CreateWebhook")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebhookResponse'
            Core.<$> (x Core..:? "webhook") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateWebhookResponse' smart constructor.
data CreateWebhookResponse = CreateWebhookResponse'
  { -- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
    webhook :: Core.Maybe Types.Webhook,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateWebhookResponse' value with any optional fields omitted.
mkCreateWebhookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateWebhookResponse
mkCreateWebhookResponse responseStatus =
  CreateWebhookResponse' {webhook = Core.Nothing, responseStatus}

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- /Note:/ Consider using 'webhook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsWebhook :: Lens.Lens' CreateWebhookResponse (Core.Maybe Types.Webhook)
cwrrsWebhook = Lens.field @"webhook"
{-# DEPRECATED cwrrsWebhook "Use generic-lens or generic-optics with 'webhook' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsResponseStatus :: Lens.Lens' CreateWebhookResponse Core.Int
cwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

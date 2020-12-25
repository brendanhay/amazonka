{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.CodeDeploy.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caApplicationName,
    caComputePlatform,
    caTags,

    -- * Destructuring the response
    CreateApplicationResponse (..),
    mkCreateApplicationResponse,

    -- ** Response lenses
    carrsApplicationId,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateApplication@ operation.
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The name of the application. This name must be unique with the applicable IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
    computePlatform :: Core.Maybe Types.ComputePlatform,
    -- | The metadata that you apply to CodeDeploy applications to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication ::
  -- | 'applicationName'
  Types.ApplicationName ->
  CreateApplication
mkCreateApplication applicationName =
  CreateApplication'
    { applicationName,
      computePlatform = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the application. This name must be unique with the applicable IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Types.ApplicationName
caApplicationName = Lens.field @"applicationName"
{-# DEPRECATED caApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caComputePlatform :: Lens.Lens' CreateApplication (Core.Maybe Types.ComputePlatform)
caComputePlatform = Lens.field @"computePlatform"
{-# DEPRECATED caComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | The metadata that you apply to CodeDeploy applications to help you organize and categorize them. Each tag consists of a key and an optional value, both of which you define.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateApplication where
  toJSON CreateApplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            ("computePlatform" Core..=) Core.<$> computePlatform,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeDeploy_20141006.CreateApplication")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreateApplication@ operation.
--
-- /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | A unique application ID.
    applicationId :: Core.Maybe Types.ApplicationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationResponse' value with any optional fields omitted.
mkCreateApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateApplicationResponse
mkCreateApplicationResponse responseStatus =
  CreateApplicationResponse'
    { applicationId = Core.Nothing,
      responseStatus
    }

-- | A unique application ID.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsApplicationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Types.ApplicationId)
carrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED carrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateApplicationResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

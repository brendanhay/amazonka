{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about triggers configured for a repository.
module Network.AWS.CodeCommit.GetRepositoryTriggers
  ( -- * Creating a request
    GetRepositoryTriggers (..),
    mkGetRepositoryTriggers,

    -- ** Request lenses
    grtRepositoryName,

    -- * Destructuring the response
    GetRepositoryTriggersResponse (..),
    mkGetRepositoryTriggersResponse,

    -- ** Response lenses
    grtrrsConfigurationId,
    grtrrsTriggers,
    grtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get repository triggers operation.
--
-- /See:/ 'mkGetRepositoryTriggers' smart constructor.
newtype GetRepositoryTriggers = GetRepositoryTriggers'
  { -- | The name of the repository for which the trigger is configured.
    repositoryName :: Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryTriggers' value with any optional fields omitted.
mkGetRepositoryTriggers ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  GetRepositoryTriggers
mkGetRepositoryTriggers repositoryName =
  GetRepositoryTriggers' {repositoryName}

-- | The name of the repository for which the trigger is configured.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtRepositoryName :: Lens.Lens' GetRepositoryTriggers Types.RepositoryName
grtRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED grtRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON GetRepositoryTriggers where
  toJSON GetRepositoryTriggers {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("repositoryName" Core..= repositoryName)]
      )

instance Core.AWSRequest GetRepositoryTriggers where
  type Rs GetRepositoryTriggers = GetRepositoryTriggersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.GetRepositoryTriggers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryTriggersResponse'
            Core.<$> (x Core..:? "configurationId")
            Core.<*> (x Core..:? "triggers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a get repository triggers operation.
--
-- /See:/ 'mkGetRepositoryTriggersResponse' smart constructor.
data GetRepositoryTriggersResponse = GetRepositoryTriggersResponse'
  { -- | The system-generated unique ID for the trigger.
    configurationId :: Core.Maybe Types.ConfigurationId,
    -- | The JSON block of configuration information for each trigger.
    triggers :: Core.Maybe [Types.RepositoryTrigger],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRepositoryTriggersResponse' value with any optional fields omitted.
mkGetRepositoryTriggersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRepositoryTriggersResponse
mkGetRepositoryTriggersResponse responseStatus =
  GetRepositoryTriggersResponse'
    { configurationId = Core.Nothing,
      triggers = Core.Nothing,
      responseStatus
    }

-- | The system-generated unique ID for the trigger.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrrsConfigurationId :: Lens.Lens' GetRepositoryTriggersResponse (Core.Maybe Types.ConfigurationId)
grtrrsConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED grtrrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The JSON block of configuration information for each trigger.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrrsTriggers :: Lens.Lens' GetRepositoryTriggersResponse (Core.Maybe [Types.RepositoryTrigger])
grtrrsTriggers = Lens.field @"triggers"
{-# DEPRECATED grtrrsTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grtrrsResponseStatus :: Lens.Lens' GetRepositoryTriggersResponse Core.Int
grtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

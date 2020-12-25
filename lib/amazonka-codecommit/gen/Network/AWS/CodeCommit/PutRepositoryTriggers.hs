{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutRepositoryTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all triggers for a repository. Used to create or delete triggers.
module Network.AWS.CodeCommit.PutRepositoryTriggers
  ( -- * Creating a request
    PutRepositoryTriggers (..),
    mkPutRepositoryTriggers,

    -- ** Request lenses
    pRepositoryName,
    pTriggers,

    -- * Destructuring the response
    PutRepositoryTriggersResponse (..),
    mkPutRepositoryTriggersResponse,

    -- ** Response lenses
    prtrrsConfigurationId,
    prtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a put repository triggers operation.
--
-- /See:/ 'mkPutRepositoryTriggers' smart constructor.
data PutRepositoryTriggers = PutRepositoryTriggers'
  { -- | The name of the repository where you want to create or update the trigger.
    repositoryName :: Types.RepositoryName,
    -- | The JSON block of configuration information for each trigger.
    triggers :: [Types.RepositoryTrigger]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRepositoryTriggers' value with any optional fields omitted.
mkPutRepositoryTriggers ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  PutRepositoryTriggers
mkPutRepositoryTriggers repositoryName =
  PutRepositoryTriggers' {repositoryName, triggers = Core.mempty}

-- | The name of the repository where you want to create or update the trigger.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRepositoryName :: Lens.Lens' PutRepositoryTriggers Types.RepositoryName
pRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED pRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The JSON block of configuration information for each trigger.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTriggers :: Lens.Lens' PutRepositoryTriggers [Types.RepositoryTrigger]
pTriggers = Lens.field @"triggers"
{-# DEPRECATED pTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

instance Core.FromJSON PutRepositoryTriggers where
  toJSON PutRepositoryTriggers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("triggers" Core..= triggers)
          ]
      )

instance Core.AWSRequest PutRepositoryTriggers where
  type Rs PutRepositoryTriggers = PutRepositoryTriggersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.PutRepositoryTriggers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRepositoryTriggersResponse'
            Core.<$> (x Core..:? "configurationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a put repository triggers operation.
--
-- /See:/ 'mkPutRepositoryTriggersResponse' smart constructor.
data PutRepositoryTriggersResponse = PutRepositoryTriggersResponse'
  { -- | The system-generated unique ID for the create or update operation.
    configurationId :: Core.Maybe Types.ConfigurationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRepositoryTriggersResponse' value with any optional fields omitted.
mkPutRepositoryTriggersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutRepositoryTriggersResponse
mkPutRepositoryTriggersResponse responseStatus =
  PutRepositoryTriggersResponse'
    { configurationId = Core.Nothing,
      responseStatus
    }

-- | The system-generated unique ID for the create or update operation.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtrrsConfigurationId :: Lens.Lens' PutRepositoryTriggersResponse (Core.Maybe Types.ConfigurationId)
prtrrsConfigurationId = Lens.field @"configurationId"
{-# DEPRECATED prtrrsConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prtrrsResponseStatus :: Lens.Lens' PutRepositoryTriggersResponse Core.Int
prtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

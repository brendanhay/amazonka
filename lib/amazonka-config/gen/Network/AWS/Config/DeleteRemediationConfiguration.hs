{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteRemediationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the remediation configuration.
module Network.AWS.Config.DeleteRemediationConfiguration
  ( -- * Creating a request
    DeleteRemediationConfiguration (..),
    mkDeleteRemediationConfiguration,

    -- ** Request lenses
    drcfConfigRuleName,
    drcfResourceType,

    -- * Destructuring the response
    DeleteRemediationConfigurationResponse (..),
    mkDeleteRemediationConfigurationResponse,

    -- ** Response lenses
    drcrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRemediationConfiguration' smart constructor.
data DeleteRemediationConfiguration = DeleteRemediationConfiguration'
  { -- | The name of the AWS Config rule for which you want to delete remediation configuration.
    configRuleName :: Types.ConfigRuleName,
    -- | The type of a resource.
    resourceType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemediationConfiguration' value with any optional fields omitted.
mkDeleteRemediationConfiguration ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  DeleteRemediationConfiguration
mkDeleteRemediationConfiguration configRuleName =
  DeleteRemediationConfiguration'
    { configRuleName,
      resourceType = Core.Nothing
    }

-- | The name of the AWS Config rule for which you want to delete remediation configuration.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcfConfigRuleName :: Lens.Lens' DeleteRemediationConfiguration Types.ConfigRuleName
drcfConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED drcfConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcfResourceType :: Lens.Lens' DeleteRemediationConfiguration (Core.Maybe Types.String)
drcfResourceType = Lens.field @"resourceType"
{-# DEPRECATED drcfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON DeleteRemediationConfiguration where
  toJSON DeleteRemediationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            ("ResourceType" Core..=) Core.<$> resourceType
          ]
      )

instance Core.AWSRequest DeleteRemediationConfiguration where
  type
    Rs DeleteRemediationConfiguration =
      DeleteRemediationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DeleteRemediationConfiguration"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemediationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRemediationConfigurationResponse' smart constructor.
newtype DeleteRemediationConfigurationResponse = DeleteRemediationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRemediationConfigurationResponse' value with any optional fields omitted.
mkDeleteRemediationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRemediationConfigurationResponse
mkDeleteRemediationConfigurationResponse responseStatus =
  DeleteRemediationConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DeleteRemediationConfigurationResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

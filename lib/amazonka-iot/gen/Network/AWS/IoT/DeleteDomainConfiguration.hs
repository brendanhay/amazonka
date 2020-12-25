{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain configuration.
module Network.AWS.IoT.DeleteDomainConfiguration
  ( -- * Creating a request
    DeleteDomainConfiguration (..),
    mkDeleteDomainConfiguration,

    -- ** Request lenses
    dDomainConfigurationName,

    -- * Destructuring the response
    DeleteDomainConfigurationResponse (..),
    mkDeleteDomainConfigurationResponse,

    -- ** Response lenses
    ddcrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDomainConfiguration' smart constructor.
newtype DeleteDomainConfiguration = DeleteDomainConfiguration'
  { -- | The name of the domain configuration to be deleted.
    domainConfigurationName :: Types.DomainConfigurationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainConfiguration' value with any optional fields omitted.
mkDeleteDomainConfiguration ::
  -- | 'domainConfigurationName'
  Types.DomainConfigurationName ->
  DeleteDomainConfiguration
mkDeleteDomainConfiguration domainConfigurationName =
  DeleteDomainConfiguration' {domainConfigurationName}

-- | The name of the domain configuration to be deleted.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainConfigurationName :: Lens.Lens' DeleteDomainConfiguration Types.DomainConfigurationName
dDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# DEPRECATED dDomainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead." #-}

instance Core.AWSRequest DeleteDomainConfiguration where
  type
    Rs DeleteDomainConfiguration =
      DeleteDomainConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/domainConfigurations/"
                Core.<> (Core.toText domainConfigurationName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDomainConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDomainConfigurationResponse' smart constructor.
newtype DeleteDomainConfigurationResponse = DeleteDomainConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainConfigurationResponse' value with any optional fields omitted.
mkDeleteDomainConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDomainConfigurationResponse
mkDeleteDomainConfigurationResponse responseStatus =
  DeleteDomainConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrfrsResponseStatus :: Lens.Lens' DeleteDomainConfigurationResponse Core.Int
ddcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.AssociateConfigurationItemsToApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more configuration items with an application.
module Network.AWS.Discovery.AssociateConfigurationItemsToApplication
  ( -- * Creating a request
    AssociateConfigurationItemsToApplication (..),
    mkAssociateConfigurationItemsToApplication,

    -- ** Request lenses
    acitaApplicationConfigurationId,
    acitaConfigurationIds,

    -- * Destructuring the response
    AssociateConfigurationItemsToApplicationResponse (..),
    mkAssociateConfigurationItemsToApplicationResponse,

    -- ** Response lenses
    acitarrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateConfigurationItemsToApplication' smart constructor.
data AssociateConfigurationItemsToApplication = AssociateConfigurationItemsToApplication'
  { -- | The configuration ID of an application with which items are to be associated.
    applicationConfigurationId :: Types.ApplicationId,
    -- | The ID of each configuration item to be associated with an application.
    configurationIds :: [Types.ConfigurationId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateConfigurationItemsToApplication' value with any optional fields omitted.
mkAssociateConfigurationItemsToApplication ::
  -- | 'applicationConfigurationId'
  Types.ApplicationId ->
  AssociateConfigurationItemsToApplication
mkAssociateConfigurationItemsToApplication
  applicationConfigurationId =
    AssociateConfigurationItemsToApplication'
      { applicationConfigurationId,
        configurationIds = Core.mempty
      }

-- | The configuration ID of an application with which items are to be associated.
--
-- /Note:/ Consider using 'applicationConfigurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitaApplicationConfigurationId :: Lens.Lens' AssociateConfigurationItemsToApplication Types.ApplicationId
acitaApplicationConfigurationId = Lens.field @"applicationConfigurationId"
{-# DEPRECATED acitaApplicationConfigurationId "Use generic-lens or generic-optics with 'applicationConfigurationId' instead." #-}

-- | The ID of each configuration item to be associated with an application.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitaConfigurationIds :: Lens.Lens' AssociateConfigurationItemsToApplication [Types.ConfigurationId]
acitaConfigurationIds = Lens.field @"configurationIds"
{-# DEPRECATED acitaConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Core.FromJSON AssociateConfigurationItemsToApplication where
  toJSON AssociateConfigurationItemsToApplication {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationConfigurationId" Core..= applicationConfigurationId),
            Core.Just ("configurationIds" Core..= configurationIds)
          ]
      )

instance Core.AWSRequest AssociateConfigurationItemsToApplication where
  type
    Rs AssociateConfigurationItemsToApplication =
      AssociateConfigurationItemsToApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.AssociateConfigurationItemsToApplication"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateConfigurationItemsToApplicationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateConfigurationItemsToApplicationResponse' smart constructor.
newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateConfigurationItemsToApplicationResponse' value with any optional fields omitted.
mkAssociateConfigurationItemsToApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateConfigurationItemsToApplicationResponse
mkAssociateConfigurationItemsToApplicationResponse responseStatus =
  AssociateConfigurationItemsToApplicationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acitarrsResponseStatus :: Lens.Lens' AssociateConfigurationItemsToApplicationResponse Core.Int
acitarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acitarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the delegated administrator account with the values provided.
module Network.AWS.GuardDuty.UpdateOrganizationConfiguration
  ( -- * Creating a request
    UpdateOrganizationConfiguration (..),
    mkUpdateOrganizationConfiguration,

    -- ** Request lenses
    uocDetectorId,
    uocAutoEnable,
    uocDataSources,

    -- * Destructuring the response
    UpdateOrganizationConfigurationResponse (..),
    mkUpdateOrganizationConfigurationResponse,

    -- ** Response lenses
    uocrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { -- | The ID of the detector to update the delegated administrator for.
    detectorId :: Types.DetectorId,
    -- | Indicates whether to automatically enable member accounts in the organization.
    autoEnable :: Core.Bool,
    -- | An object describes which data sources will be updated.
    dataSources :: Core.Maybe Types.OrganizationDataSourceConfigurations
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOrganizationConfiguration' value with any optional fields omitted.
mkUpdateOrganizationConfiguration ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'autoEnable'
  Core.Bool ->
  UpdateOrganizationConfiguration
mkUpdateOrganizationConfiguration detectorId autoEnable =
  UpdateOrganizationConfiguration'
    { detectorId,
      autoEnable,
      dataSources = Core.Nothing
    }

-- | The ID of the detector to update the delegated administrator for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDetectorId :: Lens.Lens' UpdateOrganizationConfiguration Types.DetectorId
uocDetectorId = Lens.field @"detectorId"
{-# DEPRECATED uocDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | Indicates whether to automatically enable member accounts in the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocAutoEnable :: Lens.Lens' UpdateOrganizationConfiguration Core.Bool
uocAutoEnable = Lens.field @"autoEnable"
{-# DEPRECATED uocAutoEnable "Use generic-lens or generic-optics with 'autoEnable' instead." #-}

-- | An object describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDataSources :: Lens.Lens' UpdateOrganizationConfiguration (Core.Maybe Types.OrganizationDataSourceConfigurations)
uocDataSources = Lens.field @"dataSources"
{-# DEPRECATED uocDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

instance Core.FromJSON UpdateOrganizationConfiguration where
  toJSON UpdateOrganizationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("autoEnable" Core..= autoEnable),
            ("dataSources" Core..=) Core.<$> dataSources
          ]
      )

instance Core.AWSRequest UpdateOrganizationConfiguration where
  type
    Rs UpdateOrganizationConfiguration =
      UpdateOrganizationConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/detector/" Core.<> (Core.toText detectorId) Core.<> ("/admin")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOrganizationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateOrganizationConfigurationResponse' smart constructor.
newtype UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOrganizationConfigurationResponse' value with any optional fields omitted.
mkUpdateOrganizationConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateOrganizationConfigurationResponse
mkUpdateOrganizationConfigurationResponse responseStatus =
  UpdateOrganizationConfigurationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocrrsResponseStatus :: Lens.Lens' UpdateOrganizationConfigurationResponse Core.Int
uocrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uocrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

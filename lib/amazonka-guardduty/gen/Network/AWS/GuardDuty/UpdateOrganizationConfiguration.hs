{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateOrganizationConfiguration (..)
    , mkUpdateOrganizationConfiguration
    -- ** Request lenses
    , uocDetectorId
    , uocAutoEnable
    , uocDataSources

    -- * Destructuring the response
    , UpdateOrganizationConfigurationResponse (..)
    , mkUpdateOrganizationConfigurationResponse
    -- ** Response lenses
    , uocrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector to update the delegated administrator for.
  , autoEnable :: Core.Bool
    -- ^ Indicates whether to automatically enable member accounts in the organization.
  , dataSources :: Core.Maybe Types.OrganizationDataSourceConfigurations
    -- ^ An object describes which data sources will be updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOrganizationConfiguration' value with any optional fields omitted.
mkUpdateOrganizationConfiguration
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Bool -- ^ 'autoEnable'
    -> UpdateOrganizationConfiguration
mkUpdateOrganizationConfiguration detectorId autoEnable
  = UpdateOrganizationConfiguration'{detectorId, autoEnable,
                                     dataSources = Core.Nothing}

-- | The ID of the detector to update the delegated administrator for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDetectorId :: Lens.Lens' UpdateOrganizationConfiguration Types.DetectorId
uocDetectorId = Lens.field @"detectorId"
{-# INLINEABLE uocDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | Indicates whether to automatically enable member accounts in the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocAutoEnable :: Lens.Lens' UpdateOrganizationConfiguration Core.Bool
uocAutoEnable = Lens.field @"autoEnable"
{-# INLINEABLE uocAutoEnable #-}
{-# DEPRECATED autoEnable "Use generic-lens or generic-optics with 'autoEnable' instead"  #-}

-- | An object describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocDataSources :: Lens.Lens' UpdateOrganizationConfiguration (Core.Maybe Types.OrganizationDataSourceConfigurations)
uocDataSources = Lens.field @"dataSources"
{-# INLINEABLE uocDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

instance Core.ToQuery UpdateOrganizationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateOrganizationConfiguration where
        toHeaders UpdateOrganizationConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateOrganizationConfiguration where
        toJSON UpdateOrganizationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("autoEnable" Core..= autoEnable),
                  ("dataSources" Core..=) Core.<$> dataSources])

instance Core.AWSRequest UpdateOrganizationConfiguration where
        type Rs UpdateOrganizationConfiguration =
             UpdateOrganizationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/admin",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateOrganizationConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateOrganizationConfigurationResponse' smart constructor.
newtype UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOrganizationConfigurationResponse' value with any optional fields omitted.
mkUpdateOrganizationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateOrganizationConfigurationResponse
mkUpdateOrganizationConfigurationResponse responseStatus
  = UpdateOrganizationConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uocrrsResponseStatus :: Lens.Lens' UpdateOrganizationConfigurationResponse Core.Int
uocrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uocrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

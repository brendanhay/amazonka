{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the account selected as the delegated administrator for GuardDuty.
module Network.AWS.GuardDuty.DescribeOrganizationConfiguration
    (
    -- * Creating a request
      DescribeOrganizationConfiguration (..)
    , mkDescribeOrganizationConfiguration
    -- ** Request lenses
    , docDetectorId

    -- * Destructuring the response
    , DescribeOrganizationConfigurationResponse (..)
    , mkDescribeOrganizationConfigurationResponse
    -- ** Response lenses
    , docrrsAutoEnable
    , docrrsMemberAccountLimitReached
    , docrrsDataSources
    , docrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationConfiguration' smart constructor.
newtype DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector to retrieve information about the delegated administrator from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConfiguration' value with any optional fields omitted.
mkDescribeOrganizationConfiguration
    :: Types.DetectorId -- ^ 'detectorId'
    -> DescribeOrganizationConfiguration
mkDescribeOrganizationConfiguration detectorId
  = DescribeOrganizationConfiguration'{detectorId}

-- | The ID of the detector to retrieve information about the delegated administrator from.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docDetectorId :: Lens.Lens' DescribeOrganizationConfiguration Types.DetectorId
docDetectorId = Lens.field @"detectorId"
{-# INLINEABLE docDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

instance Core.ToQuery DescribeOrganizationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganizationConfiguration where
        toHeaders DescribeOrganizationConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeOrganizationConfiguration where
        type Rs DescribeOrganizationConfiguration =
             DescribeOrganizationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/admin",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationConfigurationResponse' Core.<$>
                   (x Core..: "autoEnable") Core.<*>
                     x Core..: "memberAccountLimitReached"
                     Core.<*> x Core..:? "dataSources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { autoEnable :: Core.Bool
    -- ^ Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
  , memberAccountLimitReached :: Core.Bool
    -- ^ Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
  , dataSources :: Core.Maybe Types.OrganizationDataSourceConfigurationsResult
    -- ^ An object that describes which data sources are enabled automatically for member accounts.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationConfigurationResponse' value with any optional fields omitted.
mkDescribeOrganizationConfigurationResponse
    :: Core.Bool -- ^ 'autoEnable'
    -> Core.Bool -- ^ 'memberAccountLimitReached'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationConfigurationResponse
mkDescribeOrganizationConfigurationResponse autoEnable
  memberAccountLimitReached responseStatus
  = DescribeOrganizationConfigurationResponse'{autoEnable,
                                               memberAccountLimitReached,
                                               dataSources = Core.Nothing, responseStatus}

-- | Indicates whether GuardDuty is automatically enabled for accounts added to the organization.
--
-- /Note:/ Consider using 'autoEnable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsAutoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse Core.Bool
docrrsAutoEnable = Lens.field @"autoEnable"
{-# INLINEABLE docrrsAutoEnable #-}
{-# DEPRECATED autoEnable "Use generic-lens or generic-optics with 'autoEnable' instead"  #-}

-- | Indicates whether the maximum number of allowed member accounts are already associated with the delegated administrator master account.
--
-- /Note:/ Consider using 'memberAccountLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsMemberAccountLimitReached :: Lens.Lens' DescribeOrganizationConfigurationResponse Core.Bool
docrrsMemberAccountLimitReached = Lens.field @"memberAccountLimitReached"
{-# INLINEABLE docrrsMemberAccountLimitReached #-}
{-# DEPRECATED memberAccountLimitReached "Use generic-lens or generic-optics with 'memberAccountLimitReached' instead"  #-}

-- | An object that describes which data sources are enabled automatically for member accounts.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsDataSources :: Lens.Lens' DescribeOrganizationConfigurationResponse (Core.Maybe Types.OrganizationDataSourceConfigurationsResult)
docrrsDataSources = Lens.field @"dataSources"
{-# INLINEABLE docrrsDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docrrsResponseStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Core.Int
docrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE docrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

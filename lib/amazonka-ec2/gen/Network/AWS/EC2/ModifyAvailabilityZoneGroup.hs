{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyAvailabilityZoneGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the opt-in status of the Local Zone and Wavelength Zone group for your account.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the value for @GroupName@ .
module Network.AWS.EC2.ModifyAvailabilityZoneGroup
    (
    -- * Creating a request
      ModifyAvailabilityZoneGroup (..)
    , mkModifyAvailabilityZoneGroup
    -- ** Request lenses
    , mazgGroupName
    , mazgOptInStatus
    , mazgDryRun

    -- * Destructuring the response
    , ModifyAvailabilityZoneGroupResponse (..)
    , mkModifyAvailabilityZoneGroupResponse
    -- ** Response lenses
    , mazgrrsReturn
    , mazgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyAvailabilityZoneGroup' smart constructor.
data ModifyAvailabilityZoneGroup = ModifyAvailabilityZoneGroup'
  { groupName :: Core.Text
    -- ^ The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
  , optInStatus :: Types.ModifyAvailabilityZoneOptInStatus
    -- ^ Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyAvailabilityZoneGroup' value with any optional fields omitted.
mkModifyAvailabilityZoneGroup
    :: Core.Text -- ^ 'groupName'
    -> Types.ModifyAvailabilityZoneOptInStatus -- ^ 'optInStatus'
    -> ModifyAvailabilityZoneGroup
mkModifyAvailabilityZoneGroup groupName optInStatus
  = ModifyAvailabilityZoneGroup'{groupName, optInStatus,
                                 dryRun = Core.Nothing}

-- | The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgGroupName :: Lens.Lens' ModifyAvailabilityZoneGroup Core.Text
mazgGroupName = Lens.field @"groupName"
{-# INLINEABLE mazgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgOptInStatus :: Lens.Lens' ModifyAvailabilityZoneGroup Types.ModifyAvailabilityZoneOptInStatus
mazgOptInStatus = Lens.field @"optInStatus"
{-# INLINEABLE mazgOptInStatus #-}
{-# DEPRECATED optInStatus "Use generic-lens or generic-optics with 'optInStatus' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgDryRun :: Lens.Lens' ModifyAvailabilityZoneGroup (Core.Maybe Core.Bool)
mazgDryRun = Lens.field @"dryRun"
{-# INLINEABLE mazgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyAvailabilityZoneGroup where
        toQuery ModifyAvailabilityZoneGroup{..}
          = Core.toQueryPair "Action"
              ("ModifyAvailabilityZoneGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<> Core.toQueryPair "OptInStatus" optInStatus
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyAvailabilityZoneGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyAvailabilityZoneGroup where
        type Rs ModifyAvailabilityZoneGroup =
             ModifyAvailabilityZoneGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyAvailabilityZoneGroupResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyAvailabilityZoneGroupResponse' smart constructor.
data ModifyAvailabilityZoneGroupResponse = ModifyAvailabilityZoneGroupResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Is @true@ if the request succeeds, and an error otherwise.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyAvailabilityZoneGroupResponse' value with any optional fields omitted.
mkModifyAvailabilityZoneGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyAvailabilityZoneGroupResponse
mkModifyAvailabilityZoneGroupResponse responseStatus
  = ModifyAvailabilityZoneGroupResponse'{return = Core.Nothing,
                                         responseStatus}

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgrrsReturn :: Lens.Lens' ModifyAvailabilityZoneGroupResponse (Core.Maybe Core.Bool)
mazgrrsReturn = Lens.field @"return"
{-# INLINEABLE mazgrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgrrsResponseStatus :: Lens.Lens' ModifyAvailabilityZoneGroupResponse Core.Int
mazgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mazgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

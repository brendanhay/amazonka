{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeOrganizationalUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an organizational unit (OU).
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeOrganizationalUnit
    (
    -- * Creating a request
      DescribeOrganizationalUnit (..)
    , mkDescribeOrganizationalUnit
    -- ** Request lenses
    , douOrganizationalUnitId

    -- * Destructuring the response
    , DescribeOrganizationalUnitResponse (..)
    , mkDescribeOrganizationalUnitResponse
    -- ** Response lenses
    , dourrsOrganizationalUnit
    , dourrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOrganizationalUnit' smart constructor.
newtype DescribeOrganizationalUnit = DescribeOrganizationalUnit'
  { organizationalUnitId :: Types.OrganizationalUnitId
    -- ^ The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationalUnit' value with any optional fields omitted.
mkDescribeOrganizationalUnit
    :: Types.OrganizationalUnitId -- ^ 'organizationalUnitId'
    -> DescribeOrganizationalUnit
mkDescribeOrganizationalUnit organizationalUnitId
  = DescribeOrganizationalUnit'{organizationalUnitId}

-- | The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douOrganizationalUnitId :: Lens.Lens' DescribeOrganizationalUnit Types.OrganizationalUnitId
douOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# INLINEABLE douOrganizationalUnitId #-}
{-# DEPRECATED organizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead"  #-}

instance Core.ToQuery DescribeOrganizationalUnit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOrganizationalUnit where
        toHeaders DescribeOrganizationalUnit{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.DescribeOrganizationalUnit")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOrganizationalUnit where
        toJSON DescribeOrganizationalUnit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationalUnitId" Core..= organizationalUnitId)])

instance Core.AWSRequest DescribeOrganizationalUnit where
        type Rs DescribeOrganizationalUnit =
             DescribeOrganizationalUnitResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOrganizationalUnitResponse' Core.<$>
                   (x Core..:? "OrganizationalUnit") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOrganizationalUnitResponse' smart constructor.
data DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse'
  { organizationalUnit :: Core.Maybe Types.OrganizationalUnit
    -- ^ A structure that contains details about the specified OU.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOrganizationalUnitResponse' value with any optional fields omitted.
mkDescribeOrganizationalUnitResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOrganizationalUnitResponse
mkDescribeOrganizationalUnitResponse responseStatus
  = DescribeOrganizationalUnitResponse'{organizationalUnit =
                                          Core.Nothing,
                                        responseStatus}

-- | A structure that contains details about the specified OU.
--
-- /Note:/ Consider using 'organizationalUnit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dourrsOrganizationalUnit :: Lens.Lens' DescribeOrganizationalUnitResponse (Core.Maybe Types.OrganizationalUnit)
dourrsOrganizationalUnit = Lens.field @"organizationalUnit"
{-# INLINEABLE dourrsOrganizationalUnit #-}
{-# DEPRECATED organizationalUnit "Use generic-lens or generic-optics with 'organizationalUnit' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dourrsResponseStatus :: Lens.Lens' DescribeOrganizationalUnitResponse Core.Int
dourrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dourrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

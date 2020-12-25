{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroupReferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Describes the VPCs on the other side of a VPC peering connection that are referencing the security groups you've specified in this request.
module Network.AWS.EC2.DescribeSecurityGroupReferences
  ( -- * Creating a request
    DescribeSecurityGroupReferences (..),
    mkDescribeSecurityGroupReferences,

    -- ** Request lenses
    dsgrGroupId,
    dsgrDryRun,

    -- * Destructuring the response
    DescribeSecurityGroupReferencesResponse (..),
    mkDescribeSecurityGroupReferencesResponse,

    -- ** Response lenses
    dsgrrrsSecurityGroupReferenceSet,
    dsgrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSecurityGroupReferences' smart constructor.
data DescribeSecurityGroupReferences = DescribeSecurityGroupReferences'
  { -- | The IDs of the security groups in your account.
    groupId :: [Types.SecurityGroupId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityGroupReferences' value with any optional fields omitted.
mkDescribeSecurityGroupReferences ::
  DescribeSecurityGroupReferences
mkDescribeSecurityGroupReferences =
  DescribeSecurityGroupReferences'
    { groupId = Core.mempty,
      dryRun = Core.Nothing
    }

-- | The IDs of the security groups in your account.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrGroupId :: Lens.Lens' DescribeSecurityGroupReferences [Types.SecurityGroupId]
dsgrGroupId = Lens.field @"groupId"
{-# DEPRECATED dsgrGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrDryRun :: Lens.Lens' DescribeSecurityGroupReferences (Core.Maybe Core.Bool)
dsgrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsgrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeSecurityGroupReferences where
  type
    Rs DescribeSecurityGroupReferences =
      DescribeSecurityGroupReferencesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSecurityGroupReferences")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "GroupId" groupId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupReferencesResponse'
            Core.<$> ( x Core..@? "securityGroupReferenceSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeSecurityGroupReferencesResponse' smart constructor.
data DescribeSecurityGroupReferencesResponse = DescribeSecurityGroupReferencesResponse'
  { -- | Information about the VPCs with the referencing security groups.
    securityGroupReferenceSet :: Core.Maybe [Types.SecurityGroupReference],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityGroupReferencesResponse' value with any optional fields omitted.
mkDescribeSecurityGroupReferencesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSecurityGroupReferencesResponse
mkDescribeSecurityGroupReferencesResponse responseStatus =
  DescribeSecurityGroupReferencesResponse'
    { securityGroupReferenceSet =
        Core.Nothing,
      responseStatus
    }

-- | Information about the VPCs with the referencing security groups.
--
-- /Note:/ Consider using 'securityGroupReferenceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrrsSecurityGroupReferenceSet :: Lens.Lens' DescribeSecurityGroupReferencesResponse (Core.Maybe [Types.SecurityGroupReference])
dsgrrrsSecurityGroupReferenceSet = Lens.field @"securityGroupReferenceSet"
{-# DEPRECATED dsgrrrsSecurityGroupReferenceSet "Use generic-lens or generic-optics with 'securityGroupReferenceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrrsResponseStatus :: Lens.Lens' DescribeSecurityGroupReferencesResponse Core.Int
dsgrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsgrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

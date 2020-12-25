{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a billing group.
module Network.AWS.IoT.DescribeBillingGroup
  ( -- * Creating a request
    DescribeBillingGroup (..),
    mkDescribeBillingGroup,

    -- ** Request lenses
    dBillingGroupName,

    -- * Destructuring the response
    DescribeBillingGroupResponse (..),
    mkDescribeBillingGroupResponse,

    -- ** Response lenses
    dbgrfrsBillingGroupArn,
    dbgrfrsBillingGroupId,
    dbgrfrsBillingGroupMetadata,
    dbgrfrsBillingGroupName,
    dbgrfrsBillingGroupProperties,
    dbgrfrsVersion,
    dbgrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBillingGroup' smart constructor.
newtype DescribeBillingGroup = DescribeBillingGroup'
  { -- | The name of the billing group.
    billingGroupName :: Types.BillingGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBillingGroup' value with any optional fields omitted.
mkDescribeBillingGroup ::
  -- | 'billingGroupName'
  Types.BillingGroupName ->
  DescribeBillingGroup
mkDescribeBillingGroup billingGroupName =
  DescribeBillingGroup' {billingGroupName}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBillingGroupName :: Lens.Lens' DescribeBillingGroup Types.BillingGroupName
dBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED dBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Core.AWSRequest DescribeBillingGroup where
  type Rs DescribeBillingGroup = DescribeBillingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/billing-groups/" Core.<> (Core.toText billingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBillingGroupResponse'
            Core.<$> (x Core..:? "billingGroupArn")
            Core.<*> (x Core..:? "billingGroupId")
            Core.<*> (x Core..:? "billingGroupMetadata")
            Core.<*> (x Core..:? "billingGroupName")
            Core.<*> (x Core..:? "billingGroupProperties")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeBillingGroupResponse' smart constructor.
data DescribeBillingGroupResponse = DescribeBillingGroupResponse'
  { -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Types.BillingGroupArn,
    -- | The ID of the billing group.
    billingGroupId :: Core.Maybe Types.BillingGroupId,
    -- | Additional information about the billing group.
    billingGroupMetadata :: Core.Maybe Types.BillingGroupMetadata,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Types.BillingGroupName,
    -- | The properties of the billing group.
    billingGroupProperties :: Core.Maybe Types.BillingGroupProperties,
    -- | The version of the billing group.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBillingGroupResponse' value with any optional fields omitted.
mkDescribeBillingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBillingGroupResponse
mkDescribeBillingGroupResponse responseStatus =
  DescribeBillingGroupResponse'
    { billingGroupArn = Core.Nothing,
      billingGroupId = Core.Nothing,
      billingGroupMetadata = Core.Nothing,
      billingGroupName = Core.Nothing,
      billingGroupProperties = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsBillingGroupArn :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Types.BillingGroupArn)
dbgrfrsBillingGroupArn = Lens.field @"billingGroupArn"
{-# DEPRECATED dbgrfrsBillingGroupArn "Use generic-lens or generic-optics with 'billingGroupArn' instead." #-}

-- | The ID of the billing group.
--
-- /Note:/ Consider using 'billingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsBillingGroupId :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Types.BillingGroupId)
dbgrfrsBillingGroupId = Lens.field @"billingGroupId"
{-# DEPRECATED dbgrfrsBillingGroupId "Use generic-lens or generic-optics with 'billingGroupId' instead." #-}

-- | Additional information about the billing group.
--
-- /Note:/ Consider using 'billingGroupMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsBillingGroupMetadata :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Types.BillingGroupMetadata)
dbgrfrsBillingGroupMetadata = Lens.field @"billingGroupMetadata"
{-# DEPRECATED dbgrfrsBillingGroupMetadata "Use generic-lens or generic-optics with 'billingGroupMetadata' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsBillingGroupName :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Types.BillingGroupName)
dbgrfrsBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED dbgrfrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsBillingGroupProperties :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Types.BillingGroupProperties)
dbgrfrsBillingGroupProperties = Lens.field @"billingGroupProperties"
{-# DEPRECATED dbgrfrsBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

-- | The version of the billing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsVersion :: Lens.Lens' DescribeBillingGroupResponse (Core.Maybe Core.Integer)
dbgrfrsVersion = Lens.field @"version"
{-# DEPRECATED dbgrfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrfrsResponseStatus :: Lens.Lens' DescribeBillingGroupResponse Core.Int
dbgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Availability Zone, instance count, instance type, or network platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved Instances to be modified must be identical, except for Availability Zone, network platform, and instance type.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.ModifyReservedInstances
  ( -- * Creating a request
    ModifyReservedInstances (..),
    mkModifyReservedInstances,

    -- ** Request lenses
    mriReservedInstancesIds,
    mriTargetConfigurations,
    mriClientToken,

    -- * Destructuring the response
    ModifyReservedInstancesResponse (..),
    mkModifyReservedInstancesResponse,

    -- ** Response lenses
    mrirrsReservedInstancesModificationId,
    mrirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { -- | The IDs of the Reserved Instances to modify.
    reservedInstancesIds :: [Types.ReservationId],
    -- | The configuration settings for the Reserved Instances to modify.
    targetConfigurations :: [Types.ReservedInstancesConfiguration],
    -- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.ClientToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReservedInstances' value with any optional fields omitted.
mkModifyReservedInstances ::
  ModifyReservedInstances
mkModifyReservedInstances =
  ModifyReservedInstances'
    { reservedInstancesIds = Core.mempty,
      targetConfigurations = Core.mempty,
      clientToken = Core.Nothing
    }

-- | The IDs of the Reserved Instances to modify.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReservedInstancesIds :: Lens.Lens' ModifyReservedInstances [Types.ReservationId]
mriReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# DEPRECATED mriReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

-- | The configuration settings for the Reserved Instances to modify.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriTargetConfigurations :: Lens.Lens' ModifyReservedInstances [Types.ReservedInstancesConfiguration]
mriTargetConfigurations = Lens.field @"targetConfigurations"
{-# DEPRECATED mriTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

-- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriClientToken :: Lens.Lens' ModifyReservedInstances (Core.Maybe Types.ClientToken)
mriClientToken = Lens.field @"clientToken"
{-# DEPRECATED mriClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Core.AWSRequest ModifyReservedInstances where
  type Rs ModifyReservedInstances = ModifyReservedInstancesResponse
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
            ( Core.pure ("Action", "ModifyReservedInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ReservedInstancesId" reservedInstancesIds)
                Core.<> ( Core.toQueryList
                            "ReservedInstancesConfigurationSetItemType"
                            targetConfigurations
                        )
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyReservedInstancesResponse'
            Core.<$> (x Core..@? "reservedInstancesModificationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { -- | The ID for the modification.
    reservedInstancesModificationId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReservedInstancesResponse' value with any optional fields omitted.
mkModifyReservedInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReservedInstancesResponse
mkModifyReservedInstancesResponse responseStatus =
  ModifyReservedInstancesResponse'
    { reservedInstancesModificationId =
        Core.Nothing,
      responseStatus
    }

-- | The ID for the modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsReservedInstancesModificationId :: Lens.Lens' ModifyReservedInstancesResponse (Core.Maybe Types.String)
mrirrsReservedInstancesModificationId = Lens.field @"reservedInstancesModificationId"
{-# DEPRECATED mrirrsReservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsResponseStatus :: Lens.Lens' ModifyReservedInstancesResponse Core.Int
mrirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

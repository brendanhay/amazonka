{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyReservedInstances (..)
    , mkModifyReservedInstances
    -- ** Request lenses
    , mriReservedInstancesIds
    , mriTargetConfigurations
    , mriClientToken

    -- * Destructuring the response
    , ModifyReservedInstancesResponse (..)
    , mkModifyReservedInstancesResponse
    -- ** Response lenses
    , mrirrsReservedInstancesModificationId
    , mrirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { reservedInstancesIds :: [Types.ReservationId]
    -- ^ The IDs of the Reserved Instances to modify.
  , targetConfigurations :: [Types.ReservedInstancesConfiguration]
    -- ^ The configuration settings for the Reserved Instances to modify.
  , clientToken :: Core.Maybe Core.Text
    -- ^ A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReservedInstances' value with any optional fields omitted.
mkModifyReservedInstances
    :: ModifyReservedInstances
mkModifyReservedInstances
  = ModifyReservedInstances'{reservedInstancesIds = Core.mempty,
                             targetConfigurations = Core.mempty, clientToken = Core.Nothing}

-- | The IDs of the Reserved Instances to modify.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReservedInstancesIds :: Lens.Lens' ModifyReservedInstances [Types.ReservationId]
mriReservedInstancesIds = Lens.field @"reservedInstancesIds"
{-# INLINEABLE mriReservedInstancesIds #-}
{-# DEPRECATED reservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead"  #-}

-- | The configuration settings for the Reserved Instances to modify.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriTargetConfigurations :: Lens.Lens' ModifyReservedInstances [Types.ReservedInstancesConfiguration]
mriTargetConfigurations = Lens.field @"targetConfigurations"
{-# INLINEABLE mriTargetConfigurations #-}
{-# DEPRECATED targetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead"  #-}

-- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriClientToken :: Lens.Lens' ModifyReservedInstances (Core.Maybe Core.Text)
mriClientToken = Lens.field @"clientToken"
{-# INLINEABLE mriClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

instance Core.ToQuery ModifyReservedInstances where
        toQuery ModifyReservedInstances{..}
          = Core.toQueryPair "Action"
              ("ModifyReservedInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ReservedInstancesId" reservedInstancesIds
              Core.<>
              Core.toQueryList "ReservedInstancesConfigurationSetItemType"
                targetConfigurations
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken

instance Core.ToHeaders ModifyReservedInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyReservedInstances where
        type Rs ModifyReservedInstances = ModifyReservedInstancesResponse
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
                 ModifyReservedInstancesResponse' Core.<$>
                   (x Core..@? "reservedInstancesModificationId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { reservedInstancesModificationId :: Core.Maybe Core.Text
    -- ^ The ID for the modification.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReservedInstancesResponse' value with any optional fields omitted.
mkModifyReservedInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyReservedInstancesResponse
mkModifyReservedInstancesResponse responseStatus
  = ModifyReservedInstancesResponse'{reservedInstancesModificationId
                                       = Core.Nothing,
                                     responseStatus}

-- | The ID for the modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsReservedInstancesModificationId :: Lens.Lens' ModifyReservedInstancesResponse (Core.Maybe Core.Text)
mrirrsReservedInstancesModificationId = Lens.field @"reservedInstancesModificationId"
{-# INLINEABLE mrirrsReservedInstancesModificationId #-}
{-# DEPRECATED reservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirrsResponseStatus :: Lens.Lens' ModifyReservedInstancesResponse Core.Int
mrirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

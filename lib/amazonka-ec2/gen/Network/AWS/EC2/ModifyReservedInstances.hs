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
    mriClientToken,
    mriTargetConfigurations,
    mriReservedInstancesIds,

    -- * Destructuring the response
    ModifyReservedInstancesResponse (..),
    mkModifyReservedInstancesResponse,

    -- ** Response lenses
    mrirsReservedInstancesModificationId,
    mrirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstances' smart constructor.
data ModifyReservedInstances = ModifyReservedInstances'
  { -- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The configuration settings for the Reserved Instances to modify.
    targetConfigurations :: [ReservedInstancesConfiguration],
    -- | The IDs of the Reserved Instances to modify.
    reservedInstancesIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReservedInstances' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'targetConfigurations' - The configuration settings for the Reserved Instances to modify.
-- * 'reservedInstancesIds' - The IDs of the Reserved Instances to modify.
mkModifyReservedInstances ::
  ModifyReservedInstances
mkModifyReservedInstances =
  ModifyReservedInstances'
    { clientToken = Lude.Nothing,
      targetConfigurations = Lude.mempty,
      reservedInstancesIds = Lude.mempty
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriClientToken :: Lens.Lens' ModifyReservedInstances (Lude.Maybe Lude.Text)
mriClientToken = Lens.lens (clientToken :: ModifyReservedInstances -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ModifyReservedInstances)
{-# DEPRECATED mriClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The configuration settings for the Reserved Instances to modify.
--
-- /Note:/ Consider using 'targetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriTargetConfigurations :: Lens.Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
mriTargetConfigurations = Lens.lens (targetConfigurations :: ModifyReservedInstances -> [ReservedInstancesConfiguration]) (\s a -> s {targetConfigurations = a} :: ModifyReservedInstances)
{-# DEPRECATED mriTargetConfigurations "Use generic-lens or generic-optics with 'targetConfigurations' instead." #-}

-- | The IDs of the Reserved Instances to modify.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mriReservedInstancesIds :: Lens.Lens' ModifyReservedInstances [Lude.Text]
mriReservedInstancesIds = Lens.lens (reservedInstancesIds :: ModifyReservedInstances -> [Lude.Text]) (\s a -> s {reservedInstancesIds = a} :: ModifyReservedInstances)
{-# DEPRECATED mriReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

instance Lude.AWSRequest ModifyReservedInstances where
  type Rs ModifyReservedInstances = ModifyReservedInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyReservedInstancesResponse'
            Lude.<$> (x Lude..@? "reservedInstancesModificationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReservedInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyReservedInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReservedInstances where
  toQuery ModifyReservedInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyReservedInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQueryList
          "ReservedInstancesConfigurationSetItemType"
          targetConfigurations,
        Lude.toQueryList "ReservedInstancesId" reservedInstancesIds
      ]

-- | Contains the output of ModifyReservedInstances.
--
-- /See:/ 'mkModifyReservedInstancesResponse' smart constructor.
data ModifyReservedInstancesResponse = ModifyReservedInstancesResponse'
  { -- | The ID for the modification.
    reservedInstancesModificationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReservedInstancesResponse' with the minimum fields required to make a request.
--
-- * 'reservedInstancesModificationId' - The ID for the modification.
-- * 'responseStatus' - The response status code.
mkModifyReservedInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReservedInstancesResponse
mkModifyReservedInstancesResponse pResponseStatus_ =
  ModifyReservedInstancesResponse'
    { reservedInstancesModificationId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID for the modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirsReservedInstancesModificationId :: Lens.Lens' ModifyReservedInstancesResponse (Lude.Maybe Lude.Text)
mrirsReservedInstancesModificationId = Lens.lens (reservedInstancesModificationId :: ModifyReservedInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesModificationId = a} :: ModifyReservedInstancesResponse)
{-# DEPRECATED mrirsReservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrirsResponseStatus :: Lens.Lens' ModifyReservedInstancesResponse Lude.Int
mrirsResponseStatus = Lens.lens (responseStatus :: ModifyReservedInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReservedInstancesResponse)
{-# DEPRECATED mrirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

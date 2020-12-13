{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AllocateHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At a minimum, specify the supported instance type or instance family, the Availability Zone in which to allocate the host, and the number of hosts to allocate.
module Network.AWS.EC2.AllocateHosts
  ( -- * Creating a request
    AllocateHosts (..),
    mkAllocateHosts,

    -- ** Request lenses
    ahInstanceFamily,
    ahClientToken,
    ahQuantity,
    ahInstanceType,
    ahTagSpecifications,
    ahAvailabilityZone,
    ahHostRecovery,
    ahAutoPlacement,

    -- * Destructuring the response
    AllocateHostsResponse (..),
    mkAllocateHostsResponse,

    -- ** Response lenses
    ahrsHostIds,
    ahrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAllocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { -- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
    --
    -- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
    instanceFamily :: Lude.Maybe Lude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The number of Dedicated Hosts to allocate to your account with these parameters.
    quantity :: Lude.Int,
    -- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
    --
    -- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The tags to apply to the Dedicated Host during creation.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The Availability Zone in which to allocate the Dedicated Host.
    availabilityZone :: Lude.Text,
    -- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Default: @off@
    hostRecovery :: Lude.Maybe HostRecovery,
    -- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- Default: @on@
    autoPlacement :: Lude.Maybe AutoPlacement
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateHosts' with the minimum fields required to make a request.
--
-- * 'instanceFamily' - Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'quantity' - The number of Dedicated Hosts to allocate to your account with these parameters.
-- * 'instanceType' - Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
-- * 'tagSpecifications' - The tags to apply to the Dedicated Host during creation.
-- * 'availabilityZone' - The Availability Zone in which to allocate the Dedicated Host.
-- * 'hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @off@
-- * 'autoPlacement' - Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Default: @on@
mkAllocateHosts ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'availabilityZone'
  Lude.Text ->
  AllocateHosts
mkAllocateHosts pQuantity_ pAvailabilityZone_ =
  AllocateHosts'
    { instanceFamily = Lude.Nothing,
      clientToken = Lude.Nothing,
      quantity = pQuantity_,
      instanceType = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      availabilityZone = pAvailabilityZone_,
      hostRecovery = Lude.Nothing,
      autoPlacement = Lude.Nothing
    }

-- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceFamily :: Lens.Lens' AllocateHosts (Lude.Maybe Lude.Text)
ahInstanceFamily = Lens.lens (instanceFamily :: AllocateHosts -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: AllocateHosts)
{-# DEPRECATED ahInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahClientToken :: Lens.Lens' AllocateHosts (Lude.Maybe Lude.Text)
ahClientToken = Lens.lens (clientToken :: AllocateHosts -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: AllocateHosts)
{-# DEPRECATED ahClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of Dedicated Hosts to allocate to your account with these parameters.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahQuantity :: Lens.Lens' AllocateHosts Lude.Int
ahQuantity = Lens.lens (quantity :: AllocateHosts -> Lude.Int) (\s a -> s {quantity = a} :: AllocateHosts)
{-# DEPRECATED ahQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceType :: Lens.Lens' AllocateHosts (Lude.Maybe Lude.Text)
ahInstanceType = Lens.lens (instanceType :: AllocateHosts -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: AllocateHosts)
{-# DEPRECATED ahInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The tags to apply to the Dedicated Host during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahTagSpecifications :: Lens.Lens' AllocateHosts (Lude.Maybe [TagSpecification])
ahTagSpecifications = Lens.lens (tagSpecifications :: AllocateHosts -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: AllocateHosts)
{-# DEPRECATED ahTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Availability Zone in which to allocate the Dedicated Host.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAvailabilityZone :: Lens.Lens' AllocateHosts Lude.Text
ahAvailabilityZone = Lens.lens (availabilityZone :: AllocateHosts -> Lude.Text) (\s a -> s {availabilityZone = a} :: AllocateHosts)
{-# DEPRECATED ahAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @off@
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahHostRecovery :: Lens.Lens' AllocateHosts (Lude.Maybe HostRecovery)
ahHostRecovery = Lens.lens (hostRecovery :: AllocateHosts -> Lude.Maybe HostRecovery) (\s a -> s {hostRecovery = a} :: AllocateHosts)
{-# DEPRECATED ahHostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead." #-}

-- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Default: @on@
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAutoPlacement :: Lens.Lens' AllocateHosts (Lude.Maybe AutoPlacement)
ahAutoPlacement = Lens.lens (autoPlacement :: AllocateHosts -> Lude.Maybe AutoPlacement) (\s a -> s {autoPlacement = a} :: AllocateHosts)
{-# DEPRECATED ahAutoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead." #-}

instance Lude.AWSRequest AllocateHosts where
  type Rs AllocateHosts = AllocateHostsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AllocateHostsResponse'
            Lude.<$> ( x Lude..@? "hostIdSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AllocateHosts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AllocateHosts where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocateHosts where
  toQuery AllocateHosts' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AllocateHosts" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceFamily" Lude.=: instanceFamily,
        "ClientToken" Lude.=: clientToken,
        "Quantity" Lude.=: quantity,
        "InstanceType" Lude.=: instanceType,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "AvailabilityZone" Lude.=: availabilityZone,
        "HostRecovery" Lude.=: hostRecovery,
        "AutoPlacement" Lude.=: autoPlacement
      ]

-- | Contains the output of AllocateHosts.
--
-- /See:/ 'mkAllocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { -- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
    hostIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateHostsResponse' with the minimum fields required to make a request.
--
-- * 'hostIds' - The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
-- * 'responseStatus' - The response status code.
mkAllocateHostsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AllocateHostsResponse
mkAllocateHostsResponse pResponseStatus_ =
  AllocateHostsResponse'
    { hostIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrsHostIds :: Lens.Lens' AllocateHostsResponse (Lude.Maybe [Lude.Text])
ahrsHostIds = Lens.lens (hostIds :: AllocateHostsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {hostIds = a} :: AllocateHostsResponse)
{-# DEPRECATED ahrsHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrsResponseStatus :: Lens.Lens' AllocateHostsResponse Lude.Int
ahrsResponseStatus = Lens.lens (responseStatus :: AllocateHostsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AllocateHostsResponse)
{-# DEPRECATED ahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

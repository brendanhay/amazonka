{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the auto-placement setting of a Dedicated Host. When auto-placement is enabled, any instances that you launch with a tenancy of @host@ but without a specific host ID are placed onto any available Dedicated Host in your account that has auto-placement enabled. When auto-placement is disabled, you need to provide a host ID to have the instance launch onto a specific host. If no host ID is provided, the instance is launched onto a suitable host with auto-placement enabled.
--
-- You can also use this API action to modify a Dedicated Host to support either multiple instance types in an instance family, or to support a specific instance type only.
module Network.AWS.EC2.ModifyHosts
  ( -- * Creating a request
    ModifyHosts (..),
    mkModifyHosts,

    -- ** Request lenses
    mhInstanceFamily,
    mhInstanceType,
    mhHostRecovery,
    mhAutoPlacement,
    mhHostIds,

    -- * Destructuring the response
    ModifyHostsResponse (..),
    mkModifyHostsResponse,

    -- ** Response lenses
    mhrsUnsuccessful,
    mhrsSuccessful,
    mhrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
  { -- | Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family.
    --
    -- If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
    instanceFamily :: Lude.Maybe Lude.Text,
    -- | Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type.
    --
    -- If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
    instanceType :: Lude.Maybe Lude.Text,
    -- | Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
    hostRecovery :: Lude.Maybe HostRecovery,
    -- | Specify whether to enable or disable auto-placement.
    autoPlacement :: Lude.Maybe AutoPlacement,
    -- | The IDs of the Dedicated Hosts to modify.
    hostIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyHosts' with the minimum fields required to make a request.
--
-- * 'instanceFamily' - Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
-- * 'instanceType' - Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
-- * 'hostRecovery' - Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'autoPlacement' - Specify whether to enable or disable auto-placement.
-- * 'hostIds' - The IDs of the Dedicated Hosts to modify.
mkModifyHosts ::
  ModifyHosts
mkModifyHosts =
  ModifyHosts'
    { instanceFamily = Lude.Nothing,
      instanceType = Lude.Nothing,
      hostRecovery = Lude.Nothing,
      autoPlacement = Lude.Nothing,
      hostIds = Lude.mempty
    }

-- | Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhInstanceFamily :: Lens.Lens' ModifyHosts (Lude.Maybe Lude.Text)
mhInstanceFamily = Lens.lens (instanceFamily :: ModifyHosts -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: ModifyHosts)
{-# DEPRECATED mhInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhInstanceType :: Lens.Lens' ModifyHosts (Lude.Maybe Lude.Text)
mhInstanceType = Lens.lens (instanceType :: ModifyHosts -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ModifyHosts)
{-# DEPRECATED mhInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHostRecovery :: Lens.Lens' ModifyHosts (Lude.Maybe HostRecovery)
mhHostRecovery = Lens.lens (hostRecovery :: ModifyHosts -> Lude.Maybe HostRecovery) (\s a -> s {hostRecovery = a} :: ModifyHosts)
{-# DEPRECATED mhHostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead." #-}

-- | Specify whether to enable or disable auto-placement.
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhAutoPlacement :: Lens.Lens' ModifyHosts (Lude.Maybe AutoPlacement)
mhAutoPlacement = Lens.lens (autoPlacement :: ModifyHosts -> Lude.Maybe AutoPlacement) (\s a -> s {autoPlacement = a} :: ModifyHosts)
{-# DEPRECATED mhAutoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead." #-}

-- | The IDs of the Dedicated Hosts to modify.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHostIds :: Lens.Lens' ModifyHosts [Lude.Text]
mhHostIds = Lens.lens (hostIds :: ModifyHosts -> [Lude.Text]) (\s a -> s {hostIds = a} :: ModifyHosts)
{-# DEPRECATED mhHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

instance Lude.AWSRequest ModifyHosts where
  type Rs ModifyHosts = ModifyHostsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyHostsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyHosts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyHosts where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyHosts where
  toQuery ModifyHosts' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyHosts" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceFamily" Lude.=: instanceFamily,
        "InstanceType" Lude.=: instanceType,
        "HostRecovery" Lude.=: hostRecovery,
        "AutoPlacement" Lude.=: autoPlacement,
        Lude.toQueryList "HostId" hostIds
      ]

-- | /See:/ 'mkModifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
  { -- | The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The IDs of the Dedicated Hosts that were successfully modified.
    successful :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyHostsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
-- * 'successful' - The IDs of the Dedicated Hosts that were successfully modified.
-- * 'responseStatus' - The response status code.
mkModifyHostsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyHostsResponse
mkModifyHostsResponse pResponseStatus_ =
  ModifyHostsResponse'
    { unsuccessful = Lude.Nothing,
      successful = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrsUnsuccessful :: Lens.Lens' ModifyHostsResponse (Lude.Maybe [UnsuccessfulItem])
mhrsUnsuccessful = Lens.lens (unsuccessful :: ModifyHostsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: ModifyHostsResponse)
{-# DEPRECATED mhrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The IDs of the Dedicated Hosts that were successfully modified.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrsSuccessful :: Lens.Lens' ModifyHostsResponse (Lude.Maybe [Lude.Text])
mhrsSuccessful = Lens.lens (successful :: ModifyHostsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {successful = a} :: ModifyHostsResponse)
{-# DEPRECATED mhrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrsResponseStatus :: Lens.Lens' ModifyHostsResponse Lude.Int
mhrsResponseStatus = Lens.lens (responseStatus :: ModifyHostsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyHostsResponse)
{-# DEPRECATED mhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

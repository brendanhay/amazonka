{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstancePlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the placement attributes for a specified instance. You can do the following:
--
--
--     * Modify the affinity between an instance and a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Host> . When affinity is set to @host@ and the instance is not associated with a specific Dedicated Host, the next time the instance is launched, it is automatically associated with the host on which it lands. If the instance is restarted or rebooted, this relationship persists.
--
--
--     * Change the Dedicated Host with which an instance is associated.
--
--
--     * Change the instance tenancy of an instance from @host@ to @dedicated@ , or from @dedicated@ to @host@ .
--
--
--     * Move an instance to or from a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html placement group> .
--
--
-- At least one attribute for affinity, host ID, tenancy, or placement group name must be specified in the request. Affinity and tenancy can be modified in the same request.
-- To modify the host ID, tenancy, placement group, or partition for an instance, the instance must be in the @stopped@ state.
module Network.AWS.EC2.ModifyInstancePlacement
  ( -- * Creating a request
    ModifyInstancePlacement (..),
    mkModifyInstancePlacement,

    -- ** Request lenses
    mipAffinity,
    mipHostId,
    mipPartitionNumber,
    mipTenancy,
    mipGroupName,
    mipHostResourceGroupARN,
    mipInstanceId,

    -- * Destructuring the response
    ModifyInstancePlacementResponse (..),
    mkModifyInstancePlacementResponse,

    -- ** Response lenses
    miprsReturn,
    miprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { affinity ::
      Lude.Maybe Affinity,
    hostId :: Lude.Maybe Lude.Text,
    partitionNumber :: Lude.Maybe Lude.Int,
    tenancy :: Lude.Maybe HostTenancy,
    groupName :: Lude.Maybe Lude.Text,
    hostResourceGroupARN ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstancePlacement' with the minimum fields required to make a request.
--
-- * 'affinity' - The affinity setting for the instance.
-- * 'groupName' - The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
--
-- To remove an instance from a placement group, specify an empty string ("").
-- * 'hostId' - The ID of the Dedicated Host with which to associate the instance.
-- * 'hostResourceGroupARN' - The ARN of the host resource group in which to place the instance.
-- * 'instanceId' - The ID of the instance that you are modifying.
-- * 'partitionNumber' - Reserved for future use.
-- * 'tenancy' - The tenancy for the instance.
mkModifyInstancePlacement ::
  -- | 'instanceId'
  Lude.Text ->
  ModifyInstancePlacement
mkModifyInstancePlacement pInstanceId_ =
  ModifyInstancePlacement'
    { affinity = Lude.Nothing,
      hostId = Lude.Nothing,
      partitionNumber = Lude.Nothing,
      tenancy = Lude.Nothing,
      groupName = Lude.Nothing,
      hostResourceGroupARN = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The affinity setting for the instance.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipAffinity :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe Affinity)
mipAffinity = Lens.lens (affinity :: ModifyInstancePlacement -> Lude.Maybe Affinity) (\s a -> s {affinity = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The ID of the Dedicated Host with which to associate the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostId :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe Lude.Text)
mipHostId = Lens.lens (hostId :: ModifyInstancePlacement -> Lude.Maybe Lude.Text) (\s a -> s {hostId = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipPartitionNumber :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe Lude.Int)
mipPartitionNumber = Lens.lens (partitionNumber :: ModifyInstancePlacement -> Lude.Maybe Lude.Int) (\s a -> s {partitionNumber = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | The tenancy for the instance.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipTenancy :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe HostTenancy)
mipTenancy = Lens.lens (tenancy :: ModifyInstancePlacement -> Lude.Maybe HostTenancy) (\s a -> s {tenancy = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
--
-- To remove an instance from a placement group, specify an empty string ("").
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipGroupName :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe Lude.Text)
mipGroupName = Lens.lens (groupName :: ModifyInstancePlacement -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ARN of the host resource group in which to place the instance.
--
-- /Note:/ Consider using 'hostResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostResourceGroupARN :: Lens.Lens' ModifyInstancePlacement (Lude.Maybe Lude.Text)
mipHostResourceGroupARN = Lens.lens (hostResourceGroupARN :: ModifyInstancePlacement -> Lude.Maybe Lude.Text) (\s a -> s {hostResourceGroupARN = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipHostResourceGroupARN "Use generic-lens or generic-optics with 'hostResourceGroupARN' instead." #-}

-- | The ID of the instance that you are modifying.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipInstanceId :: Lens.Lens' ModifyInstancePlacement Lude.Text
mipInstanceId = Lens.lens (instanceId :: ModifyInstancePlacement -> Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstancePlacement)
{-# DEPRECATED mipInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest ModifyInstancePlacement where
  type Rs ModifyInstancePlacement = ModifyInstancePlacementResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyInstancePlacementResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyInstancePlacement where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstancePlacement where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstancePlacement where
  toQuery ModifyInstancePlacement' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyInstancePlacement" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Affinity" Lude.=: affinity,
        "HostId" Lude.=: hostId,
        "PartitionNumber" Lude.=: partitionNumber,
        "Tenancy" Lude.=: tenancy,
        "GroupName" Lude.=: groupName,
        "HostResourceGroupArn" Lude.=: hostResourceGroupARN,
        "InstanceId" Lude.=: instanceId
      ]

-- | /See:/ 'mkModifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstancePlacementResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
mkModifyInstancePlacementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyInstancePlacementResponse
mkModifyInstancePlacementResponse pResponseStatus_ =
  ModifyInstancePlacementResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprsReturn :: Lens.Lens' ModifyInstancePlacementResponse (Lude.Maybe Lude.Bool)
miprsReturn = Lens.lens (return :: ModifyInstancePlacementResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyInstancePlacementResponse)
{-# DEPRECATED miprsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprsResponseStatus :: Lens.Lens' ModifyInstancePlacementResponse Lude.Int
miprsResponseStatus = Lens.lens (responseStatus :: ModifyInstancePlacementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyInstancePlacementResponse)
{-# DEPRECATED miprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Placement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Placement
  ( Placement (..),

    -- * Smart constructor
    mkPlacement,

    -- * Lenses
    plaAffinity,
    plaHostId,
    plaPartitionNumber,
    plaSpreadDomain,
    plaAvailabilityZone,
    plaTenancy,
    plaGroupName,
    plaHostResourceGroupARN,
  )
where

import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement of an instance.
--
-- /See:/ 'mkPlacement' smart constructor.
data Placement = Placement'
  { affinity :: Lude.Maybe Lude.Text,
    hostId :: Lude.Maybe Lude.Text,
    partitionNumber :: Lude.Maybe Lude.Int,
    spreadDomain :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    tenancy :: Lude.Maybe Tenancy,
    groupName :: Lude.Maybe Lude.Text,
    hostResourceGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Placement' with the minimum fields required to make a request.
--
-- * 'affinity' - The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'availabilityZone' - The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'groupName' - The name of the placement group the instance is in.
-- * 'hostId' - The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'hostResourceGroupARN' - The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'partitionNumber' - The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'spreadDomain' - Reserved for future use.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
-- * 'tenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
mkPlacement ::
  Placement
mkPlacement =
  Placement'
    { affinity = Lude.Nothing,
      hostId = Lude.Nothing,
      partitionNumber = Lude.Nothing,
      spreadDomain = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      groupName = Lude.Nothing,
      hostResourceGroupARN = Lude.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaAffinity :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaAffinity = Lens.lens (affinity :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {affinity = a} :: Placement)
{-# DEPRECATED plaAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaHostId :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaHostId = Lens.lens (hostId :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {hostId = a} :: Placement)
{-# DEPRECATED plaHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaPartitionNumber :: Lens.Lens' Placement (Lude.Maybe Lude.Int)
plaPartitionNumber = Lens.lens (partitionNumber :: Placement -> Lude.Maybe Lude.Int) (\s a -> s {partitionNumber = a} :: Placement)
{-# DEPRECATED plaPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaSpreadDomain :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaSpreadDomain = Lens.lens (spreadDomain :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {spreadDomain = a} :: Placement)
{-# DEPRECATED plaSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaAvailabilityZone :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaAvailabilityZone = Lens.lens (availabilityZone :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Placement)
{-# DEPRECATED plaAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaTenancy :: Lens.Lens' Placement (Lude.Maybe Tenancy)
plaTenancy = Lens.lens (tenancy :: Placement -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: Placement)
{-# DEPRECATED plaTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The name of the placement group the instance is in.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaGroupName :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaGroupName = Lens.lens (groupName :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: Placement)
{-# DEPRECATED plaGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plaHostResourceGroupARN :: Lens.Lens' Placement (Lude.Maybe Lude.Text)
plaHostResourceGroupARN = Lens.lens (hostResourceGroupARN :: Placement -> Lude.Maybe Lude.Text) (\s a -> s {hostResourceGroupARN = a} :: Placement)
{-# DEPRECATED plaHostResourceGroupARN "Use generic-lens or generic-optics with 'hostResourceGroupARN' instead." #-}

instance Lude.FromXML Placement where
  parseXML x =
    Placement'
      Lude.<$> (x Lude..@? "affinity")
      Lude.<*> (x Lude..@? "hostId")
      Lude.<*> (x Lude..@? "partitionNumber")
      Lude.<*> (x Lude..@? "spreadDomain")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "tenancy")
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> (x Lude..@? "hostResourceGroupArn")

instance Lude.ToQuery Placement where
  toQuery Placement' {..} =
    Lude.mconcat
      [ "Affinity" Lude.=: affinity,
        "HostId" Lude.=: hostId,
        "PartitionNumber" Lude.=: partitionNumber,
        "SpreadDomain" Lude.=: spreadDomain,
        "AvailabilityZone" Lude.=: availabilityZone,
        "Tenancy" Lude.=: tenancy,
        "GroupName" Lude.=: groupName,
        "HostResourceGroupArn" Lude.=: hostResourceGroupARN
      ]

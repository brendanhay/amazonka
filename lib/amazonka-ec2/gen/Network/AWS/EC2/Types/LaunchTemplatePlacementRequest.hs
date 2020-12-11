-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
  ( LaunchTemplatePlacementRequest (..),

    -- * Smart constructor
    mkLaunchTemplatePlacementRequest,

    -- * Lenses
    ltprAffinity,
    ltprHostId,
    ltprPartitionNumber,
    ltprSpreadDomain,
    ltprAvailabilityZone,
    ltprTenancy,
    ltprGroupName,
    ltprHostResourceGroupARN,
  )
where

import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement of an instance.
--
-- /See:/ 'mkLaunchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { affinity ::
      Lude.Maybe Lude.Text,
    hostId ::
      Lude.Maybe Lude.Text,
    partitionNumber ::
      Lude.Maybe Lude.Int,
    spreadDomain ::
      Lude.Maybe Lude.Text,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    tenancy :: Lude.Maybe Tenancy,
    groupName ::
      Lude.Maybe Lude.Text,
    hostResourceGroupARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplatePlacementRequest' with the minimum fields required to make a request.
--
-- * 'affinity' - The affinity setting for an instance on a Dedicated Host.
-- * 'availabilityZone' - The Availability Zone for the instance.
-- * 'groupName' - The name of the placement group for the instance.
-- * 'hostId' - The ID of the Dedicated Host for the instance.
-- * 'hostResourceGroupARN' - The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
-- * 'partitionNumber' - The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
-- * 'spreadDomain' - Reserved for future use.
-- * 'tenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
mkLaunchTemplatePlacementRequest ::
  LaunchTemplatePlacementRequest
mkLaunchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { affinity = Lude.Nothing,
      hostId = Lude.Nothing,
      partitionNumber = Lude.Nothing,
      spreadDomain = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      groupName = Lude.Nothing,
      hostResourceGroupARN = Lude.Nothing
    }

-- | The affinity setting for an instance on a Dedicated Host.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprAffinity :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprAffinity = Lens.lens (affinity :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {affinity = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The ID of the Dedicated Host for the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprHostId :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprHostId = Lens.lens (hostId :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {hostId = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprPartitionNumber :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Int)
ltprPartitionNumber = Lens.lens (partitionNumber :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Int) (\s a -> s {partitionNumber = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprSpreadDomain :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprSpreadDomain = Lens.lens (spreadDomain :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {spreadDomain = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprAvailabilityZone :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprAvailabilityZone = Lens.lens (availabilityZone :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprTenancy :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Tenancy)
ltprTenancy = Lens.lens (tenancy :: LaunchTemplatePlacementRequest -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The name of the placement group for the instance.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprGroupName :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprGroupName = Lens.lens (groupName :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- /Note:/ Consider using 'hostResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprHostResourceGroupARN :: Lens.Lens' LaunchTemplatePlacementRequest (Lude.Maybe Lude.Text)
ltprHostResourceGroupARN = Lens.lens (hostResourceGroupARN :: LaunchTemplatePlacementRequest -> Lude.Maybe Lude.Text) (\s a -> s {hostResourceGroupARN = a} :: LaunchTemplatePlacementRequest)
{-# DEPRECATED ltprHostResourceGroupARN "Use generic-lens or generic-optics with 'hostResourceGroupARN' instead." #-}

instance Lude.ToQuery LaunchTemplatePlacementRequest where
  toQuery LaunchTemplatePlacementRequest' {..} =
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacement
  ( LaunchTemplatePlacement (..),

    -- * Smart constructor
    mkLaunchTemplatePlacement,

    -- * Lenses
    ltpAffinity,
    ltpHostId,
    ltpPartitionNumber,
    ltpSpreadDomain,
    ltpAvailabilityZone,
    ltpTenancy,
    ltpGroupName,
    ltpHostResourceGroupARN,
  )
where

import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the placement of an instance.
--
-- /See:/ 'mkLaunchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { affinity ::
      Lude.Maybe Lude.Text,
    hostId :: Lude.Maybe Lude.Text,
    partitionNumber :: Lude.Maybe Lude.Int,
    spreadDomain :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    tenancy :: Lude.Maybe Tenancy,
    groupName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LaunchTemplatePlacement' with the minimum fields required to make a request.
--
-- * 'affinity' - The affinity setting for the instance on the Dedicated Host.
-- * 'availabilityZone' - The Availability Zone of the instance.
-- * 'groupName' - The name of the placement group for the instance.
-- * 'hostId' - The ID of the Dedicated Host for the instance.
-- * 'hostResourceGroupARN' - The ARN of the host resource group in which to launch the instances.
-- * 'partitionNumber' - The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
-- * 'spreadDomain' - Reserved for future use.
-- * 'tenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
mkLaunchTemplatePlacement ::
  LaunchTemplatePlacement
mkLaunchTemplatePlacement =
  LaunchTemplatePlacement'
    { affinity = Lude.Nothing,
      hostId = Lude.Nothing,
      partitionNumber = Lude.Nothing,
      spreadDomain = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      groupName = Lude.Nothing,
      hostResourceGroupARN = Lude.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpAffinity :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpAffinity = Lens.lens (affinity :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {affinity = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The ID of the Dedicated Host for the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpHostId :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpHostId = Lens.lens (hostId :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {hostId = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpPartitionNumber :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Int)
ltpPartitionNumber = Lens.lens (partitionNumber :: LaunchTemplatePlacement -> Lude.Maybe Lude.Int) (\s a -> s {partitionNumber = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpSpreadDomain :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpSpreadDomain = Lens.lens (spreadDomain :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {spreadDomain = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The Availability Zone of the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpAvailabilityZone :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpAvailabilityZone = Lens.lens (availabilityZone :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpTenancy :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Tenancy)
ltpTenancy = Lens.lens (tenancy :: LaunchTemplatePlacement -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The name of the placement group for the instance.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpGroupName :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpGroupName = Lens.lens (groupName :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ARN of the host resource group in which to launch the instances.
--
-- /Note:/ Consider using 'hostResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpHostResourceGroupARN :: Lens.Lens' LaunchTemplatePlacement (Lude.Maybe Lude.Text)
ltpHostResourceGroupARN = Lens.lens (hostResourceGroupARN :: LaunchTemplatePlacement -> Lude.Maybe Lude.Text) (\s a -> s {hostResourceGroupARN = a} :: LaunchTemplatePlacement)
{-# DEPRECATED ltpHostResourceGroupARN "Use generic-lens or generic-optics with 'hostResourceGroupARN' instead." #-}

instance Lude.FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      Lude.<$> (x Lude..@? "affinity")
      Lude.<*> (x Lude..@? "hostId")
      Lude.<*> (x Lude..@? "partitionNumber")
      Lude.<*> (x Lude..@? "spreadDomain")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "tenancy")
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> (x Lude..@? "hostResourceGroupArn")

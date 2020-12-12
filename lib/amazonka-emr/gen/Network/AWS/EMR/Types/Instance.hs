{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iStatus,
    iPublicDNSName,
    iEBSVolumes,
    iEC2InstanceId,
    iInstanceType,
    iMarket,
    iPrivateIPAddress,
    iInstanceFleetId,
    iId,
    iInstanceGroupId,
    iPrivateDNSName,
    iPublicIPAddress,
  )
where

import Network.AWS.EMR.Types.EBSVolume
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an EC2 instance provisioned as part of cluster.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { status :: Lude.Maybe InstanceStatus,
    publicDNSName :: Lude.Maybe Lude.Text,
    ebsVolumes :: Lude.Maybe [EBSVolume],
    ec2InstanceId :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    market :: Lude.Maybe MarketType,
    privateIPAddress :: Lude.Maybe Lude.Text,
    instanceFleetId :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    instanceGroupId :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
    publicIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'ebsVolumes' - The list of EBS volumes that are attached to this instance.
-- * 'ec2InstanceId' - The unique identifier of the instance in Amazon EC2.
-- * 'id' - The unique identifier for the instance in Amazon EMR.
-- * 'instanceFleetId' - The unique identifier of the instance fleet to which an EC2 instance belongs.
-- * 'instanceGroupId' - The identifier of the instance group to which this instance belongs.
-- * 'instanceType' - The EC2 instance type, for example @m3.xlarge@ .
-- * 'market' - The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
-- * 'privateDNSName' - The private DNS name of the instance.
-- * 'privateIPAddress' - The private IP address of the instance.
-- * 'publicDNSName' - The public DNS name of the instance.
-- * 'publicIPAddress' - The public IP address of the instance.
-- * 'status' - The current status of the instance.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { status = Lude.Nothing,
      publicDNSName = Lude.Nothing,
      ebsVolumes = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      instanceType = Lude.Nothing,
      market = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      instanceFleetId = Lude.Nothing,
      id = Lude.Nothing,
      instanceGroupId = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      publicIPAddress = Lude.Nothing
    }

-- | The current status of the instance.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Instance (Lude.Maybe InstanceStatus)
iStatus = Lens.lens (status :: Instance -> Lude.Maybe InstanceStatus) (\s a -> s {status = a} :: Instance)
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The public DNS name of the instance.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPublicDNSName = Lens.lens (publicDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: Instance)
{-# DEPRECATED iPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The list of EBS volumes that are attached to this instance.
--
-- /Note:/ Consider using 'ebsVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEBSVolumes :: Lens.Lens' Instance (Lude.Maybe [EBSVolume])
iEBSVolumes = Lens.lens (ebsVolumes :: Instance -> Lude.Maybe [EBSVolume]) (\s a -> s {ebsVolumes = a} :: Instance)
{-# DEPRECATED iEBSVolumes "Use generic-lens or generic-optics with 'ebsVolumes' instead." #-}

-- | The unique identifier of the instance in Amazon EC2.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEC2InstanceId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iEC2InstanceId = Lens.lens (ec2InstanceId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: Instance)
{-# DEPRECATED iEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The EC2 instance type, for example @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceType = Lens.lens (instanceType :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: Instance)
{-# DEPRECATED iInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMarket :: Lens.Lens' Instance (Lude.Maybe MarketType)
iMarket = Lens.lens (market :: Instance -> Lude.Maybe MarketType) (\s a -> s {market = a} :: Instance)
{-# DEPRECATED iMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | The private IP address of the instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPrivateIPAddress = Lens.lens (privateIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: Instance)
{-# DEPRECATED iPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The unique identifier of the instance fleet to which an EC2 instance belongs.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceFleetId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceFleetId = Lens.lens (instanceFleetId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceFleetId = a} :: Instance)
{-# DEPRECATED iInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The unique identifier for the instance in Amazon EMR.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iId = Lens.lens (id :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Instance)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The identifier of the instance group to which this instance belongs.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceGroupId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceGroupId = Lens.lens (instanceGroupId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceGroupId = a} :: Instance)
{-# DEPRECATED iInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | The private DNS name of the instance.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPrivateDNSName = Lens.lens (privateDNSName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: Instance)
{-# DEPRECATED iPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The public IP address of the instance.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPublicIPAddress = Lens.lens (publicIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicIPAddress = a} :: Instance)
{-# DEPRECATED iPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PublicDnsName")
            Lude.<*> (x Lude..:? "EbsVolumes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Ec2InstanceId")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "Market")
            Lude.<*> (x Lude..:? "PrivateIpAddress")
            Lude.<*> (x Lude..:? "InstanceFleetId")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "InstanceGroupId")
            Lude.<*> (x Lude..:? "PrivateDnsName")
            Lude.<*> (x Lude..:? "PublicIpAddress")
      )

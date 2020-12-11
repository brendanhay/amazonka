-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainController
  ( DomainController (..),

    -- * Smart constructor
    mkDomainController,

    -- * Lenses
    dcStatus,
    dcDirectoryId,
    dcVPCId,
    dcLaunchTime,
    dcSubnetId,
    dcAvailabilityZone,
    dcStatusLastUpdatedDateTime,
    dcStatusReason,
    dcDNSIPAddr,
    dcDomainControllerId,
  )
where

import Network.AWS.DirectoryService.Types.DomainControllerStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the domain controllers for a specified directory.
--
-- /See:/ 'mkDomainController' smart constructor.
data DomainController = DomainController'
  { status ::
      Lude.Maybe DomainControllerStatus,
    directoryId :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    launchTime :: Lude.Maybe Lude.Timestamp,
    subnetId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    statusLastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    statusReason :: Lude.Maybe Lude.Text,
    dnsIPAddr :: Lude.Maybe Lude.Text,
    domainControllerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainController' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone where the domain controller is located.
-- * 'directoryId' - Identifier of the directory where the domain controller resides.
-- * 'dnsIPAddr' - The IP address of the domain controller.
-- * 'domainControllerId' - Identifies a specific domain controller in the directory.
-- * 'launchTime' - Specifies when the domain controller was created.
-- * 'status' - The status of the domain controller.
-- * 'statusLastUpdatedDateTime' - The date and time that the status was last updated.
-- * 'statusReason' - A description of the domain controller state.
-- * 'subnetId' - Identifier of the subnet in the VPC that contains the domain controller.
-- * 'vpcId' - The identifier of the VPC that contains the domain controller.
mkDomainController ::
  DomainController
mkDomainController =
  DomainController'
    { status = Lude.Nothing,
      directoryId = Lude.Nothing,
      vpcId = Lude.Nothing,
      launchTime = Lude.Nothing,
      subnetId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      statusLastUpdatedDateTime = Lude.Nothing,
      statusReason = Lude.Nothing,
      dnsIPAddr = Lude.Nothing,
      domainControllerId = Lude.Nothing
    }

-- | The status of the domain controller.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatus :: Lens.Lens' DomainController (Lude.Maybe DomainControllerStatus)
dcStatus = Lens.lens (status :: DomainController -> Lude.Maybe DomainControllerStatus) (\s a -> s {status = a} :: DomainController)
{-# DEPRECATED dcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Identifier of the directory where the domain controller resides.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDirectoryId :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcDirectoryId = Lens.lens (directoryId :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DomainController)
{-# DEPRECATED dcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifier of the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVPCId :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcVPCId = Lens.lens (vpcId :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DomainController)
{-# DEPRECATED dcVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Specifies when the domain controller was created.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLaunchTime :: Lens.Lens' DomainController (Lude.Maybe Lude.Timestamp)
dcLaunchTime = Lens.lens (launchTime :: DomainController -> Lude.Maybe Lude.Timestamp) (\s a -> s {launchTime = a} :: DomainController)
{-# DEPRECATED dcLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | Identifier of the subnet in the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSubnetId :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcSubnetId = Lens.lens (subnetId :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DomainController)
{-# DEPRECATED dcSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Availability Zone where the domain controller is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAvailabilityZone :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcAvailabilityZone = Lens.lens (availabilityZone :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DomainController)
{-# DEPRECATED dcAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time that the status was last updated.
--
-- /Note:/ Consider using 'statusLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusLastUpdatedDateTime :: Lens.Lens' DomainController (Lude.Maybe Lude.Timestamp)
dcStatusLastUpdatedDateTime = Lens.lens (statusLastUpdatedDateTime :: DomainController -> Lude.Maybe Lude.Timestamp) (\s a -> s {statusLastUpdatedDateTime = a} :: DomainController)
{-# DEPRECATED dcStatusLastUpdatedDateTime "Use generic-lens or generic-optics with 'statusLastUpdatedDateTime' instead." #-}

-- | A description of the domain controller state.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusReason :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcStatusReason = Lens.lens (statusReason :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: DomainController)
{-# DEPRECATED dcStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The IP address of the domain controller.
--
-- /Note:/ Consider using 'dnsIPAddr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDNSIPAddr :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcDNSIPAddr = Lens.lens (dnsIPAddr :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {dnsIPAddr = a} :: DomainController)
{-# DEPRECATED dcDNSIPAddr "Use generic-lens or generic-optics with 'dnsIPAddr' instead." #-}

-- | Identifies a specific domain controller in the directory.
--
-- /Note:/ Consider using 'domainControllerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDomainControllerId :: Lens.Lens' DomainController (Lude.Maybe Lude.Text)
dcDomainControllerId = Lens.lens (domainControllerId :: DomainController -> Lude.Maybe Lude.Text) (\s a -> s {domainControllerId = a} :: DomainController)
{-# DEPRECATED dcDomainControllerId "Use generic-lens or generic-optics with 'domainControllerId' instead." #-}

instance Lude.FromJSON DomainController where
  parseJSON =
    Lude.withObject
      "DomainController"
      ( \x ->
          DomainController'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "LaunchTime")
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "StatusLastUpdatedDateTime")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "DnsIpAddr")
            Lude.<*> (x Lude..:? "DomainControllerId")
      )

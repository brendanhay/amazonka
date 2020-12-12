{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iCreationTime,
    iInstanceId,
    iStatus,
    iIPAddress,
    iOperatingSystem,
    iType,
    iFleetId,
    iDNSName,
  )
where

import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.InstanceStatus
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    instanceId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe InstanceStatus,
    ipAddress :: Lude.Maybe Lude.Text,
    operatingSystem :: Lude.Maybe OperatingSystem,
    type' :: Lude.Maybe EC2InstanceType,
    fleetId :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text
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
-- * 'creationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'dnsName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
-- * 'fleetId' - A unique identifier for a fleet that the instance is in.
-- * 'instanceId' - A unique identifier for an instance.
-- * 'ipAddress' - IP address that is assigned to the instance.
-- * 'operatingSystem' - Operating system that is running on this instance.
-- * 'status' - Current status of the instance. Possible statuses include the following:
--
--
--     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.
--
--
--     * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.
--
--
--     * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
--
--
-- * 'type'' - EC2 instance type that defines the computing resources of this instance.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { creationTime = Lude.Nothing,
      instanceId = Lude.Nothing,
      status = Lude.Nothing,
      ipAddress = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      type' = Lude.Nothing,
      fleetId = Lude.Nothing,
      dnsName = Lude.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreationTime :: Lens.Lens' Instance (Lude.Maybe Lude.Timestamp)
iCreationTime = Lens.lens (creationTime :: Instance -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Instance)
{-# DEPRECATED iCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A unique identifier for an instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iInstanceId = Lens.lens (instanceId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Instance)
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Current status of the instance. Possible statuses include the following:
--
--
--     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.
--
--
--     * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.
--
--
--     * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Instance (Lude.Maybe InstanceStatus)
iStatus = Lens.lens (status :: Instance -> Lude.Maybe InstanceStatus) (\s a -> s {status = a} :: Instance)
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | IP address that is assigned to the instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iIPAddress = Lens.lens (ipAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: Instance)
{-# DEPRECATED iIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Operating system that is running on this instance.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOperatingSystem :: Lens.Lens' Instance (Lude.Maybe OperatingSystem)
iOperatingSystem = Lens.lens (operatingSystem :: Instance -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: Instance)
{-# DEPRECATED iOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | EC2 instance type that defines the computing resources of this instance.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Instance (Lude.Maybe EC2InstanceType)
iType = Lens.lens (type' :: Instance -> Lude.Maybe EC2InstanceType) (\s a -> s {type' = a} :: Instance)
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A unique identifier for a fleet that the instance is in.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFleetId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iFleetId = Lens.lens (fleetId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: Instance)
{-# DEPRECATED iFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDNSName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iDNSName = Lens.lens (dnsName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: Instance)
{-# DEPRECATED iDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "DnsName")
      )

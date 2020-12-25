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
    iDnsName,
    iFleetId,
    iInstanceId,
    iIpAddress,
    iOperatingSystem,
    iStatus,
    iType,
  )
where

import qualified Network.AWS.GameLift.Types.DnsName as Types
import qualified Network.AWS.GameLift.Types.EC2InstanceType as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.InstanceId as Types
import qualified Network.AWS.GameLift.Types.InstanceStatus as Types
import qualified Network.AWS.GameLift.Types.IpAddress as Types
import qualified Network.AWS.GameLift.Types.OperatingSystem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Core.Maybe Core.NominalDiffTime,
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
    dnsName :: Core.Maybe Types.DnsName,
    -- | A unique identifier for a fleet that the instance is in.
    fleetId :: Core.Maybe Types.FleetId,
    -- | A unique identifier for an instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | IP address that is assigned to the instance.
    ipAddress :: Core.Maybe Types.IpAddress,
    -- | Operating system that is running on this instance.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
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
    status :: Core.Maybe Types.InstanceStatus,
    -- | EC2 instance type that defines the computing resources of this instance.
    type' :: Core.Maybe Types.EC2InstanceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { creationTime = Core.Nothing,
      dnsName = Core.Nothing,
      fleetId = Core.Nothing,
      instanceId = Core.Nothing,
      ipAddress = Core.Nothing,
      operatingSystem = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreationTime :: Lens.Lens' Instance (Core.Maybe Core.NominalDiffTime)
iCreationTime = Lens.field @"creationTime"
{-# DEPRECATED iCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
iDnsName :: Lens.Lens' Instance (Core.Maybe Types.DnsName)
iDnsName = Lens.field @"dnsName"
{-# DEPRECATED iDnsName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | A unique identifier for a fleet that the instance is in.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iFleetId :: Lens.Lens' Instance (Core.Maybe Types.FleetId)
iFleetId = Lens.field @"fleetId"
{-# DEPRECATED iFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | A unique identifier for an instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | IP address that is assigned to the instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIpAddress :: Lens.Lens' Instance (Core.Maybe Types.IpAddress)
iIpAddress = Lens.field @"ipAddress"
{-# DEPRECATED iIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Operating system that is running on this instance.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOperatingSystem :: Lens.Lens' Instance (Core.Maybe Types.OperatingSystem)
iOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED iOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

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
iStatus :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatus)
iStatus = Lens.field @"status"
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | EC2 instance type that defines the computing resources of this instance.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Instance (Core.Maybe Types.EC2InstanceType)
iType = Lens.field @"type'"
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject "Instance" Core.$
      \x ->
        Instance'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DnsName")
          Core.<*> (x Core..:? "FleetId")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "IpAddress")
          Core.<*> (x Core..:? "OperatingSystem")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Type")

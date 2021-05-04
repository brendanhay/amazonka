{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Instance where

import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.InstanceStatus
import Network.AWS.GameLift.Types.OperatingSystem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Properties that describe an instance of a virtual computing resource
-- that hosts one or more game servers. A fleet may contain zero or more
-- instances.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | Current status of the instance. Possible statuses include the following:
    --
    -- -   __PENDING__ -- The instance is in the process of being created and
    --     launching server processes as defined in the fleet\'s run-time
    --     configuration.
    --
    -- -   __ACTIVE__ -- The instance has been successfully created and at
    --     least one server process has successfully launched and reported back
    --     to Amazon GameLift that it is ready to host a game session. The
    --     instance is now considered ready to host game sessions.
    --
    -- -   __TERMINATING__ -- The instance is in the process of shutting down.
    --     This may happen to reduce capacity during a scaling down event or to
    --     recycle resources in the event of a problem.
    status :: Prelude.Maybe InstanceStatus,
    -- | A unique identifier for an instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A unique identifier for a fleet that the instance is in.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | IP address that is assigned to the instance.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | DNS identifier assigned to the instance that is running the game
    -- session. Values have the following format:
    --
    -- -   TLS-enabled fleets:
    --     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
    --
    -- -   Non-TLS-enabled fleets:
    --     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
    --
    -- When connecting to a game session that is running on a TLS-enabled
    -- fleet, you must use the DNS name, not the IP address.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | EC2 instance type that defines the computing resources of this instance.
    type' :: Prelude.Maybe EC2InstanceType,
    -- | Operating system that is running on this instance.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'instance_status' - Current status of the instance. Possible statuses include the following:
--
-- -   __PENDING__ -- The instance is in the process of being created and
--     launching server processes as defined in the fleet\'s run-time
--     configuration.
--
-- -   __ACTIVE__ -- The instance has been successfully created and at
--     least one server process has successfully launched and reported back
--     to Amazon GameLift that it is ready to host a game session. The
--     instance is now considered ready to host game sessions.
--
-- -   __TERMINATING__ -- The instance is in the process of shutting down.
--     This may happen to reduce capacity during a scaling down event or to
--     recycle resources in the event of a problem.
--
-- 'instanceId', 'instance_instanceId' - A unique identifier for an instance.
--
-- 'creationTime', 'instance_creationTime' - Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'fleetId', 'instance_fleetId' - A unique identifier for a fleet that the instance is in.
--
-- 'ipAddress', 'instance_ipAddress' - IP address that is assigned to the instance.
--
-- 'dnsName', 'instance_dnsName' - DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
--
-- 'type'', 'instance_type' - EC2 instance type that defines the computing resources of this instance.
--
-- 'operatingSystem', 'instance_operatingSystem' - Operating system that is running on this instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { status = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      type' = Prelude.Nothing,
      operatingSystem = Prelude.Nothing
    }

-- | Current status of the instance. Possible statuses include the following:
--
-- -   __PENDING__ -- The instance is in the process of being created and
--     launching server processes as defined in the fleet\'s run-time
--     configuration.
--
-- -   __ACTIVE__ -- The instance has been successfully created and at
--     least one server process has successfully launched and reported back
--     to Amazon GameLift that it is ready to host a game session. The
--     instance is now considered ready to host game sessions.
--
-- -   __TERMINATING__ -- The instance is in the process of shutting down.
--     This may happen to reduce capacity during a scaling down event or to
--     recycle resources in the event of a problem.
instance_status :: Lens.Lens' Instance (Prelude.Maybe InstanceStatus)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | A unique identifier for an instance.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
instance_creationTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_creationTime = Lens.lens (\Instance' {creationTime} -> creationTime) (\s@Instance' {} a -> s {creationTime = a} :: Instance) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier for a fleet that the instance is in.
instance_fleetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_fleetId = Lens.lens (\Instance' {fleetId} -> fleetId) (\s@Instance' {} a -> s {fleetId = a} :: Instance)

-- | IP address that is assigned to the instance.
instance_ipAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ipAddress = Lens.lens (\Instance' {ipAddress} -> ipAddress) (\s@Instance' {} a -> s {ipAddress = a} :: Instance)

-- | DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
instance_dnsName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_dnsName = Lens.lens (\Instance' {dnsName} -> dnsName) (\s@Instance' {} a -> s {dnsName = a} :: Instance)

-- | EC2 instance type that defines the computing resources of this instance.
instance_type :: Lens.Lens' Instance (Prelude.Maybe EC2InstanceType)
instance_type = Lens.lens (\Instance' {type'} -> type') (\s@Instance' {} a -> s {type' = a} :: Instance)

-- | Operating system that is running on this instance.
instance_operatingSystem :: Lens.Lens' Instance (Prelude.Maybe OperatingSystem)
instance_operatingSystem = Lens.lens (\Instance' {operatingSystem} -> operatingSystem) (\s@Instance' {} a -> s {operatingSystem = a} :: Instance)

instance Prelude.FromJSON Instance where
  parseJSON =
    Prelude.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "DnsName")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "OperatingSystem")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance

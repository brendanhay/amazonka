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
-- Module      : Amazonka.GameLift.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.EC2InstanceType
import Amazonka.GameLift.Types.InstanceStatus
import Amazonka.GameLift.Types.OperatingSystem
import qualified Amazonka.Prelude as Prelude

-- | Represents an EC2 instance of virtual computing resources that hosts one
-- or more game servers. In GameLift, a fleet can contain zero or more
-- instances.
--
-- __Related actions__
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The DNS identifier assigned to the instance that is running the game
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
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that the instance is in.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | IP address that is assigned to the instance.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The fleet location of the instance, expressed as an Amazon Web Services
    -- Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | Operating system that is running on this instance.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | Current status of the instance. Possible statuses include the following:
    --
    -- -   __PENDING__ -- The instance is in the process of being created and
    --     launching server processes as defined in the fleet\'s run-time
    --     configuration.
    --
    -- -   __ACTIVE__ -- The instance has been successfully created and at
    --     least one server process has successfully launched and reported back
    --     to GameLift that it is ready to host a game session. The instance is
    --     now considered ready to host game sessions.
    --
    -- -   __TERMINATING__ -- The instance is in the process of shutting down.
    --     This may happen to reduce capacity during a scaling down event or to
    --     recycle resources in the event of a problem.
    status :: Prelude.Maybe InstanceStatus,
    -- | Amazon EC2 instance type that defines the computing resources of this
    -- instance.
    type' :: Prelude.Maybe EC2InstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'instance_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'dnsName', 'instance_dnsName' - The DNS identifier assigned to the instance that is running the game
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
-- 'fleetArn', 'instance_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'instance_fleetId' - A unique identifier for the fleet that the instance is in.
--
-- 'instanceId', 'instance_instanceId' - A unique identifier for the instance.
--
-- 'ipAddress', 'instance_ipAddress' - IP address that is assigned to the instance.
--
-- 'location', 'instance_location' - The fleet location of the instance, expressed as an Amazon Web Services
-- Region code, such as @us-west-2@.
--
-- 'operatingSystem', 'instance_operatingSystem' - Operating system that is running on this instance.
--
-- 'status', 'instance_status' - Current status of the instance. Possible statuses include the following:
--
-- -   __PENDING__ -- The instance is in the process of being created and
--     launching server processes as defined in the fleet\'s run-time
--     configuration.
--
-- -   __ACTIVE__ -- The instance has been successfully created and at
--     least one server process has successfully launched and reported back
--     to GameLift that it is ready to host a game session. The instance is
--     now considered ready to host game sessions.
--
-- -   __TERMINATING__ -- The instance is in the process of shutting down.
--     This may happen to reduce capacity during a scaling down event or to
--     recycle resources in the event of a problem.
--
-- 'type'', 'instance_type' - Amazon EC2 instance type that defines the computing resources of this
-- instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { creationTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      location = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
instance_creationTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_creationTime = Lens.lens (\Instance' {creationTime} -> creationTime) (\s@Instance' {} a -> s {creationTime = a} :: Instance) Prelude.. Lens.mapping Data._Time

-- | The DNS identifier assigned to the instance that is running the game
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

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
instance_fleetArn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_fleetArn = Lens.lens (\Instance' {fleetArn} -> fleetArn) (\s@Instance' {} a -> s {fleetArn = a} :: Instance)

-- | A unique identifier for the fleet that the instance is in.
instance_fleetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_fleetId = Lens.lens (\Instance' {fleetId} -> fleetId) (\s@Instance' {} a -> s {fleetId = a} :: Instance)

-- | A unique identifier for the instance.
instance_instanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceId = Lens.lens (\Instance' {instanceId} -> instanceId) (\s@Instance' {} a -> s {instanceId = a} :: Instance)

-- | IP address that is assigned to the instance.
instance_ipAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ipAddress = Lens.lens (\Instance' {ipAddress} -> ipAddress) (\s@Instance' {} a -> s {ipAddress = a} :: Instance)

-- | The fleet location of the instance, expressed as an Amazon Web Services
-- Region code, such as @us-west-2@.
instance_location :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_location = Lens.lens (\Instance' {location} -> location) (\s@Instance' {} a -> s {location = a} :: Instance)

-- | Operating system that is running on this instance.
instance_operatingSystem :: Lens.Lens' Instance (Prelude.Maybe OperatingSystem)
instance_operatingSystem = Lens.lens (\Instance' {operatingSystem} -> operatingSystem) (\s@Instance' {} a -> s {operatingSystem = a} :: Instance)

-- | Current status of the instance. Possible statuses include the following:
--
-- -   __PENDING__ -- The instance is in the process of being created and
--     launching server processes as defined in the fleet\'s run-time
--     configuration.
--
-- -   __ACTIVE__ -- The instance has been successfully created and at
--     least one server process has successfully launched and reported back
--     to GameLift that it is ready to host a game session. The instance is
--     now considered ready to host game sessions.
--
-- -   __TERMINATING__ -- The instance is in the process of shutting down.
--     This may happen to reduce capacity during a scaling down event or to
--     recycle resources in the event of a problem.
instance_status :: Lens.Lens' Instance (Prelude.Maybe InstanceStatus)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | Amazon EC2 instance type that defines the computing resources of this
-- instance.
instance_type :: Lens.Lens' Instance (Prelude.Maybe EC2InstanceType)
instance_type = Lens.lens (\Instance' {type'} -> type') (\s@Instance' {} a -> s {type' = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "OperatingSystem")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'

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
-- Module      : Network.AWS.Lightsail.Types.InstanceEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceEntry where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.PortInfoSourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon Elastic Compute Cloud instance and related
-- resources to be created using the @create cloud formation stack@
-- operation.
--
-- /See:/ 'newInstanceEntry' smart constructor.
data InstanceEntry = InstanceEntry'
  { -- | A launch script you can create that configures a server with additional
    -- user data. For example, you might want to run @apt-get -y update@.
    --
    -- Depending on the machine image you choose, the command to get software
    -- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
    -- Ubuntu use @apt-get@, and FreeBSD uses @pkg@.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The name of the export snapshot record, which contains the exported
    -- Lightsail instance snapshot that will be used as the source of the new
    -- Amazon EC2 instance.
    --
    -- Use the @get export snapshot records@ operation to get a list of export
    -- snapshot records that you can use to create a CloudFormation stack.
    sourceName :: Prelude.Text,
    -- | The instance type (e.g., @t2.micro@) to use for the new Amazon EC2
    -- instance.
    instanceType :: Prelude.Text,
    -- | The port configuration to use for the new Amazon EC2 instance.
    --
    -- The following configuration options are available:
    --
    -- -   @DEFAULT@ - Use the default firewall settings from the Lightsail
    --     instance blueprint. If this is specified, then IPv4 and IPv6 will be
    --     configured for the new instance that is created in Amazon EC2.
    --
    -- -   @INSTANCE@ - Use the configured firewall settings from the source
    --     Lightsail instance. If this is specified, the new instance that is
    --     created in Amazon EC2 will be configured to match the configuration
    --     of the source Lightsail instance. For example, if the source
    --     instance is configured for dual-stack (IPv4 and IPv6), then IPv4 and
    --     IPv6 will be configured for the new instance that is created in
    --     Amazon EC2. If the source instance is configured for IPv4 only, then
    --     only IPv4 will be configured for the new instance that is created in
    --     Amazon EC2.
    --
    -- -   @NONE@ - Use the default Amazon EC2 security group. If this is
    --     specified, then only IPv4 will be configured for the new instance
    --     that is created in Amazon EC2.
    --
    -- -   @CLOSED@ - All ports closed. If this is specified, then only IPv4
    --     will be configured for the new instance that is created in Amazon
    --     EC2.
    --
    -- If you configured @lightsail-connect@ as a @cidrListAliases@ on your
    -- instance, or if you chose to allow the Lightsail browser-based SSH or
    -- RDP clients to connect to your instance, that configuration is not
    -- carried over to your new Amazon EC2 instance.
    portInfoSource :: PortInfoSourceType,
    -- | The Availability Zone for the new Amazon EC2 instance.
    availabilityZone :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userData', 'instanceEntry_userData' - A launch script you can create that configures a server with additional
-- user data. For example, you might want to run @apt-get -y update@.
--
-- Depending on the machine image you choose, the command to get software
-- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
-- Ubuntu use @apt-get@, and FreeBSD uses @pkg@.
--
-- 'sourceName', 'instanceEntry_sourceName' - The name of the export snapshot record, which contains the exported
-- Lightsail instance snapshot that will be used as the source of the new
-- Amazon EC2 instance.
--
-- Use the @get export snapshot records@ operation to get a list of export
-- snapshot records that you can use to create a CloudFormation stack.
--
-- 'instanceType', 'instanceEntry_instanceType' - The instance type (e.g., @t2.micro@) to use for the new Amazon EC2
-- instance.
--
-- 'portInfoSource', 'instanceEntry_portInfoSource' - The port configuration to use for the new Amazon EC2 instance.
--
-- The following configuration options are available:
--
-- -   @DEFAULT@ - Use the default firewall settings from the Lightsail
--     instance blueprint. If this is specified, then IPv4 and IPv6 will be
--     configured for the new instance that is created in Amazon EC2.
--
-- -   @INSTANCE@ - Use the configured firewall settings from the source
--     Lightsail instance. If this is specified, the new instance that is
--     created in Amazon EC2 will be configured to match the configuration
--     of the source Lightsail instance. For example, if the source
--     instance is configured for dual-stack (IPv4 and IPv6), then IPv4 and
--     IPv6 will be configured for the new instance that is created in
--     Amazon EC2. If the source instance is configured for IPv4 only, then
--     only IPv4 will be configured for the new instance that is created in
--     Amazon EC2.
--
-- -   @NONE@ - Use the default Amazon EC2 security group. If this is
--     specified, then only IPv4 will be configured for the new instance
--     that is created in Amazon EC2.
--
-- -   @CLOSED@ - All ports closed. If this is specified, then only IPv4
--     will be configured for the new instance that is created in Amazon
--     EC2.
--
-- If you configured @lightsail-connect@ as a @cidrListAliases@ on your
-- instance, or if you chose to allow the Lightsail browser-based SSH or
-- RDP clients to connect to your instance, that configuration is not
-- carried over to your new Amazon EC2 instance.
--
-- 'availabilityZone', 'instanceEntry_availabilityZone' - The Availability Zone for the new Amazon EC2 instance.
newInstanceEntry ::
  -- | 'sourceName'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'portInfoSource'
  PortInfoSourceType ->
  -- | 'availabilityZone'
  Prelude.Text ->
  InstanceEntry
newInstanceEntry
  pSourceName_
  pInstanceType_
  pPortInfoSource_
  pAvailabilityZone_ =
    InstanceEntry'
      { userData = Prelude.Nothing,
        sourceName = pSourceName_,
        instanceType = pInstanceType_,
        portInfoSource = pPortInfoSource_,
        availabilityZone = pAvailabilityZone_
      }

-- | A launch script you can create that configures a server with additional
-- user data. For example, you might want to run @apt-get -y update@.
--
-- Depending on the machine image you choose, the command to get software
-- on your instance varies. Amazon Linux and CentOS use @yum@, Debian and
-- Ubuntu use @apt-get@, and FreeBSD uses @pkg@.
instanceEntry_userData :: Lens.Lens' InstanceEntry (Prelude.Maybe Prelude.Text)
instanceEntry_userData = Lens.lens (\InstanceEntry' {userData} -> userData) (\s@InstanceEntry' {} a -> s {userData = a} :: InstanceEntry)

-- | The name of the export snapshot record, which contains the exported
-- Lightsail instance snapshot that will be used as the source of the new
-- Amazon EC2 instance.
--
-- Use the @get export snapshot records@ operation to get a list of export
-- snapshot records that you can use to create a CloudFormation stack.
instanceEntry_sourceName :: Lens.Lens' InstanceEntry Prelude.Text
instanceEntry_sourceName = Lens.lens (\InstanceEntry' {sourceName} -> sourceName) (\s@InstanceEntry' {} a -> s {sourceName = a} :: InstanceEntry)

-- | The instance type (e.g., @t2.micro@) to use for the new Amazon EC2
-- instance.
instanceEntry_instanceType :: Lens.Lens' InstanceEntry Prelude.Text
instanceEntry_instanceType = Lens.lens (\InstanceEntry' {instanceType} -> instanceType) (\s@InstanceEntry' {} a -> s {instanceType = a} :: InstanceEntry)

-- | The port configuration to use for the new Amazon EC2 instance.
--
-- The following configuration options are available:
--
-- -   @DEFAULT@ - Use the default firewall settings from the Lightsail
--     instance blueprint. If this is specified, then IPv4 and IPv6 will be
--     configured for the new instance that is created in Amazon EC2.
--
-- -   @INSTANCE@ - Use the configured firewall settings from the source
--     Lightsail instance. If this is specified, the new instance that is
--     created in Amazon EC2 will be configured to match the configuration
--     of the source Lightsail instance. For example, if the source
--     instance is configured for dual-stack (IPv4 and IPv6), then IPv4 and
--     IPv6 will be configured for the new instance that is created in
--     Amazon EC2. If the source instance is configured for IPv4 only, then
--     only IPv4 will be configured for the new instance that is created in
--     Amazon EC2.
--
-- -   @NONE@ - Use the default Amazon EC2 security group. If this is
--     specified, then only IPv4 will be configured for the new instance
--     that is created in Amazon EC2.
--
-- -   @CLOSED@ - All ports closed. If this is specified, then only IPv4
--     will be configured for the new instance that is created in Amazon
--     EC2.
--
-- If you configured @lightsail-connect@ as a @cidrListAliases@ on your
-- instance, or if you chose to allow the Lightsail browser-based SSH or
-- RDP clients to connect to your instance, that configuration is not
-- carried over to your new Amazon EC2 instance.
instanceEntry_portInfoSource :: Lens.Lens' InstanceEntry PortInfoSourceType
instanceEntry_portInfoSource = Lens.lens (\InstanceEntry' {portInfoSource} -> portInfoSource) (\s@InstanceEntry' {} a -> s {portInfoSource = a} :: InstanceEntry)

-- | The Availability Zone for the new Amazon EC2 instance.
instanceEntry_availabilityZone :: Lens.Lens' InstanceEntry Prelude.Text
instanceEntry_availabilityZone = Lens.lens (\InstanceEntry' {availabilityZone} -> availabilityZone) (\s@InstanceEntry' {} a -> s {availabilityZone = a} :: InstanceEntry)

instance Prelude.Hashable InstanceEntry

instance Prelude.NFData InstanceEntry

instance Prelude.ToJSON InstanceEntry where
  toJSON InstanceEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("userData" Prelude..=) Prelude.<$> userData,
            Prelude.Just ("sourceName" Prelude..= sourceName),
            Prelude.Just
              ("instanceType" Prelude..= instanceType),
            Prelude.Just
              ("portInfoSource" Prelude..= portInfoSource),
            Prelude.Just
              ("availabilityZone" Prelude..= availabilityZone)
          ]
      )

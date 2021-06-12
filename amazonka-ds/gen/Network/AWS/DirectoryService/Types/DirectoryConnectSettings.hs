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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information for the ConnectDirectory operation when an AD
-- Connector directory is being created.
--
-- /See:/ 'newDirectoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { -- | The identifier of the VPC in which the AD Connector is created.
    vpcId :: Core.Text,
    -- | A list of subnet identifiers in the VPC in which the AD Connector is
    -- created.
    subnetIds :: [Core.Text],
    -- | A list of one or more IP addresses of DNS servers or domain controllers
    -- in the on-premises directory.
    customerDnsIps :: [Core.Text],
    -- | The user name of an account in the on-premises directory that is used to
    -- connect to the directory. This account must have the following
    -- permissions:
    --
    -- -   Read users and groups
    --
    -- -   Create computer objects
    --
    -- -   Join computers to the domain
    customerUserName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectoryConnectSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'directoryConnectSettings_vpcId' - The identifier of the VPC in which the AD Connector is created.
--
-- 'subnetIds', 'directoryConnectSettings_subnetIds' - A list of subnet identifiers in the VPC in which the AD Connector is
-- created.
--
-- 'customerDnsIps', 'directoryConnectSettings_customerDnsIps' - A list of one or more IP addresses of DNS servers or domain controllers
-- in the on-premises directory.
--
-- 'customerUserName', 'directoryConnectSettings_customerUserName' - The user name of an account in the on-premises directory that is used to
-- connect to the directory. This account must have the following
-- permissions:
--
-- -   Read users and groups
--
-- -   Create computer objects
--
-- -   Join computers to the domain
newDirectoryConnectSettings ::
  -- | 'vpcId'
  Core.Text ->
  -- | 'customerUserName'
  Core.Text ->
  DirectoryConnectSettings
newDirectoryConnectSettings
  pVpcId_
  pCustomerUserName_ =
    DirectoryConnectSettings'
      { vpcId = pVpcId_,
        subnetIds = Core.mempty,
        customerDnsIps = Core.mempty,
        customerUserName = pCustomerUserName_
      }

-- | The identifier of the VPC in which the AD Connector is created.
directoryConnectSettings_vpcId :: Lens.Lens' DirectoryConnectSettings Core.Text
directoryConnectSettings_vpcId = Lens.lens (\DirectoryConnectSettings' {vpcId} -> vpcId) (\s@DirectoryConnectSettings' {} a -> s {vpcId = a} :: DirectoryConnectSettings)

-- | A list of subnet identifiers in the VPC in which the AD Connector is
-- created.
directoryConnectSettings_subnetIds :: Lens.Lens' DirectoryConnectSettings [Core.Text]
directoryConnectSettings_subnetIds = Lens.lens (\DirectoryConnectSettings' {subnetIds} -> subnetIds) (\s@DirectoryConnectSettings' {} a -> s {subnetIds = a} :: DirectoryConnectSettings) Core.. Lens._Coerce

-- | A list of one or more IP addresses of DNS servers or domain controllers
-- in the on-premises directory.
directoryConnectSettings_customerDnsIps :: Lens.Lens' DirectoryConnectSettings [Core.Text]
directoryConnectSettings_customerDnsIps = Lens.lens (\DirectoryConnectSettings' {customerDnsIps} -> customerDnsIps) (\s@DirectoryConnectSettings' {} a -> s {customerDnsIps = a} :: DirectoryConnectSettings) Core.. Lens._Coerce

-- | The user name of an account in the on-premises directory that is used to
-- connect to the directory. This account must have the following
-- permissions:
--
-- -   Read users and groups
--
-- -   Create computer objects
--
-- -   Join computers to the domain
directoryConnectSettings_customerUserName :: Lens.Lens' DirectoryConnectSettings Core.Text
directoryConnectSettings_customerUserName = Lens.lens (\DirectoryConnectSettings' {customerUserName} -> customerUserName) (\s@DirectoryConnectSettings' {} a -> s {customerUserName = a} :: DirectoryConnectSettings)

instance Core.Hashable DirectoryConnectSettings

instance Core.NFData DirectoryConnectSettings

instance Core.ToJSON DirectoryConnectSettings where
  toJSON DirectoryConnectSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VpcId" Core..= vpcId),
            Core.Just ("SubnetIds" Core..= subnetIds),
            Core.Just ("CustomerDnsIps" Core..= customerDnsIps),
            Core.Just
              ("CustomerUserName" Core..= customerUserName)
          ]
      )

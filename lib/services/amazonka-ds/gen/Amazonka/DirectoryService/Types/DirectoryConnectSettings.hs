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
-- Module      : Amazonka.DirectoryService.Types.DirectoryConnectSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryConnectSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information for the ConnectDirectory operation when an AD
-- Connector directory is being created.
--
-- /See:/ 'newDirectoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { -- | The identifier of the VPC in which the AD Connector is created.
    vpcId :: Prelude.Text,
    -- | A list of subnet identifiers in the VPC in which the AD Connector is
    -- created.
    subnetIds :: [Prelude.Text],
    -- | A list of one or more IP addresses of DNS servers or domain controllers
    -- in your self-managed directory.
    customerDnsIps :: [Prelude.Text],
    -- | The user name of an account in your self-managed directory that is used
    -- to connect to the directory. This account must have the following
    -- permissions:
    --
    -- -   Read users and groups
    --
    -- -   Create computer objects
    --
    -- -   Join computers to the domain
    customerUserName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- in your self-managed directory.
--
-- 'customerUserName', 'directoryConnectSettings_customerUserName' - The user name of an account in your self-managed directory that is used
-- to connect to the directory. This account must have the following
-- permissions:
--
-- -   Read users and groups
--
-- -   Create computer objects
--
-- -   Join computers to the domain
newDirectoryConnectSettings ::
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'customerUserName'
  Prelude.Text ->
  DirectoryConnectSettings
newDirectoryConnectSettings
  pVpcId_
  pCustomerUserName_ =
    DirectoryConnectSettings'
      { vpcId = pVpcId_,
        subnetIds = Prelude.mempty,
        customerDnsIps = Prelude.mempty,
        customerUserName = pCustomerUserName_
      }

-- | The identifier of the VPC in which the AD Connector is created.
directoryConnectSettings_vpcId :: Lens.Lens' DirectoryConnectSettings Prelude.Text
directoryConnectSettings_vpcId = Lens.lens (\DirectoryConnectSettings' {vpcId} -> vpcId) (\s@DirectoryConnectSettings' {} a -> s {vpcId = a} :: DirectoryConnectSettings)

-- | A list of subnet identifiers in the VPC in which the AD Connector is
-- created.
directoryConnectSettings_subnetIds :: Lens.Lens' DirectoryConnectSettings [Prelude.Text]
directoryConnectSettings_subnetIds = Lens.lens (\DirectoryConnectSettings' {subnetIds} -> subnetIds) (\s@DirectoryConnectSettings' {} a -> s {subnetIds = a} :: DirectoryConnectSettings) Prelude.. Lens.coerced

-- | A list of one or more IP addresses of DNS servers or domain controllers
-- in your self-managed directory.
directoryConnectSettings_customerDnsIps :: Lens.Lens' DirectoryConnectSettings [Prelude.Text]
directoryConnectSettings_customerDnsIps = Lens.lens (\DirectoryConnectSettings' {customerDnsIps} -> customerDnsIps) (\s@DirectoryConnectSettings' {} a -> s {customerDnsIps = a} :: DirectoryConnectSettings) Prelude.. Lens.coerced

-- | The user name of an account in your self-managed directory that is used
-- to connect to the directory. This account must have the following
-- permissions:
--
-- -   Read users and groups
--
-- -   Create computer objects
--
-- -   Join computers to the domain
directoryConnectSettings_customerUserName :: Lens.Lens' DirectoryConnectSettings Prelude.Text
directoryConnectSettings_customerUserName = Lens.lens (\DirectoryConnectSettings' {customerUserName} -> customerUserName) (\s@DirectoryConnectSettings' {} a -> s {customerUserName = a} :: DirectoryConnectSettings)

instance Prelude.Hashable DirectoryConnectSettings where
  hashWithSalt _salt DirectoryConnectSettings' {..} =
    _salt
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` customerDnsIps
      `Prelude.hashWithSalt` customerUserName

instance Prelude.NFData DirectoryConnectSettings where
  rnf DirectoryConnectSettings' {..} =
    Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf customerDnsIps
      `Prelude.seq` Prelude.rnf customerUserName

instance Data.ToJSON DirectoryConnectSettings where
  toJSON DirectoryConnectSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("CustomerDnsIps" Data..= customerDnsIps),
            Prelude.Just
              ("CustomerUserName" Data..= customerUserName)
          ]
      )

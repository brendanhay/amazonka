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
-- Module      : Amazonka.MigrationHubStrategy.Types.NetworkInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.NetworkInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the server\'s network for which the assessment was
-- run.
--
-- /See:/ 'newNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { -- | Information about the name of the interface of the server for which the
    -- assessment was run.
    interfaceName :: Prelude.Text,
    -- | Information about the IP address of the server for which the assessment
    -- was run.
    ipAddress :: Prelude.Text,
    -- | Information about the MAC address of the server for which the assessment
    -- was run.
    macAddress :: Prelude.Text,
    -- | Information about the subnet mask of the server for which the assessment
    -- was run.
    netMask :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interfaceName', 'networkInfo_interfaceName' - Information about the name of the interface of the server for which the
-- assessment was run.
--
-- 'ipAddress', 'networkInfo_ipAddress' - Information about the IP address of the server for which the assessment
-- was run.
--
-- 'macAddress', 'networkInfo_macAddress' - Information about the MAC address of the server for which the assessment
-- was run.
--
-- 'netMask', 'networkInfo_netMask' - Information about the subnet mask of the server for which the assessment
-- was run.
newNetworkInfo ::
  -- | 'interfaceName'
  Prelude.Text ->
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'macAddress'
  Prelude.Text ->
  -- | 'netMask'
  Prelude.Text ->
  NetworkInfo
newNetworkInfo
  pInterfaceName_
  pIpAddress_
  pMacAddress_
  pNetMask_ =
    NetworkInfo'
      { interfaceName = pInterfaceName_,
        ipAddress = pIpAddress_,
        macAddress = pMacAddress_,
        netMask = pNetMask_
      }

-- | Information about the name of the interface of the server for which the
-- assessment was run.
networkInfo_interfaceName :: Lens.Lens' NetworkInfo Prelude.Text
networkInfo_interfaceName = Lens.lens (\NetworkInfo' {interfaceName} -> interfaceName) (\s@NetworkInfo' {} a -> s {interfaceName = a} :: NetworkInfo)

-- | Information about the IP address of the server for which the assessment
-- was run.
networkInfo_ipAddress :: Lens.Lens' NetworkInfo Prelude.Text
networkInfo_ipAddress = Lens.lens (\NetworkInfo' {ipAddress} -> ipAddress) (\s@NetworkInfo' {} a -> s {ipAddress = a} :: NetworkInfo)

-- | Information about the MAC address of the server for which the assessment
-- was run.
networkInfo_macAddress :: Lens.Lens' NetworkInfo Prelude.Text
networkInfo_macAddress = Lens.lens (\NetworkInfo' {macAddress} -> macAddress) (\s@NetworkInfo' {} a -> s {macAddress = a} :: NetworkInfo)

-- | Information about the subnet mask of the server for which the assessment
-- was run.
networkInfo_netMask :: Lens.Lens' NetworkInfo Prelude.Text
networkInfo_netMask = Lens.lens (\NetworkInfo' {netMask} -> netMask) (\s@NetworkInfo' {} a -> s {netMask = a} :: NetworkInfo)

instance Data.FromJSON NetworkInfo where
  parseJSON =
    Data.withObject
      "NetworkInfo"
      ( \x ->
          NetworkInfo'
            Prelude.<$> (x Data..: "interfaceName")
            Prelude.<*> (x Data..: "ipAddress")
            Prelude.<*> (x Data..: "macAddress")
            Prelude.<*> (x Data..: "netMask")
      )

instance Prelude.Hashable NetworkInfo where
  hashWithSalt _salt NetworkInfo' {..} =
    _salt `Prelude.hashWithSalt` interfaceName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` netMask

instance Prelude.NFData NetworkInfo where
  rnf NetworkInfo' {..} =
    Prelude.rnf interfaceName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf netMask

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
-- Module      : Amazonka.EC2.Types.IpamDiscoveredAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamDiscoveredAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamDiscoveryFailureReason
import qualified Amazonka.Prelude as Prelude

-- | An IPAM discovered account. A discovered account is an Amazon Web
-- Services account that is monitored under a resource discovery. If you
-- have integrated IPAM with Amazon Web Services Organizations, all
-- accounts in the organization are discovered accounts.
--
-- /See:/ 'newIpamDiscoveredAccount' smart constructor.
data IpamDiscoveredAccount = IpamDiscoveredAccount'
  { -- | The account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the account information is returned
    -- from. An account can be discovered in multiple regions and will have a
    -- separate discovered account for each Region.
    discoveryRegion :: Prelude.Maybe Prelude.Text,
    -- | The resource discovery failure reason.
    failureReason :: Prelude.Maybe IpamDiscoveryFailureReason,
    -- | The last attempted resource discovery time.
    lastAttemptedDiscoveryTime :: Prelude.Maybe Data.ISO8601,
    -- | The last successful resource discovery time.
    lastSuccessfulDiscoveryTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamDiscoveredAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'ipamDiscoveredAccount_accountId' - The account ID.
--
-- 'discoveryRegion', 'ipamDiscoveredAccount_discoveryRegion' - The Amazon Web Services Region that the account information is returned
-- from. An account can be discovered in multiple regions and will have a
-- separate discovered account for each Region.
--
-- 'failureReason', 'ipamDiscoveredAccount_failureReason' - The resource discovery failure reason.
--
-- 'lastAttemptedDiscoveryTime', 'ipamDiscoveredAccount_lastAttemptedDiscoveryTime' - The last attempted resource discovery time.
--
-- 'lastSuccessfulDiscoveryTime', 'ipamDiscoveredAccount_lastSuccessfulDiscoveryTime' - The last successful resource discovery time.
newIpamDiscoveredAccount ::
  IpamDiscoveredAccount
newIpamDiscoveredAccount =
  IpamDiscoveredAccount'
    { accountId = Prelude.Nothing,
      discoveryRegion = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastAttemptedDiscoveryTime = Prelude.Nothing,
      lastSuccessfulDiscoveryTime = Prelude.Nothing
    }

-- | The account ID.
ipamDiscoveredAccount_accountId :: Lens.Lens' IpamDiscoveredAccount (Prelude.Maybe Prelude.Text)
ipamDiscoveredAccount_accountId = Lens.lens (\IpamDiscoveredAccount' {accountId} -> accountId) (\s@IpamDiscoveredAccount' {} a -> s {accountId = a} :: IpamDiscoveredAccount)

-- | The Amazon Web Services Region that the account information is returned
-- from. An account can be discovered in multiple regions and will have a
-- separate discovered account for each Region.
ipamDiscoveredAccount_discoveryRegion :: Lens.Lens' IpamDiscoveredAccount (Prelude.Maybe Prelude.Text)
ipamDiscoveredAccount_discoveryRegion = Lens.lens (\IpamDiscoveredAccount' {discoveryRegion} -> discoveryRegion) (\s@IpamDiscoveredAccount' {} a -> s {discoveryRegion = a} :: IpamDiscoveredAccount)

-- | The resource discovery failure reason.
ipamDiscoveredAccount_failureReason :: Lens.Lens' IpamDiscoveredAccount (Prelude.Maybe IpamDiscoveryFailureReason)
ipamDiscoveredAccount_failureReason = Lens.lens (\IpamDiscoveredAccount' {failureReason} -> failureReason) (\s@IpamDiscoveredAccount' {} a -> s {failureReason = a} :: IpamDiscoveredAccount)

-- | The last attempted resource discovery time.
ipamDiscoveredAccount_lastAttemptedDiscoveryTime :: Lens.Lens' IpamDiscoveredAccount (Prelude.Maybe Prelude.UTCTime)
ipamDiscoveredAccount_lastAttemptedDiscoveryTime = Lens.lens (\IpamDiscoveredAccount' {lastAttemptedDiscoveryTime} -> lastAttemptedDiscoveryTime) (\s@IpamDiscoveredAccount' {} a -> s {lastAttemptedDiscoveryTime = a} :: IpamDiscoveredAccount) Prelude.. Lens.mapping Data._Time

-- | The last successful resource discovery time.
ipamDiscoveredAccount_lastSuccessfulDiscoveryTime :: Lens.Lens' IpamDiscoveredAccount (Prelude.Maybe Prelude.UTCTime)
ipamDiscoveredAccount_lastSuccessfulDiscoveryTime = Lens.lens (\IpamDiscoveredAccount' {lastSuccessfulDiscoveryTime} -> lastSuccessfulDiscoveryTime) (\s@IpamDiscoveredAccount' {} a -> s {lastSuccessfulDiscoveryTime = a} :: IpamDiscoveredAccount) Prelude.. Lens.mapping Data._Time

instance Data.FromXML IpamDiscoveredAccount where
  parseXML x =
    IpamDiscoveredAccount'
      Prelude.<$> (x Data..@? "accountId")
      Prelude.<*> (x Data..@? "discoveryRegion")
      Prelude.<*> (x Data..@? "failureReason")
      Prelude.<*> (x Data..@? "lastAttemptedDiscoveryTime")
      Prelude.<*> (x Data..@? "lastSuccessfulDiscoveryTime")

instance Prelude.Hashable IpamDiscoveredAccount where
  hashWithSalt _salt IpamDiscoveredAccount' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` discoveryRegion
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastAttemptedDiscoveryTime
      `Prelude.hashWithSalt` lastSuccessfulDiscoveryTime

instance Prelude.NFData IpamDiscoveredAccount where
  rnf IpamDiscoveredAccount' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf discoveryRegion
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastAttemptedDiscoveryTime
      `Prelude.seq` Prelude.rnf lastSuccessfulDiscoveryTime

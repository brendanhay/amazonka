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
-- Module      : Amazonka.Lightsail.Types.RegisteredDomainDelegationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RegisteredDomainDelegationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.NameServersUpdateState
import Amazonka.Lightsail.Types.R53HostedZoneDeletionState
import qualified Amazonka.Prelude as Prelude

-- | Describes the delegation state of an Amazon Route 53 registered domain
-- to Amazon Lightsail.
--
-- When you delegate an Amazon Route 53 registered domain to Lightsail, you
-- can manage the DNS of the domain using a Lightsail DNS zone. You no
-- longer use the Route 53 hosted zone to manage the DNS of the domain. To
-- delegate the domain, Lightsail automatically updates the domain\'s name
-- servers in Route 53 to the name servers of the Lightsail DNS zone. Then,
-- Lightsail automatically deletes the Route 53 hosted zone for the domain.
--
-- All of the following conditions must be true for automatic domain
-- delegation to be successful:
--
-- -   The registered domain must be in the same Amazon Web Services
--     account as the Lightsail account making the request.
--
-- -   The user or entity making the request must have permission to manage
--     domains in Route 53.
--
-- -   The Route 53 hosted zone for the domain must be empty. It cannot
--     contain DNS records other than start of authority (SOA) and name
--     server records.
--
-- If automatic domain delegation fails, or if you manage the DNS of your
-- domain using a service other than Route 53, then you must manually add
-- the Lightsail DNS zone name servers to your domain in order to delegate
-- management of its DNS to Lightsail. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/lightsail-how-to-create-dns-entry Creating a DNS zone to manage your domain’s records in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newRegisteredDomainDelegationInfo' smart constructor.
data RegisteredDomainDelegationInfo = RegisteredDomainDelegationInfo'
  { -- | An object that describes the state of the name server records that are
    -- automatically added to the Route 53 domain by Lightsail.
    nameServersUpdateState :: Prelude.Maybe NameServersUpdateState,
    r53HostedZoneDeletionState :: Prelude.Maybe R53HostedZoneDeletionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredDomainDelegationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameServersUpdateState', 'registeredDomainDelegationInfo_nameServersUpdateState' - An object that describes the state of the name server records that are
-- automatically added to the Route 53 domain by Lightsail.
--
-- 'r53HostedZoneDeletionState', 'registeredDomainDelegationInfo_r53HostedZoneDeletionState' - Undocumented member.
newRegisteredDomainDelegationInfo ::
  RegisteredDomainDelegationInfo
newRegisteredDomainDelegationInfo =
  RegisteredDomainDelegationInfo'
    { nameServersUpdateState =
        Prelude.Nothing,
      r53HostedZoneDeletionState =
        Prelude.Nothing
    }

-- | An object that describes the state of the name server records that are
-- automatically added to the Route 53 domain by Lightsail.
registeredDomainDelegationInfo_nameServersUpdateState :: Lens.Lens' RegisteredDomainDelegationInfo (Prelude.Maybe NameServersUpdateState)
registeredDomainDelegationInfo_nameServersUpdateState = Lens.lens (\RegisteredDomainDelegationInfo' {nameServersUpdateState} -> nameServersUpdateState) (\s@RegisteredDomainDelegationInfo' {} a -> s {nameServersUpdateState = a} :: RegisteredDomainDelegationInfo)

-- | Undocumented member.
registeredDomainDelegationInfo_r53HostedZoneDeletionState :: Lens.Lens' RegisteredDomainDelegationInfo (Prelude.Maybe R53HostedZoneDeletionState)
registeredDomainDelegationInfo_r53HostedZoneDeletionState = Lens.lens (\RegisteredDomainDelegationInfo' {r53HostedZoneDeletionState} -> r53HostedZoneDeletionState) (\s@RegisteredDomainDelegationInfo' {} a -> s {r53HostedZoneDeletionState = a} :: RegisteredDomainDelegationInfo)

instance Data.FromJSON RegisteredDomainDelegationInfo where
  parseJSON =
    Data.withObject
      "RegisteredDomainDelegationInfo"
      ( \x ->
          RegisteredDomainDelegationInfo'
            Prelude.<$> (x Data..:? "nameServersUpdateState")
            Prelude.<*> (x Data..:? "r53HostedZoneDeletionState")
      )

instance
  Prelude.Hashable
    RegisteredDomainDelegationInfo
  where
  hashWithSalt
    _salt
    RegisteredDomainDelegationInfo' {..} =
      _salt
        `Prelude.hashWithSalt` nameServersUpdateState
        `Prelude.hashWithSalt` r53HostedZoneDeletionState

instance
  Prelude.NFData
    RegisteredDomainDelegationInfo
  where
  rnf RegisteredDomainDelegationInfo' {..} =
    Prelude.rnf nameServersUpdateState
      `Prelude.seq` Prelude.rnf r53HostedZoneDeletionState

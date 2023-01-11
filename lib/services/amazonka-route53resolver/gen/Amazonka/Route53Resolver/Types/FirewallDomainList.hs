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
-- Module      : Amazonka.Route53Resolver.Types.FirewallDomainList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallDomainList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.FirewallDomainListStatus

-- | High-level information about a list of firewall domains for use in a
-- FirewallRule. This is returned by GetFirewallDomainList.
--
-- To retrieve the domains that are defined for this domain list, call
-- ListFirewallDomains.
--
-- /See:/ 'newFirewallDomainList' smart constructor.
data FirewallDomainList = FirewallDomainList'
  { -- | The Amazon Resource Name (ARN) of the firewall domain list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the domain list was created, in Unix time format
    -- and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A unique string defined by you to identify the request. This allows you
    -- to retry failed requests without the risk of running the operation
    -- twice. This can be any unique string, for example, a timestamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The number of domain names that are specified in the domain list.
    domainCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the domain list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The owner of the list, used only for lists that are not managed by you.
    -- For example, the managed domain list
    -- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
    -- @Route 53 Resolver DNS Firewall@.
    managedOwnerName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the domain list was last modified, in Unix time
    -- format and Coordinated Universal Time (UTC).
    modificationTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the domain list.
    status :: Prelude.Maybe FirewallDomainListStatus,
    -- | Additional information about the status of the list, if available.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallDomainList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'firewallDomainList_arn' - The Amazon Resource Name (ARN) of the firewall domain list.
--
-- 'creationTime', 'firewallDomainList_creationTime' - The date and time that the domain list was created, in Unix time format
-- and Coordinated Universal Time (UTC).
--
-- 'creatorRequestId', 'firewallDomainList_creatorRequestId' - A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
--
-- 'domainCount', 'firewallDomainList_domainCount' - The number of domain names that are specified in the domain list.
--
-- 'id', 'firewallDomainList_id' - The ID of the domain list.
--
-- 'managedOwnerName', 'firewallDomainList_managedOwnerName' - The owner of the list, used only for lists that are not managed by you.
-- For example, the managed domain list
-- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
-- @Route 53 Resolver DNS Firewall@.
--
-- 'modificationTime', 'firewallDomainList_modificationTime' - The date and time that the domain list was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'name', 'firewallDomainList_name' - The name of the domain list.
--
-- 'status', 'firewallDomainList_status' - The status of the domain list.
--
-- 'statusMessage', 'firewallDomainList_statusMessage' - Additional information about the status of the list, if available.
newFirewallDomainList ::
  FirewallDomainList
newFirewallDomainList =
  FirewallDomainList'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      domainCount = Prelude.Nothing,
      id = Prelude.Nothing,
      managedOwnerName = Prelude.Nothing,
      modificationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall domain list.
firewallDomainList_arn :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_arn = Lens.lens (\FirewallDomainList' {arn} -> arn) (\s@FirewallDomainList' {} a -> s {arn = a} :: FirewallDomainList)

-- | The date and time that the domain list was created, in Unix time format
-- and Coordinated Universal Time (UTC).
firewallDomainList_creationTime :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_creationTime = Lens.lens (\FirewallDomainList' {creationTime} -> creationTime) (\s@FirewallDomainList' {} a -> s {creationTime = a} :: FirewallDomainList)

-- | A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
firewallDomainList_creatorRequestId :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_creatorRequestId = Lens.lens (\FirewallDomainList' {creatorRequestId} -> creatorRequestId) (\s@FirewallDomainList' {} a -> s {creatorRequestId = a} :: FirewallDomainList)

-- | The number of domain names that are specified in the domain list.
firewallDomainList_domainCount :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Int)
firewallDomainList_domainCount = Lens.lens (\FirewallDomainList' {domainCount} -> domainCount) (\s@FirewallDomainList' {} a -> s {domainCount = a} :: FirewallDomainList)

-- | The ID of the domain list.
firewallDomainList_id :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_id = Lens.lens (\FirewallDomainList' {id} -> id) (\s@FirewallDomainList' {} a -> s {id = a} :: FirewallDomainList)

-- | The owner of the list, used only for lists that are not managed by you.
-- For example, the managed domain list
-- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
-- @Route 53 Resolver DNS Firewall@.
firewallDomainList_managedOwnerName :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_managedOwnerName = Lens.lens (\FirewallDomainList' {managedOwnerName} -> managedOwnerName) (\s@FirewallDomainList' {} a -> s {managedOwnerName = a} :: FirewallDomainList)

-- | The date and time that the domain list was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
firewallDomainList_modificationTime :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_modificationTime = Lens.lens (\FirewallDomainList' {modificationTime} -> modificationTime) (\s@FirewallDomainList' {} a -> s {modificationTime = a} :: FirewallDomainList)

-- | The name of the domain list.
firewallDomainList_name :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_name = Lens.lens (\FirewallDomainList' {name} -> name) (\s@FirewallDomainList' {} a -> s {name = a} :: FirewallDomainList)

-- | The status of the domain list.
firewallDomainList_status :: Lens.Lens' FirewallDomainList (Prelude.Maybe FirewallDomainListStatus)
firewallDomainList_status = Lens.lens (\FirewallDomainList' {status} -> status) (\s@FirewallDomainList' {} a -> s {status = a} :: FirewallDomainList)

-- | Additional information about the status of the list, if available.
firewallDomainList_statusMessage :: Lens.Lens' FirewallDomainList (Prelude.Maybe Prelude.Text)
firewallDomainList_statusMessage = Lens.lens (\FirewallDomainList' {statusMessage} -> statusMessage) (\s@FirewallDomainList' {} a -> s {statusMessage = a} :: FirewallDomainList)

instance Data.FromJSON FirewallDomainList where
  parseJSON =
    Data.withObject
      "FirewallDomainList"
      ( \x ->
          FirewallDomainList'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "DomainCount")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "ManagedOwnerName")
            Prelude.<*> (x Data..:? "ModificationTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable FirewallDomainList where
  hashWithSalt _salt FirewallDomainList' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` domainCount
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` managedOwnerName
      `Prelude.hashWithSalt` modificationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData FirewallDomainList where
  rnf FirewallDomainList' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf domainCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf managedOwnerName
      `Prelude.seq` Prelude.rnf modificationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage

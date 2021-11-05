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
-- Module      : Amazonka.Route53Resolver.Types.FirewallDomainListMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallDomainListMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Minimal high-level information for a firewall domain list. The action
-- ListFirewallDomainLists returns an array of these objects.
--
-- To retrieve full information for a firewall domain list, call
-- GetFirewallDomainList and ListFirewallDomains.
--
-- /See:/ 'newFirewallDomainListMetadata' smart constructor.
data FirewallDomainListMetadata = FirewallDomainListMetadata'
  { -- | The Amazon Resource Name (ARN) of the firewall domain list metadata.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique string defined by you to identify the request. This allows you
    -- to retry failed requests without the risk of running the operation
    -- twice. This can be any unique string, for example, a timestamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the list, used only for lists that are not managed by you.
    -- For example, the managed domain list
    -- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
    -- @Route 53 Resolver DNS Firewall@.
    managedOwnerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the domain list.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallDomainListMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'firewallDomainListMetadata_arn' - The Amazon Resource Name (ARN) of the firewall domain list metadata.
--
-- 'creatorRequestId', 'firewallDomainListMetadata_creatorRequestId' - A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
--
-- 'managedOwnerName', 'firewallDomainListMetadata_managedOwnerName' - The owner of the list, used only for lists that are not managed by you.
-- For example, the managed domain list
-- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
-- @Route 53 Resolver DNS Firewall@.
--
-- 'name', 'firewallDomainListMetadata_name' - The name of the domain list.
--
-- 'id', 'firewallDomainListMetadata_id' - The ID of the domain list.
newFirewallDomainListMetadata ::
  FirewallDomainListMetadata
newFirewallDomainListMetadata =
  FirewallDomainListMetadata'
    { arn = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      managedOwnerName = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall domain list metadata.
firewallDomainListMetadata_arn :: Lens.Lens' FirewallDomainListMetadata (Prelude.Maybe Prelude.Text)
firewallDomainListMetadata_arn = Lens.lens (\FirewallDomainListMetadata' {arn} -> arn) (\s@FirewallDomainListMetadata' {} a -> s {arn = a} :: FirewallDomainListMetadata)

-- | A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
firewallDomainListMetadata_creatorRequestId :: Lens.Lens' FirewallDomainListMetadata (Prelude.Maybe Prelude.Text)
firewallDomainListMetadata_creatorRequestId = Lens.lens (\FirewallDomainListMetadata' {creatorRequestId} -> creatorRequestId) (\s@FirewallDomainListMetadata' {} a -> s {creatorRequestId = a} :: FirewallDomainListMetadata)

-- | The owner of the list, used only for lists that are not managed by you.
-- For example, the managed domain list
-- @AWSManagedDomainsMalwareDomainList@ has the managed owner name
-- @Route 53 Resolver DNS Firewall@.
firewallDomainListMetadata_managedOwnerName :: Lens.Lens' FirewallDomainListMetadata (Prelude.Maybe Prelude.Text)
firewallDomainListMetadata_managedOwnerName = Lens.lens (\FirewallDomainListMetadata' {managedOwnerName} -> managedOwnerName) (\s@FirewallDomainListMetadata' {} a -> s {managedOwnerName = a} :: FirewallDomainListMetadata)

-- | The name of the domain list.
firewallDomainListMetadata_name :: Lens.Lens' FirewallDomainListMetadata (Prelude.Maybe Prelude.Text)
firewallDomainListMetadata_name = Lens.lens (\FirewallDomainListMetadata' {name} -> name) (\s@FirewallDomainListMetadata' {} a -> s {name = a} :: FirewallDomainListMetadata)

-- | The ID of the domain list.
firewallDomainListMetadata_id :: Lens.Lens' FirewallDomainListMetadata (Prelude.Maybe Prelude.Text)
firewallDomainListMetadata_id = Lens.lens (\FirewallDomainListMetadata' {id} -> id) (\s@FirewallDomainListMetadata' {} a -> s {id = a} :: FirewallDomainListMetadata)

instance Core.FromJSON FirewallDomainListMetadata where
  parseJSON =
    Core.withObject
      "FirewallDomainListMetadata"
      ( \x ->
          FirewallDomainListMetadata'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "ManagedOwnerName")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable FirewallDomainListMetadata

instance Prelude.NFData FirewallDomainListMetadata

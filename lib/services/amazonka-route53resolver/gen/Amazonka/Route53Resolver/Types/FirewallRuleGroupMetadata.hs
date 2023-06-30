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
-- Module      : Amazonka.Route53Resolver.Types.FirewallRuleGroupMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallRuleGroupMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ShareStatus

-- | Minimal high-level information for a firewall rule group. The action
-- ListFirewallRuleGroups returns an array of these objects.
--
-- To retrieve full information for a firewall rule group, call
-- GetFirewallRuleGroup and ListFirewallRules.
--
-- /See:/ 'newFirewallRuleGroupMetadata' smart constructor.
data FirewallRuleGroupMetadata = FirewallRuleGroupMetadata'
  { -- | The ARN (Amazon Resource Name) of the rule group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique string defined by you to identify the request. This allows you
    -- to retry failed requests without the risk of running the operation
    -- twice. This can be any unique string, for example, a timestamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the rule group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the account that created the rule
    -- group. When a rule group is shared with your account, this is the
    -- account that has shared the rule group with you.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Whether the rule group is shared with other Amazon Web Services
    -- accounts, or was shared with the current account by another Amazon Web
    -- Services account. Sharing is configured through Resource Access Manager
    -- (RAM).
    shareStatus :: Prelude.Maybe ShareStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallRuleGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'firewallRuleGroupMetadata_arn' - The ARN (Amazon Resource Name) of the rule group.
--
-- 'creatorRequestId', 'firewallRuleGroupMetadata_creatorRequestId' - A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
--
-- 'id', 'firewallRuleGroupMetadata_id' - The ID of the rule group.
--
-- 'name', 'firewallRuleGroupMetadata_name' - The name of the rule group.
--
-- 'ownerId', 'firewallRuleGroupMetadata_ownerId' - The Amazon Web Services account ID for the account that created the rule
-- group. When a rule group is shared with your account, this is the
-- account that has shared the rule group with you.
--
-- 'shareStatus', 'firewallRuleGroupMetadata_shareStatus' - Whether the rule group is shared with other Amazon Web Services
-- accounts, or was shared with the current account by another Amazon Web
-- Services account. Sharing is configured through Resource Access Manager
-- (RAM).
newFirewallRuleGroupMetadata ::
  FirewallRuleGroupMetadata
newFirewallRuleGroupMetadata =
  FirewallRuleGroupMetadata'
    { arn = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      shareStatus = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the rule group.
firewallRuleGroupMetadata_arn :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe Prelude.Text)
firewallRuleGroupMetadata_arn = Lens.lens (\FirewallRuleGroupMetadata' {arn} -> arn) (\s@FirewallRuleGroupMetadata' {} a -> s {arn = a} :: FirewallRuleGroupMetadata)

-- | A unique string defined by you to identify the request. This allows you
-- to retry failed requests without the risk of running the operation
-- twice. This can be any unique string, for example, a timestamp.
firewallRuleGroupMetadata_creatorRequestId :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe Prelude.Text)
firewallRuleGroupMetadata_creatorRequestId = Lens.lens (\FirewallRuleGroupMetadata' {creatorRequestId} -> creatorRequestId) (\s@FirewallRuleGroupMetadata' {} a -> s {creatorRequestId = a} :: FirewallRuleGroupMetadata)

-- | The ID of the rule group.
firewallRuleGroupMetadata_id :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe Prelude.Text)
firewallRuleGroupMetadata_id = Lens.lens (\FirewallRuleGroupMetadata' {id} -> id) (\s@FirewallRuleGroupMetadata' {} a -> s {id = a} :: FirewallRuleGroupMetadata)

-- | The name of the rule group.
firewallRuleGroupMetadata_name :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe Prelude.Text)
firewallRuleGroupMetadata_name = Lens.lens (\FirewallRuleGroupMetadata' {name} -> name) (\s@FirewallRuleGroupMetadata' {} a -> s {name = a} :: FirewallRuleGroupMetadata)

-- | The Amazon Web Services account ID for the account that created the rule
-- group. When a rule group is shared with your account, this is the
-- account that has shared the rule group with you.
firewallRuleGroupMetadata_ownerId :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe Prelude.Text)
firewallRuleGroupMetadata_ownerId = Lens.lens (\FirewallRuleGroupMetadata' {ownerId} -> ownerId) (\s@FirewallRuleGroupMetadata' {} a -> s {ownerId = a} :: FirewallRuleGroupMetadata)

-- | Whether the rule group is shared with other Amazon Web Services
-- accounts, or was shared with the current account by another Amazon Web
-- Services account. Sharing is configured through Resource Access Manager
-- (RAM).
firewallRuleGroupMetadata_shareStatus :: Lens.Lens' FirewallRuleGroupMetadata (Prelude.Maybe ShareStatus)
firewallRuleGroupMetadata_shareStatus = Lens.lens (\FirewallRuleGroupMetadata' {shareStatus} -> shareStatus) (\s@FirewallRuleGroupMetadata' {} a -> s {shareStatus = a} :: FirewallRuleGroupMetadata)

instance Data.FromJSON FirewallRuleGroupMetadata where
  parseJSON =
    Data.withObject
      "FirewallRuleGroupMetadata"
      ( \x ->
          FirewallRuleGroupMetadata'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatorRequestId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ShareStatus")
      )

instance Prelude.Hashable FirewallRuleGroupMetadata where
  hashWithSalt _salt FirewallRuleGroupMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` shareStatus

instance Prelude.NFData FirewallRuleGroupMetadata where
  rnf FirewallRuleGroupMetadata' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf shareStatus

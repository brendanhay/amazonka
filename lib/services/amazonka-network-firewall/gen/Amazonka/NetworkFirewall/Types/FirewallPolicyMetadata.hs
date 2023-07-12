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
-- Module      : Amazonka.NetworkFirewall.Types.FirewallPolicyMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.FirewallPolicyMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a firewall policy, returned by operations
-- like create and describe. You can use the information provided in the
-- metadata to retrieve and manage a firewall policy. You can retrieve all
-- objects for a firewall policy by calling DescribeFirewallPolicy.
--
-- /See:/ 'newFirewallPolicyMetadata' smart constructor.
data FirewallPolicyMetadata = FirewallPolicyMetadata'
  { -- | The Amazon Resource Name (ARN) of the firewall policy.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall policy. You can\'t change the name
    -- of a firewall policy after you create it.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'firewallPolicyMetadata_arn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- 'name', 'firewallPolicyMetadata_name' - The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
newFirewallPolicyMetadata ::
  FirewallPolicyMetadata
newFirewallPolicyMetadata =
  FirewallPolicyMetadata'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall policy.
firewallPolicyMetadata_arn :: Lens.Lens' FirewallPolicyMetadata (Prelude.Maybe Prelude.Text)
firewallPolicyMetadata_arn = Lens.lens (\FirewallPolicyMetadata' {arn} -> arn) (\s@FirewallPolicyMetadata' {} a -> s {arn = a} :: FirewallPolicyMetadata)

-- | The descriptive name of the firewall policy. You can\'t change the name
-- of a firewall policy after you create it.
firewallPolicyMetadata_name :: Lens.Lens' FirewallPolicyMetadata (Prelude.Maybe Prelude.Text)
firewallPolicyMetadata_name = Lens.lens (\FirewallPolicyMetadata' {name} -> name) (\s@FirewallPolicyMetadata' {} a -> s {name = a} :: FirewallPolicyMetadata)

instance Data.FromJSON FirewallPolicyMetadata where
  parseJSON =
    Data.withObject
      "FirewallPolicyMetadata"
      ( \x ->
          FirewallPolicyMetadata'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable FirewallPolicyMetadata where
  hashWithSalt _salt FirewallPolicyMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name

instance Prelude.NFData FirewallPolicyMetadata where
  rnf FirewallPolicyMetadata' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name

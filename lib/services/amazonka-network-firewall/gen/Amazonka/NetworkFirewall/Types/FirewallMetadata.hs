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
-- Module      : Amazonka.NetworkFirewall.Types.FirewallMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.FirewallMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | High-level information about a firewall, returned by operations like
-- create and describe. You can use the information provided in the
-- metadata to retrieve and manage a firewall.
--
-- /See:/ 'newFirewallMetadata' smart constructor.
data FirewallMetadata = FirewallMetadata'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'firewallMetadata_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallName', 'firewallMetadata_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
newFirewallMetadata ::
  FirewallMetadata
newFirewallMetadata =
  FirewallMetadata'
    { firewallArn = Prelude.Nothing,
      firewallName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the firewall.
firewallMetadata_firewallArn :: Lens.Lens' FirewallMetadata (Prelude.Maybe Prelude.Text)
firewallMetadata_firewallArn = Lens.lens (\FirewallMetadata' {firewallArn} -> firewallArn) (\s@FirewallMetadata' {} a -> s {firewallArn = a} :: FirewallMetadata)

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
firewallMetadata_firewallName :: Lens.Lens' FirewallMetadata (Prelude.Maybe Prelude.Text)
firewallMetadata_firewallName = Lens.lens (\FirewallMetadata' {firewallName} -> firewallName) (\s@FirewallMetadata' {} a -> s {firewallName = a} :: FirewallMetadata)

instance Core.FromJSON FirewallMetadata where
  parseJSON =
    Core.withObject
      "FirewallMetadata"
      ( \x ->
          FirewallMetadata'
            Prelude.<$> (x Core..:? "FirewallArn")
            Prelude.<*> (x Core..:? "FirewallName")
      )

instance Prelude.Hashable FirewallMetadata where
  hashWithSalt _salt FirewallMetadata' {..} =
    _salt `Prelude.hashWithSalt` firewallArn
      `Prelude.hashWithSalt` firewallName

instance Prelude.NFData FirewallMetadata where
  rnf FirewallMetadata' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallName

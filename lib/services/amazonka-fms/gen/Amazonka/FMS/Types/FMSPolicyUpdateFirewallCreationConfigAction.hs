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
-- Module      : Amazonka.FMS.Types.FMSPolicyUpdateFirewallCreationConfigAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.FMSPolicyUpdateFirewallCreationConfigAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the actions that you can take to remediate
-- scope violations caused by your policy\'s @FirewallCreationConfig@.
-- @FirewallCreationConfig@ is an optional configuration that you can use
-- to choose which Availability Zones Firewall Manager creates Network
-- Firewall endpoints in.
--
-- /See:/ 'newFMSPolicyUpdateFirewallCreationConfigAction' smart constructor.
data FMSPolicyUpdateFirewallCreationConfigAction = FMSPolicyUpdateFirewallCreationConfigAction'
  { -- | A @FirewallCreationConfig@ that you can copy into your current policy\'s
    -- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_SecurityServicePolicyData.html SecurityServiceData>
    -- in order to remedy scope violations.
    firewallCreationConfig :: Prelude.Maybe Prelude.Text,
    -- | Describes the remedial action.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FMSPolicyUpdateFirewallCreationConfigAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallCreationConfig', 'fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig' - A @FirewallCreationConfig@ that you can copy into your current policy\'s
-- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_SecurityServicePolicyData.html SecurityServiceData>
-- in order to remedy scope violations.
--
-- 'description', 'fMSPolicyUpdateFirewallCreationConfigAction_description' - Describes the remedial action.
newFMSPolicyUpdateFirewallCreationConfigAction ::
  FMSPolicyUpdateFirewallCreationConfigAction
newFMSPolicyUpdateFirewallCreationConfigAction =
  FMSPolicyUpdateFirewallCreationConfigAction'
    { firewallCreationConfig =
        Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A @FirewallCreationConfig@ that you can copy into your current policy\'s
-- <https://docs.aws.amazon.com/fms/2018-01-01/APIReference/API_SecurityServicePolicyData.html SecurityServiceData>
-- in order to remedy scope violations.
fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig :: Lens.Lens' FMSPolicyUpdateFirewallCreationConfigAction (Prelude.Maybe Prelude.Text)
fMSPolicyUpdateFirewallCreationConfigAction_firewallCreationConfig = Lens.lens (\FMSPolicyUpdateFirewallCreationConfigAction' {firewallCreationConfig} -> firewallCreationConfig) (\s@FMSPolicyUpdateFirewallCreationConfigAction' {} a -> s {firewallCreationConfig = a} :: FMSPolicyUpdateFirewallCreationConfigAction)

-- | Describes the remedial action.
fMSPolicyUpdateFirewallCreationConfigAction_description :: Lens.Lens' FMSPolicyUpdateFirewallCreationConfigAction (Prelude.Maybe Prelude.Text)
fMSPolicyUpdateFirewallCreationConfigAction_description = Lens.lens (\FMSPolicyUpdateFirewallCreationConfigAction' {description} -> description) (\s@FMSPolicyUpdateFirewallCreationConfigAction' {} a -> s {description = a} :: FMSPolicyUpdateFirewallCreationConfigAction)

instance
  Core.FromJSON
    FMSPolicyUpdateFirewallCreationConfigAction
  where
  parseJSON =
    Core.withObject
      "FMSPolicyUpdateFirewallCreationConfigAction"
      ( \x ->
          FMSPolicyUpdateFirewallCreationConfigAction'
            Prelude.<$> (x Core..:? "FirewallCreationConfig")
              Prelude.<*> (x Core..:? "Description")
      )

instance
  Prelude.Hashable
    FMSPolicyUpdateFirewallCreationConfigAction
  where
  hashWithSalt
    _salt
    FMSPolicyUpdateFirewallCreationConfigAction' {..} =
      _salt `Prelude.hashWithSalt` firewallCreationConfig
        `Prelude.hashWithSalt` description

instance
  Prelude.NFData
    FMSPolicyUpdateFirewallCreationConfigAction
  where
  rnf FMSPolicyUpdateFirewallCreationConfigAction' {..} =
    Prelude.rnf firewallCreationConfig
      `Prelude.seq` Prelude.rnf description

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
-- Module      : Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StatelessCustomActionDefinition

-- | A custom action that can be used for stateless packet handling.
--
-- /See:/ 'newFirewallPolicyStatelessCustomActionsDetails' smart constructor.
data FirewallPolicyStatelessCustomActionsDetails = FirewallPolicyStatelessCustomActionsDetails'
  { -- | The definition of the custom action.
    actionDefinition :: Prelude.Maybe StatelessCustomActionDefinition,
    -- | The name of the custom action.
    actionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyStatelessCustomActionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionDefinition', 'firewallPolicyStatelessCustomActionsDetails_actionDefinition' - The definition of the custom action.
--
-- 'actionName', 'firewallPolicyStatelessCustomActionsDetails_actionName' - The name of the custom action.
newFirewallPolicyStatelessCustomActionsDetails ::
  FirewallPolicyStatelessCustomActionsDetails
newFirewallPolicyStatelessCustomActionsDetails =
  FirewallPolicyStatelessCustomActionsDetails'
    { actionDefinition =
        Prelude.Nothing,
      actionName = Prelude.Nothing
    }

-- | The definition of the custom action.
firewallPolicyStatelessCustomActionsDetails_actionDefinition :: Lens.Lens' FirewallPolicyStatelessCustomActionsDetails (Prelude.Maybe StatelessCustomActionDefinition)
firewallPolicyStatelessCustomActionsDetails_actionDefinition = Lens.lens (\FirewallPolicyStatelessCustomActionsDetails' {actionDefinition} -> actionDefinition) (\s@FirewallPolicyStatelessCustomActionsDetails' {} a -> s {actionDefinition = a} :: FirewallPolicyStatelessCustomActionsDetails)

-- | The name of the custom action.
firewallPolicyStatelessCustomActionsDetails_actionName :: Lens.Lens' FirewallPolicyStatelessCustomActionsDetails (Prelude.Maybe Prelude.Text)
firewallPolicyStatelessCustomActionsDetails_actionName = Lens.lens (\FirewallPolicyStatelessCustomActionsDetails' {actionName} -> actionName) (\s@FirewallPolicyStatelessCustomActionsDetails' {} a -> s {actionName = a} :: FirewallPolicyStatelessCustomActionsDetails)

instance
  Data.FromJSON
    FirewallPolicyStatelessCustomActionsDetails
  where
  parseJSON =
    Data.withObject
      "FirewallPolicyStatelessCustomActionsDetails"
      ( \x ->
          FirewallPolicyStatelessCustomActionsDetails'
            Prelude.<$> (x Data..:? "ActionDefinition")
              Prelude.<*> (x Data..:? "ActionName")
      )

instance
  Prelude.Hashable
    FirewallPolicyStatelessCustomActionsDetails
  where
  hashWithSalt
    _salt
    FirewallPolicyStatelessCustomActionsDetails' {..} =
      _salt `Prelude.hashWithSalt` actionDefinition
        `Prelude.hashWithSalt` actionName

instance
  Prelude.NFData
    FirewallPolicyStatelessCustomActionsDetails
  where
  rnf FirewallPolicyStatelessCustomActionsDetails' {..} =
    Prelude.rnf actionDefinition
      `Prelude.seq` Prelude.rnf actionName

instance
  Data.ToJSON
    FirewallPolicyStatelessCustomActionsDetails
  where
  toJSON
    FirewallPolicyStatelessCustomActionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ActionDefinition" Data..=)
                Prelude.<$> actionDefinition,
              ("ActionName" Data..=) Prelude.<$> actionName
            ]
        )

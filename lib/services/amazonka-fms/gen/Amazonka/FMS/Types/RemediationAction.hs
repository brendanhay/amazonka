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
-- Module      : Amazonka.FMS.Types.RemediationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.RemediationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.EC2AssociateRouteTableAction
import Amazonka.FMS.Types.EC2CopyRouteTableAction
import Amazonka.FMS.Types.EC2CreateRouteAction
import Amazonka.FMS.Types.EC2CreateRouteTableAction
import Amazonka.FMS.Types.EC2DeleteRouteAction
import Amazonka.FMS.Types.EC2ReplaceRouteAction
import Amazonka.FMS.Types.EC2ReplaceRouteTableAssociationAction
import Amazonka.FMS.Types.FMSPolicyUpdateFirewallCreationConfigAction
import qualified Amazonka.Prelude as Prelude

-- | Information about an individual action you can take to remediate a
-- violation.
--
-- /See:/ 'newRemediationAction' smart constructor.
data RemediationAction = RemediationAction'
  { -- | A description of a remediation action.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the AssociateRouteTable action in the Amazon EC2 API.
    eC2AssociateRouteTableAction :: Prelude.Maybe EC2AssociateRouteTableAction,
    -- | Information about the CopyRouteTable action in the Amazon EC2 API.
    eC2CopyRouteTableAction :: Prelude.Maybe EC2CopyRouteTableAction,
    -- | Information about the CreateRoute action in the Amazon EC2 API.
    eC2CreateRouteAction :: Prelude.Maybe EC2CreateRouteAction,
    -- | Information about the CreateRouteTable action in the Amazon EC2 API.
    eC2CreateRouteTableAction :: Prelude.Maybe EC2CreateRouteTableAction,
    -- | Information about the DeleteRoute action in the Amazon EC2 API.
    eC2DeleteRouteAction :: Prelude.Maybe EC2DeleteRouteAction,
    -- | Information about the ReplaceRoute action in the Amazon EC2 API.
    eC2ReplaceRouteAction :: Prelude.Maybe EC2ReplaceRouteAction,
    -- | Information about the ReplaceRouteTableAssociation action in the Amazon
    -- EC2 API.
    eC2ReplaceRouteTableAssociationAction :: Prelude.Maybe EC2ReplaceRouteTableAssociationAction,
    -- | The remedial action to take when updating a firewall configuration.
    fMSPolicyUpdateFirewallCreationConfigAction :: Prelude.Maybe FMSPolicyUpdateFirewallCreationConfigAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'remediationAction_description' - A description of a remediation action.
--
-- 'eC2AssociateRouteTableAction', 'remediationAction_eC2AssociateRouteTableAction' - Information about the AssociateRouteTable action in the Amazon EC2 API.
--
-- 'eC2CopyRouteTableAction', 'remediationAction_eC2CopyRouteTableAction' - Information about the CopyRouteTable action in the Amazon EC2 API.
--
-- 'eC2CreateRouteAction', 'remediationAction_eC2CreateRouteAction' - Information about the CreateRoute action in the Amazon EC2 API.
--
-- 'eC2CreateRouteTableAction', 'remediationAction_eC2CreateRouteTableAction' - Information about the CreateRouteTable action in the Amazon EC2 API.
--
-- 'eC2DeleteRouteAction', 'remediationAction_eC2DeleteRouteAction' - Information about the DeleteRoute action in the Amazon EC2 API.
--
-- 'eC2ReplaceRouteAction', 'remediationAction_eC2ReplaceRouteAction' - Information about the ReplaceRoute action in the Amazon EC2 API.
--
-- 'eC2ReplaceRouteTableAssociationAction', 'remediationAction_eC2ReplaceRouteTableAssociationAction' - Information about the ReplaceRouteTableAssociation action in the Amazon
-- EC2 API.
--
-- 'fMSPolicyUpdateFirewallCreationConfigAction', 'remediationAction_fMSPolicyUpdateFirewallCreationConfigAction' - The remedial action to take when updating a firewall configuration.
newRemediationAction ::
  RemediationAction
newRemediationAction =
  RemediationAction'
    { description = Prelude.Nothing,
      eC2AssociateRouteTableAction = Prelude.Nothing,
      eC2CopyRouteTableAction = Prelude.Nothing,
      eC2CreateRouteAction = Prelude.Nothing,
      eC2CreateRouteTableAction = Prelude.Nothing,
      eC2DeleteRouteAction = Prelude.Nothing,
      eC2ReplaceRouteAction = Prelude.Nothing,
      eC2ReplaceRouteTableAssociationAction =
        Prelude.Nothing,
      fMSPolicyUpdateFirewallCreationConfigAction =
        Prelude.Nothing
    }

-- | A description of a remediation action.
remediationAction_description :: Lens.Lens' RemediationAction (Prelude.Maybe Prelude.Text)
remediationAction_description = Lens.lens (\RemediationAction' {description} -> description) (\s@RemediationAction' {} a -> s {description = a} :: RemediationAction)

-- | Information about the AssociateRouteTable action in the Amazon EC2 API.
remediationAction_eC2AssociateRouteTableAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2AssociateRouteTableAction)
remediationAction_eC2AssociateRouteTableAction = Lens.lens (\RemediationAction' {eC2AssociateRouteTableAction} -> eC2AssociateRouteTableAction) (\s@RemediationAction' {} a -> s {eC2AssociateRouteTableAction = a} :: RemediationAction)

-- | Information about the CopyRouteTable action in the Amazon EC2 API.
remediationAction_eC2CopyRouteTableAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2CopyRouteTableAction)
remediationAction_eC2CopyRouteTableAction = Lens.lens (\RemediationAction' {eC2CopyRouteTableAction} -> eC2CopyRouteTableAction) (\s@RemediationAction' {} a -> s {eC2CopyRouteTableAction = a} :: RemediationAction)

-- | Information about the CreateRoute action in the Amazon EC2 API.
remediationAction_eC2CreateRouteAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2CreateRouteAction)
remediationAction_eC2CreateRouteAction = Lens.lens (\RemediationAction' {eC2CreateRouteAction} -> eC2CreateRouteAction) (\s@RemediationAction' {} a -> s {eC2CreateRouteAction = a} :: RemediationAction)

-- | Information about the CreateRouteTable action in the Amazon EC2 API.
remediationAction_eC2CreateRouteTableAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2CreateRouteTableAction)
remediationAction_eC2CreateRouteTableAction = Lens.lens (\RemediationAction' {eC2CreateRouteTableAction} -> eC2CreateRouteTableAction) (\s@RemediationAction' {} a -> s {eC2CreateRouteTableAction = a} :: RemediationAction)

-- | Information about the DeleteRoute action in the Amazon EC2 API.
remediationAction_eC2DeleteRouteAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2DeleteRouteAction)
remediationAction_eC2DeleteRouteAction = Lens.lens (\RemediationAction' {eC2DeleteRouteAction} -> eC2DeleteRouteAction) (\s@RemediationAction' {} a -> s {eC2DeleteRouteAction = a} :: RemediationAction)

-- | Information about the ReplaceRoute action in the Amazon EC2 API.
remediationAction_eC2ReplaceRouteAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2ReplaceRouteAction)
remediationAction_eC2ReplaceRouteAction = Lens.lens (\RemediationAction' {eC2ReplaceRouteAction} -> eC2ReplaceRouteAction) (\s@RemediationAction' {} a -> s {eC2ReplaceRouteAction = a} :: RemediationAction)

-- | Information about the ReplaceRouteTableAssociation action in the Amazon
-- EC2 API.
remediationAction_eC2ReplaceRouteTableAssociationAction :: Lens.Lens' RemediationAction (Prelude.Maybe EC2ReplaceRouteTableAssociationAction)
remediationAction_eC2ReplaceRouteTableAssociationAction = Lens.lens (\RemediationAction' {eC2ReplaceRouteTableAssociationAction} -> eC2ReplaceRouteTableAssociationAction) (\s@RemediationAction' {} a -> s {eC2ReplaceRouteTableAssociationAction = a} :: RemediationAction)

-- | The remedial action to take when updating a firewall configuration.
remediationAction_fMSPolicyUpdateFirewallCreationConfigAction :: Lens.Lens' RemediationAction (Prelude.Maybe FMSPolicyUpdateFirewallCreationConfigAction)
remediationAction_fMSPolicyUpdateFirewallCreationConfigAction = Lens.lens (\RemediationAction' {fMSPolicyUpdateFirewallCreationConfigAction} -> fMSPolicyUpdateFirewallCreationConfigAction) (\s@RemediationAction' {} a -> s {fMSPolicyUpdateFirewallCreationConfigAction = a} :: RemediationAction)

instance Data.FromJSON RemediationAction where
  parseJSON =
    Data.withObject
      "RemediationAction"
      ( \x ->
          RemediationAction'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EC2AssociateRouteTableAction")
            Prelude.<*> (x Data..:? "EC2CopyRouteTableAction")
            Prelude.<*> (x Data..:? "EC2CreateRouteAction")
            Prelude.<*> (x Data..:? "EC2CreateRouteTableAction")
            Prelude.<*> (x Data..:? "EC2DeleteRouteAction")
            Prelude.<*> (x Data..:? "EC2ReplaceRouteAction")
            Prelude.<*> (x Data..:? "EC2ReplaceRouteTableAssociationAction")
            Prelude.<*> ( x
                            Data..:? "FMSPolicyUpdateFirewallCreationConfigAction"
                        )
      )

instance Prelude.Hashable RemediationAction where
  hashWithSalt _salt RemediationAction' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eC2AssociateRouteTableAction
      `Prelude.hashWithSalt` eC2CopyRouteTableAction
      `Prelude.hashWithSalt` eC2CreateRouteAction
      `Prelude.hashWithSalt` eC2CreateRouteTableAction
      `Prelude.hashWithSalt` eC2DeleteRouteAction
      `Prelude.hashWithSalt` eC2ReplaceRouteAction
      `Prelude.hashWithSalt` eC2ReplaceRouteTableAssociationAction
      `Prelude.hashWithSalt` fMSPolicyUpdateFirewallCreationConfigAction

instance Prelude.NFData RemediationAction where
  rnf RemediationAction' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf eC2AssociateRouteTableAction
      `Prelude.seq` Prelude.rnf eC2CopyRouteTableAction
      `Prelude.seq` Prelude.rnf eC2CreateRouteAction
      `Prelude.seq` Prelude.rnf eC2CreateRouteTableAction
      `Prelude.seq` Prelude.rnf eC2DeleteRouteAction
      `Prelude.seq` Prelude.rnf eC2ReplaceRouteAction
      `Prelude.seq` Prelude.rnf eC2ReplaceRouteTableAssociationAction
      `Prelude.seq` Prelude.rnf
        fMSPolicyUpdateFirewallCreationConfigAction

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
-- Module      : Network.AWS.FMS.Types.PossibleRemediationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PossibleRemediationAction where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.RemediationActionWithOrder
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of remediation actions.
--
-- /See:/ 'newPossibleRemediationAction' smart constructor.
data PossibleRemediationAction = PossibleRemediationAction'
  { -- | Information about whether an action is taken by default.
    isDefaultAction :: Prelude.Maybe Prelude.Bool,
    -- | A description of the list of remediation actions.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ordered list of remediation actions.
    orderedRemediationActions :: [RemediationActionWithOrder]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PossibleRemediationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultAction', 'possibleRemediationAction_isDefaultAction' - Information about whether an action is taken by default.
--
-- 'description', 'possibleRemediationAction_description' - A description of the list of remediation actions.
--
-- 'orderedRemediationActions', 'possibleRemediationAction_orderedRemediationActions' - The ordered list of remediation actions.
newPossibleRemediationAction ::
  PossibleRemediationAction
newPossibleRemediationAction =
  PossibleRemediationAction'
    { isDefaultAction =
        Prelude.Nothing,
      description = Prelude.Nothing,
      orderedRemediationActions = Prelude.mempty
    }

-- | Information about whether an action is taken by default.
possibleRemediationAction_isDefaultAction :: Lens.Lens' PossibleRemediationAction (Prelude.Maybe Prelude.Bool)
possibleRemediationAction_isDefaultAction = Lens.lens (\PossibleRemediationAction' {isDefaultAction} -> isDefaultAction) (\s@PossibleRemediationAction' {} a -> s {isDefaultAction = a} :: PossibleRemediationAction)

-- | A description of the list of remediation actions.
possibleRemediationAction_description :: Lens.Lens' PossibleRemediationAction (Prelude.Maybe Prelude.Text)
possibleRemediationAction_description = Lens.lens (\PossibleRemediationAction' {description} -> description) (\s@PossibleRemediationAction' {} a -> s {description = a} :: PossibleRemediationAction)

-- | The ordered list of remediation actions.
possibleRemediationAction_orderedRemediationActions :: Lens.Lens' PossibleRemediationAction [RemediationActionWithOrder]
possibleRemediationAction_orderedRemediationActions = Lens.lens (\PossibleRemediationAction' {orderedRemediationActions} -> orderedRemediationActions) (\s@PossibleRemediationAction' {} a -> s {orderedRemediationActions = a} :: PossibleRemediationAction) Prelude.. Lens._Coerce

instance Core.FromJSON PossibleRemediationAction where
  parseJSON =
    Core.withObject
      "PossibleRemediationAction"
      ( \x ->
          PossibleRemediationAction'
            Prelude.<$> (x Core..:? "IsDefaultAction")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "OrderedRemediationActions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PossibleRemediationAction

instance Prelude.NFData PossibleRemediationAction

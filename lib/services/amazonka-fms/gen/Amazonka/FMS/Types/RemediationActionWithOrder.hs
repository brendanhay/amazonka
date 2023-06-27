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
-- Module      : Amazonka.FMS.Types.RemediationActionWithOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.RemediationActionWithOrder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.RemediationAction
import qualified Amazonka.Prelude as Prelude

-- | An ordered list of actions you can take to remediate a violation.
--
-- /See:/ 'newRemediationActionWithOrder' smart constructor.
data RemediationActionWithOrder = RemediationActionWithOrder'
  { -- | The order of the remediation actions in the list.
    order :: Prelude.Maybe Prelude.Int,
    -- | Information about an action you can take to remediate a violation.
    remediationAction :: Prelude.Maybe RemediationAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationActionWithOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'order', 'remediationActionWithOrder_order' - The order of the remediation actions in the list.
--
-- 'remediationAction', 'remediationActionWithOrder_remediationAction' - Information about an action you can take to remediate a violation.
newRemediationActionWithOrder ::
  RemediationActionWithOrder
newRemediationActionWithOrder =
  RemediationActionWithOrder'
    { order =
        Prelude.Nothing,
      remediationAction = Prelude.Nothing
    }

-- | The order of the remediation actions in the list.
remediationActionWithOrder_order :: Lens.Lens' RemediationActionWithOrder (Prelude.Maybe Prelude.Int)
remediationActionWithOrder_order = Lens.lens (\RemediationActionWithOrder' {order} -> order) (\s@RemediationActionWithOrder' {} a -> s {order = a} :: RemediationActionWithOrder)

-- | Information about an action you can take to remediate a violation.
remediationActionWithOrder_remediationAction :: Lens.Lens' RemediationActionWithOrder (Prelude.Maybe RemediationAction)
remediationActionWithOrder_remediationAction = Lens.lens (\RemediationActionWithOrder' {remediationAction} -> remediationAction) (\s@RemediationActionWithOrder' {} a -> s {remediationAction = a} :: RemediationActionWithOrder)

instance Data.FromJSON RemediationActionWithOrder where
  parseJSON =
    Data.withObject
      "RemediationActionWithOrder"
      ( \x ->
          RemediationActionWithOrder'
            Prelude.<$> (x Data..:? "Order")
            Prelude.<*> (x Data..:? "RemediationAction")
      )

instance Prelude.Hashable RemediationActionWithOrder where
  hashWithSalt _salt RemediationActionWithOrder' {..} =
    _salt
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` remediationAction

instance Prelude.NFData RemediationActionWithOrder where
  rnf RemediationActionWithOrder' {..} =
    Prelude.rnf order
      `Prelude.seq` Prelude.rnf remediationAction

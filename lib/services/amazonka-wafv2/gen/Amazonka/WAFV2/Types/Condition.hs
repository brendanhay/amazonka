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
-- Module      : Amazonka.WAFV2.Types.Condition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ActionCondition
import Amazonka.WAFV2.Types.LabelNameCondition

-- | A single match condition for a Filter.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | A single label name condition. This is the fully qualified label name
    -- that a log record must contain in order to meet the condition. Fully
    -- qualified labels have a prefix, optional namespaces, and label name. The
    -- prefix identifies the rule group or web ACL context of the rule that
    -- added the label.
    labelNameCondition :: Prelude.Maybe LabelNameCondition,
    -- | A single action condition. This is the action setting that a log record
    -- must contain in order to meet the condition.
    actionCondition :: Prelude.Maybe ActionCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelNameCondition', 'condition_labelNameCondition' - A single label name condition. This is the fully qualified label name
-- that a log record must contain in order to meet the condition. Fully
-- qualified labels have a prefix, optional namespaces, and label name. The
-- prefix identifies the rule group or web ACL context of the rule that
-- added the label.
--
-- 'actionCondition', 'condition_actionCondition' - A single action condition. This is the action setting that a log record
-- must contain in order to meet the condition.
newCondition ::
  Condition
newCondition =
  Condition'
    { labelNameCondition = Prelude.Nothing,
      actionCondition = Prelude.Nothing
    }

-- | A single label name condition. This is the fully qualified label name
-- that a log record must contain in order to meet the condition. Fully
-- qualified labels have a prefix, optional namespaces, and label name. The
-- prefix identifies the rule group or web ACL context of the rule that
-- added the label.
condition_labelNameCondition :: Lens.Lens' Condition (Prelude.Maybe LabelNameCondition)
condition_labelNameCondition = Lens.lens (\Condition' {labelNameCondition} -> labelNameCondition) (\s@Condition' {} a -> s {labelNameCondition = a} :: Condition)

-- | A single action condition. This is the action setting that a log record
-- must contain in order to meet the condition.
condition_actionCondition :: Lens.Lens' Condition (Prelude.Maybe ActionCondition)
condition_actionCondition = Lens.lens (\Condition' {actionCondition} -> actionCondition) (\s@Condition' {} a -> s {actionCondition = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..:? "LabelNameCondition")
            Prelude.<*> (x Core..:? "ActionCondition")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` labelNameCondition
      `Prelude.hashWithSalt` actionCondition

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf labelNameCondition
      `Prelude.seq` Prelude.rnf actionCondition

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LabelNameCondition" Core..=)
              Prelude.<$> labelNameCondition,
            ("ActionCondition" Core..=)
              Prelude.<$> actionCondition
          ]
      )

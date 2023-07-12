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
-- Module      : Amazonka.WAFV2.Types.LabelNameCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.LabelNameCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single label name condition for a Condition in a logging filter.
--
-- /See:/ 'newLabelNameCondition' smart constructor.
data LabelNameCondition = LabelNameCondition'
  { -- | The label name that a log record must contain in order to meet the
    -- condition. This must be a fully qualified label name. Fully qualified
    -- labels have a prefix, optional namespaces, and label name. The prefix
    -- identifies the rule group or web ACL context of the rule that added the
    -- label.
    labelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelNameCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelName', 'labelNameCondition_labelName' - The label name that a log record must contain in order to meet the
-- condition. This must be a fully qualified label name. Fully qualified
-- labels have a prefix, optional namespaces, and label name. The prefix
-- identifies the rule group or web ACL context of the rule that added the
-- label.
newLabelNameCondition ::
  -- | 'labelName'
  Prelude.Text ->
  LabelNameCondition
newLabelNameCondition pLabelName_ =
  LabelNameCondition' {labelName = pLabelName_}

-- | The label name that a log record must contain in order to meet the
-- condition. This must be a fully qualified label name. Fully qualified
-- labels have a prefix, optional namespaces, and label name. The prefix
-- identifies the rule group or web ACL context of the rule that added the
-- label.
labelNameCondition_labelName :: Lens.Lens' LabelNameCondition Prelude.Text
labelNameCondition_labelName = Lens.lens (\LabelNameCondition' {labelName} -> labelName) (\s@LabelNameCondition' {} a -> s {labelName = a} :: LabelNameCondition)

instance Data.FromJSON LabelNameCondition where
  parseJSON =
    Data.withObject
      "LabelNameCondition"
      ( \x ->
          LabelNameCondition'
            Prelude.<$> (x Data..: "LabelName")
      )

instance Prelude.Hashable LabelNameCondition where
  hashWithSalt _salt LabelNameCondition' {..} =
    _salt `Prelude.hashWithSalt` labelName

instance Prelude.NFData LabelNameCondition where
  rnf LabelNameCondition' {..} = Prelude.rnf labelName

instance Data.ToJSON LabelNameCondition where
  toJSON LabelNameCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LabelName" Data..= labelName)]
      )

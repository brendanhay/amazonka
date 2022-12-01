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
-- Module      : Amazonka.WAFV2.Types.LabelSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.LabelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | List of labels used by one or more of the rules of a RuleGroup. This
-- summary object is used for the following rule group lists:
--
-- -   @AvailableLabels@ - Labels that rules add to matching requests.
--     These labels are defined in the @RuleLabels@ for a Rule.
--
-- -   @ConsumedLabels@ - Labels that rules match against. These labels are
--     defined in a @LabelMatchStatement@ specification, in the Statement
--     definition of a rule.
--
-- /See:/ 'newLabelSummary' smart constructor.
data LabelSummary = LabelSummary'
  { -- | An individual label specification.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'labelSummary_name' - An individual label specification.
newLabelSummary ::
  LabelSummary
newLabelSummary =
  LabelSummary' {name = Prelude.Nothing}

-- | An individual label specification.
labelSummary_name :: Lens.Lens' LabelSummary (Prelude.Maybe Prelude.Text)
labelSummary_name = Lens.lens (\LabelSummary' {name} -> name) (\s@LabelSummary' {} a -> s {name = a} :: LabelSummary)

instance Core.FromJSON LabelSummary where
  parseJSON =
    Core.withObject
      "LabelSummary"
      ( \x ->
          LabelSummary' Prelude.<$> (x Core..:? "Name")
      )

instance Prelude.Hashable LabelSummary where
  hashWithSalt _salt LabelSummary' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData LabelSummary where
  rnf LabelSummary' {..} = Prelude.rnf name

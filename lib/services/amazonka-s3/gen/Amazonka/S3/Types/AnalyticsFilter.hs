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
-- Module      : Amazonka.S3.Types.AnalyticsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AnalyticsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.AnalyticsAndOperator
import Amazonka.S3.Types.Tag

-- | The filter used to describe a set of objects for analyses. A filter must
-- have exactly one prefix, one tag, or one conjunction
-- (AnalyticsAndOperator). If no filter is provided, all objects will be
-- considered in any analysis.
--
-- /See:/ 'newAnalyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { -- | The tag to use when evaluating an analytics filter.
    tag :: Prelude.Maybe Tag,
    -- | The prefix to use when evaluating an analytics filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A conjunction (logical AND) of predicates, which is used in evaluating
    -- an analytics filter. The operator must have at least two predicates.
    and :: Prelude.Maybe AnalyticsAndOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyticsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tag', 'analyticsFilter_tag' - The tag to use when evaluating an analytics filter.
--
-- 'prefix', 'analyticsFilter_prefix' - The prefix to use when evaluating an analytics filter.
--
-- 'and', 'analyticsFilter_and' - A conjunction (logical AND) of predicates, which is used in evaluating
-- an analytics filter. The operator must have at least two predicates.
newAnalyticsFilter ::
  AnalyticsFilter
newAnalyticsFilter =
  AnalyticsFilter'
    { tag = Prelude.Nothing,
      prefix = Prelude.Nothing,
      and = Prelude.Nothing
    }

-- | The tag to use when evaluating an analytics filter.
analyticsFilter_tag :: Lens.Lens' AnalyticsFilter (Prelude.Maybe Tag)
analyticsFilter_tag = Lens.lens (\AnalyticsFilter' {tag} -> tag) (\s@AnalyticsFilter' {} a -> s {tag = a} :: AnalyticsFilter)

-- | The prefix to use when evaluating an analytics filter.
analyticsFilter_prefix :: Lens.Lens' AnalyticsFilter (Prelude.Maybe Prelude.Text)
analyticsFilter_prefix = Lens.lens (\AnalyticsFilter' {prefix} -> prefix) (\s@AnalyticsFilter' {} a -> s {prefix = a} :: AnalyticsFilter)

-- | A conjunction (logical AND) of predicates, which is used in evaluating
-- an analytics filter. The operator must have at least two predicates.
analyticsFilter_and :: Lens.Lens' AnalyticsFilter (Prelude.Maybe AnalyticsAndOperator)
analyticsFilter_and = Lens.lens (\AnalyticsFilter' {and} -> and) (\s@AnalyticsFilter' {} a -> s {and = a} :: AnalyticsFilter)

instance Core.FromXML AnalyticsFilter where
  parseXML x =
    AnalyticsFilter'
      Prelude.<$> (x Core..@? "Tag")
      Prelude.<*> (x Core..@? "Prefix")
      Prelude.<*> (x Core..@? "And")

instance Prelude.Hashable AnalyticsFilter where
  hashWithSalt _salt AnalyticsFilter' {..} =
    _salt `Prelude.hashWithSalt` tag
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` and

instance Prelude.NFData AnalyticsFilter where
  rnf AnalyticsFilter' {..} =
    Prelude.rnf tag
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf and

instance Core.ToXML AnalyticsFilter where
  toXML AnalyticsFilter' {..} =
    Prelude.mconcat
      [ "Tag" Core.@= tag,
        "Prefix" Core.@= prefix,
        "And" Core.@= and
      ]

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
-- Module      : Amazonka.S3.Types.AnalyticsAndOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AnalyticsAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates in any
-- combination, and an object must match all of the predicates for the
-- filter to apply.
--
-- /See:/ 'newAnalyticsAndOperator' smart constructor.
data AnalyticsAndOperator = AnalyticsAndOperator'
  { -- | The list of tags to use when evaluating an AND predicate.
    tags :: Prelude.Maybe [Tag],
    -- | The prefix to use when evaluating an AND predicate: The prefix that an
    -- object must have to be included in the metrics results.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyticsAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'analyticsAndOperator_tags' - The list of tags to use when evaluating an AND predicate.
--
-- 'prefix', 'analyticsAndOperator_prefix' - The prefix to use when evaluating an AND predicate: The prefix that an
-- object must have to be included in the metrics results.
newAnalyticsAndOperator ::
  AnalyticsAndOperator
newAnalyticsAndOperator =
  AnalyticsAndOperator'
    { tags = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The list of tags to use when evaluating an AND predicate.
analyticsAndOperator_tags :: Lens.Lens' AnalyticsAndOperator (Prelude.Maybe [Tag])
analyticsAndOperator_tags = Lens.lens (\AnalyticsAndOperator' {tags} -> tags) (\s@AnalyticsAndOperator' {} a -> s {tags = a} :: AnalyticsAndOperator) Prelude.. Lens.mapping Lens.coerced

-- | The prefix to use when evaluating an AND predicate: The prefix that an
-- object must have to be included in the metrics results.
analyticsAndOperator_prefix :: Lens.Lens' AnalyticsAndOperator (Prelude.Maybe Prelude.Text)
analyticsAndOperator_prefix = Lens.lens (\AnalyticsAndOperator' {prefix} -> prefix) (\s@AnalyticsAndOperator' {} a -> s {prefix = a} :: AnalyticsAndOperator)

instance Core.FromXML AnalyticsAndOperator where
  parseXML x =
    AnalyticsAndOperator'
      Prelude.<$> ( x Core..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> (x Core..@? "Prefix")

instance Prelude.Hashable AnalyticsAndOperator where
  hashWithSalt _salt AnalyticsAndOperator' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData AnalyticsAndOperator where
  rnf AnalyticsAndOperator' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf prefix

instance Core.ToXML AnalyticsAndOperator where
  toXML AnalyticsAndOperator' {..} =
    Prelude.mconcat
      [ "Tag"
          Core.@= Core.toXML (Core.toXMLList "Tag" Prelude.<$> tags),
        "Prefix" Core.@= prefix
      ]

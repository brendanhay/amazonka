{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.MetricsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.Tag

-- | Specifies a metrics configuration filter. The metrics configuration only
-- includes objects that meet the filter\'s criteria. A filter must be a
-- prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- /See:/ 'newMetricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { -- | The prefix used when evaluating a metrics filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A conjunction (logical AND) of predicates, which is used in evaluating a
    -- metrics filter. The operator must have at least two predicates, and an
    -- object must match all of the predicates in order for the filter to
    -- apply.
    and :: Prelude.Maybe MetricsAndOperator,
    -- | The tag used when evaluating a metrics filter.
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'metricsFilter_prefix' - The prefix used when evaluating a metrics filter.
--
-- 'and', 'metricsFilter_and' - A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- 'tag', 'metricsFilter_tag' - The tag used when evaluating a metrics filter.
newMetricsFilter ::
  MetricsFilter
newMetricsFilter =
  MetricsFilter'
    { prefix = Prelude.Nothing,
      and = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | The prefix used when evaluating a metrics filter.
metricsFilter_prefix :: Lens.Lens' MetricsFilter (Prelude.Maybe Prelude.Text)
metricsFilter_prefix = Lens.lens (\MetricsFilter' {prefix} -> prefix) (\s@MetricsFilter' {} a -> s {prefix = a} :: MetricsFilter)

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
metricsFilter_and :: Lens.Lens' MetricsFilter (Prelude.Maybe MetricsAndOperator)
metricsFilter_and = Lens.lens (\MetricsFilter' {and} -> and) (\s@MetricsFilter' {} a -> s {and = a} :: MetricsFilter)

-- | The tag used when evaluating a metrics filter.
metricsFilter_tag :: Lens.Lens' MetricsFilter (Prelude.Maybe Tag)
metricsFilter_tag = Lens.lens (\MetricsFilter' {tag} -> tag) (\s@MetricsFilter' {} a -> s {tag = a} :: MetricsFilter)

instance Prelude.FromXML MetricsFilter where
  parseXML x =
    MetricsFilter'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> (x Prelude..@? "And")
      Prelude.<*> (x Prelude..@? "Tag")

instance Prelude.Hashable MetricsFilter

instance Prelude.NFData MetricsFilter

instance Prelude.ToXML MetricsFilter where
  toXML MetricsFilter' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "And" Prelude.@= and,
        "Tag" Prelude.@= tag
      ]

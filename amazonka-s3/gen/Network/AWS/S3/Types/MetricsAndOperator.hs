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
-- Module      : Network.AWS.S3.Types.MetricsAndOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsAndOperator where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- /See:/ 'newMetricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { -- | The prefix used when evaluating an AND predicate.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The list of tags used when evaluating an AND predicate.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetricsAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'metricsAndOperator_prefix' - The prefix used when evaluating an AND predicate.
--
-- 'tags', 'metricsAndOperator_tags' - The list of tags used when evaluating an AND predicate.
newMetricsAndOperator ::
  MetricsAndOperator
newMetricsAndOperator =
  MetricsAndOperator'
    { prefix = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The prefix used when evaluating an AND predicate.
metricsAndOperator_prefix :: Lens.Lens' MetricsAndOperator (Prelude.Maybe Prelude.Text)
metricsAndOperator_prefix = Lens.lens (\MetricsAndOperator' {prefix} -> prefix) (\s@MetricsAndOperator' {} a -> s {prefix = a} :: MetricsAndOperator)

-- | The list of tags used when evaluating an AND predicate.
metricsAndOperator_tags :: Lens.Lens' MetricsAndOperator (Prelude.Maybe [Tag])
metricsAndOperator_tags = Lens.lens (\MetricsAndOperator' {tags} -> tags) (\s@MetricsAndOperator' {} a -> s {tags = a} :: MetricsAndOperator) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML MetricsAndOperator where
  parseXML x =
    MetricsAndOperator'
      Prelude.<$> (x Prelude..@? "Prefix")
      Prelude.<*> ( x Prelude..@? "Tag" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance Prelude.Hashable MetricsAndOperator

instance Prelude.NFData MetricsAndOperator

instance Prelude.ToXML MetricsAndOperator where
  toXML MetricsAndOperator' {..} =
    Prelude.mconcat
      [ "Prefix" Prelude.@= prefix,
        "Tag"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Tag" Prelude.<$> tags)
      ]

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
-- Module      : Amazonka.S3.Types.MetricsAndOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.MetricsAndOperator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- /See:/ 'newMetricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { -- | The access point ARN used when evaluating an @AND@ predicate.
    accessPointArn :: Prelude.Maybe Prelude.Text,
    -- | The prefix used when evaluating an AND predicate.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The list of tags used when evaluating an AND predicate.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricsAndOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointArn', 'metricsAndOperator_accessPointArn' - The access point ARN used when evaluating an @AND@ predicate.
--
-- 'prefix', 'metricsAndOperator_prefix' - The prefix used when evaluating an AND predicate.
--
-- 'tags', 'metricsAndOperator_tags' - The list of tags used when evaluating an AND predicate.
newMetricsAndOperator ::
  MetricsAndOperator
newMetricsAndOperator =
  MetricsAndOperator'
    { accessPointArn =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The access point ARN used when evaluating an @AND@ predicate.
metricsAndOperator_accessPointArn :: Lens.Lens' MetricsAndOperator (Prelude.Maybe Prelude.Text)
metricsAndOperator_accessPointArn = Lens.lens (\MetricsAndOperator' {accessPointArn} -> accessPointArn) (\s@MetricsAndOperator' {} a -> s {accessPointArn = a} :: MetricsAndOperator)

-- | The prefix used when evaluating an AND predicate.
metricsAndOperator_prefix :: Lens.Lens' MetricsAndOperator (Prelude.Maybe Prelude.Text)
metricsAndOperator_prefix = Lens.lens (\MetricsAndOperator' {prefix} -> prefix) (\s@MetricsAndOperator' {} a -> s {prefix = a} :: MetricsAndOperator)

-- | The list of tags used when evaluating an AND predicate.
metricsAndOperator_tags :: Lens.Lens' MetricsAndOperator (Prelude.Maybe [Tag])
metricsAndOperator_tags = Lens.lens (\MetricsAndOperator' {tags} -> tags) (\s@MetricsAndOperator' {} a -> s {tags = a} :: MetricsAndOperator) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML MetricsAndOperator where
  parseXML x =
    MetricsAndOperator'
      Prelude.<$> (x Data..@? "AccessPointArn")
      Prelude.<*> (x Data..@? "Prefix")
      Prelude.<*> ( x Data..@? "Tag" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )

instance Prelude.Hashable MetricsAndOperator where
  hashWithSalt _salt MetricsAndOperator' {..} =
    _salt `Prelude.hashWithSalt` accessPointArn
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` tags

instance Prelude.NFData MetricsAndOperator where
  rnf MetricsAndOperator' {..} =
    Prelude.rnf accessPointArn
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf tags

instance Data.ToXML MetricsAndOperator where
  toXML MetricsAndOperator' {..} =
    Prelude.mconcat
      [ "AccessPointArn" Data.@= accessPointArn,
        "Prefix" Data.@= prefix,
        "Tag"
          Data.@= Data.toXML (Data.toXMLList "Tag" Prelude.<$> tags)
      ]

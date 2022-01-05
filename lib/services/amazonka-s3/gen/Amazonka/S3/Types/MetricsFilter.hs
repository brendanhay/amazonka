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
-- Module      : Amazonka.S3.Types.MetricsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.MetricsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.MetricsAndOperator
import Amazonka.S3.Types.Tag

-- | Specifies a metrics configuration filter. The metrics configuration only
-- includes objects that meet the filter\'s criteria. A filter must be a
-- prefix, an object tag, an access point ARN, or a conjunction
-- (MetricsAndOperator). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>.
--
-- /See:/ 'newMetricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { -- | The tag used when evaluating a metrics filter.
    tag :: Prelude.Maybe Tag,
    -- | The prefix used when evaluating a metrics filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A conjunction (logical AND) of predicates, which is used in evaluating a
    -- metrics filter. The operator must have at least two predicates, and an
    -- object must match all of the predicates in order for the filter to
    -- apply.
    and :: Prelude.Maybe MetricsAndOperator,
    -- | The access point ARN used when evaluating a metrics filter.
    accessPointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tag', 'metricsFilter_tag' - The tag used when evaluating a metrics filter.
--
-- 'prefix', 'metricsFilter_prefix' - The prefix used when evaluating a metrics filter.
--
-- 'and', 'metricsFilter_and' - A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
--
-- 'accessPointArn', 'metricsFilter_accessPointArn' - The access point ARN used when evaluating a metrics filter.
newMetricsFilter ::
  MetricsFilter
newMetricsFilter =
  MetricsFilter'
    { tag = Prelude.Nothing,
      prefix = Prelude.Nothing,
      and = Prelude.Nothing,
      accessPointArn = Prelude.Nothing
    }

-- | The tag used when evaluating a metrics filter.
metricsFilter_tag :: Lens.Lens' MetricsFilter (Prelude.Maybe Tag)
metricsFilter_tag = Lens.lens (\MetricsFilter' {tag} -> tag) (\s@MetricsFilter' {} a -> s {tag = a} :: MetricsFilter)

-- | The prefix used when evaluating a metrics filter.
metricsFilter_prefix :: Lens.Lens' MetricsFilter (Prelude.Maybe Prelude.Text)
metricsFilter_prefix = Lens.lens (\MetricsFilter' {prefix} -> prefix) (\s@MetricsFilter' {} a -> s {prefix = a} :: MetricsFilter)

-- | A conjunction (logical AND) of predicates, which is used in evaluating a
-- metrics filter. The operator must have at least two predicates, and an
-- object must match all of the predicates in order for the filter to
-- apply.
metricsFilter_and :: Lens.Lens' MetricsFilter (Prelude.Maybe MetricsAndOperator)
metricsFilter_and = Lens.lens (\MetricsFilter' {and} -> and) (\s@MetricsFilter' {} a -> s {and = a} :: MetricsFilter)

-- | The access point ARN used when evaluating a metrics filter.
metricsFilter_accessPointArn :: Lens.Lens' MetricsFilter (Prelude.Maybe Prelude.Text)
metricsFilter_accessPointArn = Lens.lens (\MetricsFilter' {accessPointArn} -> accessPointArn) (\s@MetricsFilter' {} a -> s {accessPointArn = a} :: MetricsFilter)

instance Core.FromXML MetricsFilter where
  parseXML x =
    MetricsFilter'
      Prelude.<$> (x Core..@? "Tag")
      Prelude.<*> (x Core..@? "Prefix")
      Prelude.<*> (x Core..@? "And")
      Prelude.<*> (x Core..@? "AccessPointArn")

instance Prelude.Hashable MetricsFilter where
  hashWithSalt _salt MetricsFilter' {..} =
    _salt `Prelude.hashWithSalt` tag
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` and
      `Prelude.hashWithSalt` accessPointArn

instance Prelude.NFData MetricsFilter where
  rnf MetricsFilter' {..} =
    Prelude.rnf tag
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf and
      `Prelude.seq` Prelude.rnf accessPointArn

instance Core.ToXML MetricsFilter where
  toXML MetricsFilter' {..} =
    Prelude.mconcat
      [ "Tag" Core.@= tag,
        "Prefix" Core.@= prefix,
        "And" Core.@= and,
        "AccessPointArn" Core.@= accessPointArn
      ]

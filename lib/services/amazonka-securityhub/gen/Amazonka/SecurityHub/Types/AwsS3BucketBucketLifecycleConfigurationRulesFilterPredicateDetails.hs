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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails

-- | The configuration for the filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
  { -- | Whether to use @AND@ or @OR@ to join the operands. Valid values are
    -- @LifecycleAndOperator@ or @LifecycleOrOperator@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A tag filter.
    tag :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails,
    -- | A prefix filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The values to use for the filter.
    operands :: Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type' - Whether to use @AND@ or @OR@ to join the operands. Valid values are
-- @LifecycleAndOperator@ or @LifecycleOrOperator@.
--
-- 'tag', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag' - A tag filter.
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix' - A prefix filter.
--
-- 'operands', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands' - The values to use for the filter.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
    { type' =
        Prelude.Nothing,
      tag =
        Prelude.Nothing,
      prefix =
        Prelude.Nothing,
      operands =
        Prelude.Nothing
    }

-- | Whether to use @AND@ or @OR@ to join the operands. Valid values are
-- @LifecycleAndOperator@ or @LifecycleOrOperator@.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {type'} -> type') (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {type' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | A tag filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {tag} -> tag) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {tag = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | A prefix filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | The values to use for the filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails])
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {operands} -> operands) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {operands = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
            Prelude.<$> (x Core..:? "Type") Prelude.<*> (x Core..:? "Tag")
              Prelude.<*> (x Core..:? "Prefix")
              Prelude.<*> (x Core..:? "Operands" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` tag
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` operands

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      Prelude.rnf type'
        `Prelude.seq` Prelude.rnf tag
        `Prelude.seq` Prelude.rnf prefix
        `Prelude.seq` Prelude.rnf operands

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Type" Core..=) Prelude.<$> type',
              ("Tag" Core..=) Prelude.<$> tag,
              ("Prefix" Core..=) Prelude.<$> prefix,
              ("Operands" Core..=) Prelude.<$> operands
            ]
        )

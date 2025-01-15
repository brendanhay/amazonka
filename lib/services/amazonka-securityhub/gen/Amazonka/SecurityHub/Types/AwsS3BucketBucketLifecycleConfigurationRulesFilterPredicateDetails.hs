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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails

-- | The configuration for the filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
  { -- | The values to use for the filter.
    operands :: Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails],
    -- | A prefix filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A tag filter.
    tag :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails,
    -- | Whether to use @AND@ or @OR@ to join the operands. Valid values are
    -- @LifecycleAndOperator@ or @LifecycleOrOperator@.
    type' :: Prelude.Maybe Prelude.Text
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
-- 'operands', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands' - The values to use for the filter.
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix' - A prefix filter.
--
-- 'tag', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag' - A tag filter.
--
-- 'type'', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type' - Whether to use @AND@ or @OR@ to join the operands. Valid values are
-- @LifecycleAndOperator@ or @LifecycleOrOperator@.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
    { operands =
        Prelude.Nothing,
      prefix =
        Prelude.Nothing,
      tag =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | The values to use for the filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe [AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails])
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {operands} -> operands) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {operands = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails) Prelude.. Lens.mapping Lens.coerced

-- | A prefix filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | A tag filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {tag} -> tag) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {tag = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | Whether to use @AND@ or @OR@ to join the operands. Valid values are
-- @LifecycleAndOperator@ or @LifecycleOrOperator@.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {type'} -> type') (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {type' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

instance
  Data.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
            Prelude.<$> (x Data..:? "Operands" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "Tag")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      _salt
        `Prelude.hashWithSalt` operands
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` tag
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      Prelude.rnf operands `Prelude.seq`
        Prelude.rnf prefix `Prelude.seq`
          Prelude.rnf tag `Prelude.seq`
            Prelude.rnf type'

instance
  Data.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Operands" Data..=) Prelude.<$> operands,
              ("Prefix" Data..=) Prelude.<$> prefix,
              ("Tag" Data..=) Prelude.<$> tag,
              ("Type" Data..=) Prelude.<$> type'
            ]
        )

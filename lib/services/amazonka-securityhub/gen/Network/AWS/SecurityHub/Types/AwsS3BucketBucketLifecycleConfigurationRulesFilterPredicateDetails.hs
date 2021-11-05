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
-- Module      : Network.AWS.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
import Network.AWS.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails

-- | The configuration for the filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
  { -- | A tag filter.
    tag :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails,
    -- | A prefix filter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Whether to use @AND@ or @OR@ to join the operands.
    type' :: Prelude.Maybe Prelude.Text,
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
-- 'tag', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag' - A tag filter.
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix' - A prefix filter.
--
-- 'type'', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type' - Whether to use @AND@ or @OR@ to join the operands.
--
-- 'operands', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_operands' - The values to use for the filter.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails'
    { tag =
        Prelude.Nothing,
      prefix =
        Prelude.Nothing,
      type' =
        Prelude.Nothing,
      operands =
        Prelude.Nothing
    }

-- | A tag filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_tag = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {tag} -> tag) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {tag = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | A prefix filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

-- | Whether to use @AND@ or @OR@ to join the operands.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails_type = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {type'} -> type') (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {} a -> s {type' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)

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
            Prelude.<$> (x Core..:? "Tag") Prelude.<*> (x Core..:? "Prefix")
              Prelude.<*> (x Core..:? "Type")
              Prelude.<*> (x Core..:? "Operands" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Tag" Core..=) Prelude.<$> tag,
              ("Prefix" Core..=) Prelude.<$> prefix,
              ("Type" Core..=) Prelude.<$> type',
              ("Operands" Core..=) Prelude.<$> operands
            ]
        )

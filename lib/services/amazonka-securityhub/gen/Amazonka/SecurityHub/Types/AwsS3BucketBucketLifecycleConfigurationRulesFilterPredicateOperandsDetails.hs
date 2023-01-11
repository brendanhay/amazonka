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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails

-- | A value to use for the filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
  { -- | Prefix text for matching objects.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A tag that is assigned to matching objects.
    tag :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails,
    -- | The type of filter value. Valid values are @LifecyclePrefixPredicate@ or
    -- @LifecycleTagPredicate@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix' - Prefix text for matching objects.
--
-- 'tag', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag' - A tag that is assigned to matching objects.
--
-- 'type'', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type' - The type of filter value. Valid values are @LifecyclePrefixPredicate@ or
-- @LifecycleTagPredicate@.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
    { prefix =
        Prelude.Nothing,
      tag =
        Prelude.Nothing,
      type' =
        Prelude.Nothing
    }

-- | Prefix text for matching objects.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_prefix = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {prefix} -> prefix) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {prefix = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

-- | A tag that is assigned to matching objects.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_tag = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {tag} -> tag) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {tag = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

-- | The type of filter value. Valid values are @LifecyclePrefixPredicate@ or
-- @LifecycleTagPredicate@.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails_type = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {type'} -> type') (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {} a -> s {type' = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails)

instance
  Data.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails'
            Prelude.<$> (x Data..:? "Prefix") Prelude.<*> (x Data..:? "Tag")
              Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      _salt `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` tag
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      Prelude.rnf prefix
        `Prelude.seq` Prelude.rnf tag
        `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Prefix" Data..=) Prelude.<$> prefix,
              ("Tag" Data..=) Prelude.<$> tag,
              ("Type" Data..=) Prelude.<$> type'
            ]
        )

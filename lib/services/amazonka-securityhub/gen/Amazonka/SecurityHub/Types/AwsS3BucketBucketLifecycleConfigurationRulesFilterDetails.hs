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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails

-- | Identifies the objects that a rule applies to.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails'
  { -- | The configuration for the filter.
    predicate :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predicate', 'awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate' - The configuration for the filter.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails'
    { predicate =
        Prelude.Nothing
    }

-- | The configuration for the filter.
awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateDetails)
awsS3BucketBucketLifecycleConfigurationRulesFilterDetails_predicate = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' {predicate} -> predicate) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' {} a -> s {predicate = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails'
            Prelude.<$> (x Core..:? "Predicate")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' {..} =
      _salt `Prelude.hashWithSalt` predicate

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' {..} =
      Prelude.rnf predicate

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Predicate" Core..=) Prelude.<$> predicate]
        )

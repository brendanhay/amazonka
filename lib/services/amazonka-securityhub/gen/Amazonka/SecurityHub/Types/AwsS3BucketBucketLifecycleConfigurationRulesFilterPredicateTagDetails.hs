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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A tag filter.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails'
  { -- | The tag value
    value :: Prelude.Maybe Prelude.Text,
    -- | The tag key.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_value' - The tag value
--
-- 'key', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_key' - The tag key.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails'
    { value =
        Prelude.Nothing,
      key =
        Prelude.Nothing
    }

-- | The tag value
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_value :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_value = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' {value} -> value) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' {} a -> s {value = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails)

-- | The tag key.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_key :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails_key = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' {key} -> key) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' {} a -> s {key = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Key")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateTagDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Value" Core..=) Prelude.<$> value,
              ("Key" Core..=) Prelude.<$> key
            ]
        )

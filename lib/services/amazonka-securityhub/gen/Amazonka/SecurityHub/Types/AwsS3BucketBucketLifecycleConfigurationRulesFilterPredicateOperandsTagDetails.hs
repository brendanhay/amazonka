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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A tag that is assigned to matching objects.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails = AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails'
  { -- | The tag key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_key' - The tag key.
--
-- 'value', 'awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_value' - The tag value.
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
newAwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails'
    { key =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | The tag key.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_key :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_key = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {key} -> key) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {} a -> s {key = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails)

-- | The tag value.
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_value :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails_value = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {value} -> value) (\s@AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {} a -> s {value = a} :: AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails)

instance
  Data.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails'
            Prelude.<$> (x Data..:? "Key") Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {..} =
      Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance
  Data.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesFilterPredicateOperandsTagDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Key" Data..=) Prelude.<$> key,
              ("Value" Data..=) Prelude.<$> value
            ]
        )

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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfigurationS3KeyFilterRuleName

-- | Details for a filter rule.
--
-- /See:/ 'newAwsS3BucketNotificationConfigurationS3KeyFilterRule' smart constructor.
data AwsS3BucketNotificationConfigurationS3KeyFilterRule = AwsS3BucketNotificationConfigurationS3KeyFilterRule'
  { -- | Indicates whether the filter is based on the prefix or suffix of the
    -- Amazon S3 key.
    name :: Prelude.Maybe AwsS3BucketNotificationConfigurationS3KeyFilterRuleName,
    -- | The filter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketNotificationConfigurationS3KeyFilterRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsS3BucketNotificationConfigurationS3KeyFilterRule_name' - Indicates whether the filter is based on the prefix or suffix of the
-- Amazon S3 key.
--
-- 'value', 'awsS3BucketNotificationConfigurationS3KeyFilterRule_value' - The filter value.
newAwsS3BucketNotificationConfigurationS3KeyFilterRule ::
  AwsS3BucketNotificationConfigurationS3KeyFilterRule
newAwsS3BucketNotificationConfigurationS3KeyFilterRule =
  AwsS3BucketNotificationConfigurationS3KeyFilterRule'
    { name =
        Prelude.Nothing,
      value =
        Prelude.Nothing
    }

-- | Indicates whether the filter is based on the prefix or suffix of the
-- Amazon S3 key.
awsS3BucketNotificationConfigurationS3KeyFilterRule_name :: Lens.Lens' AwsS3BucketNotificationConfigurationS3KeyFilterRule (Prelude.Maybe AwsS3BucketNotificationConfigurationS3KeyFilterRuleName)
awsS3BucketNotificationConfigurationS3KeyFilterRule_name = Lens.lens (\AwsS3BucketNotificationConfigurationS3KeyFilterRule' {name} -> name) (\s@AwsS3BucketNotificationConfigurationS3KeyFilterRule' {} a -> s {name = a} :: AwsS3BucketNotificationConfigurationS3KeyFilterRule)

-- | The filter value.
awsS3BucketNotificationConfigurationS3KeyFilterRule_value :: Lens.Lens' AwsS3BucketNotificationConfigurationS3KeyFilterRule (Prelude.Maybe Prelude.Text)
awsS3BucketNotificationConfigurationS3KeyFilterRule_value = Lens.lens (\AwsS3BucketNotificationConfigurationS3KeyFilterRule' {value} -> value) (\s@AwsS3BucketNotificationConfigurationS3KeyFilterRule' {} a -> s {value = a} :: AwsS3BucketNotificationConfigurationS3KeyFilterRule)

instance
  Core.FromJSON
    AwsS3BucketNotificationConfigurationS3KeyFilterRule
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketNotificationConfigurationS3KeyFilterRule"
      ( \x ->
          AwsS3BucketNotificationConfigurationS3KeyFilterRule'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    AwsS3BucketNotificationConfigurationS3KeyFilterRule
  where
  hashWithSalt
    _salt
    AwsS3BucketNotificationConfigurationS3KeyFilterRule' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsS3BucketNotificationConfigurationS3KeyFilterRule
  where
  rnf
    AwsS3BucketNotificationConfigurationS3KeyFilterRule' {..} =
      Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance
  Core.ToJSON
    AwsS3BucketNotificationConfigurationS3KeyFilterRule
  where
  toJSON
    AwsS3BucketNotificationConfigurationS3KeyFilterRule' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Name" Core..=) Prelude.<$> name,
              ("Value" Core..=) Prelude.<$> value
            ]
        )

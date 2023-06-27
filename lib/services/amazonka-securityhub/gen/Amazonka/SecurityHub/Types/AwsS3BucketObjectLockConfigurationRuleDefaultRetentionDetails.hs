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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default S3 Object Lock retention mode and period that you want to
-- apply to new objects placed in the specified Amazon S3 bucket.
--
-- /See:/ 'newAwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' smart constructor.
data AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails = AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails'
  { -- | The number of days that you want to specify for the default retention
    -- period.
    days :: Prelude.Maybe Prelude.Int,
    -- | The default Object Lock retention mode you want to apply to new objects
    -- placed in the specified bucket.
    mode :: Prelude.Maybe Prelude.Text,
    -- | The number of years that you want to specify for the default retention
    -- period.
    years :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'days', 'awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_days' - The number of days that you want to specify for the default retention
-- period.
--
-- 'mode', 'awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_mode' - The default Object Lock retention mode you want to apply to new objects
-- placed in the specified bucket.
--
-- 'years', 'awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_years' - The number of years that you want to specify for the default retention
-- period.
newAwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails ::
  AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
newAwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails =
  AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails'
    { days =
        Prelude.Nothing,
      mode =
        Prelude.Nothing,
      years =
        Prelude.Nothing
    }

-- | The number of days that you want to specify for the default retention
-- period.
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_days :: Lens.Lens' AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails (Prelude.Maybe Prelude.Int)
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_days = Lens.lens (\AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {days} -> days) (\s@AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {} a -> s {days = a} :: AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails)

-- | The default Object Lock retention mode you want to apply to new objects
-- placed in the specified bucket.
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_mode :: Lens.Lens' AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails (Prelude.Maybe Prelude.Text)
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_mode = Lens.lens (\AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {mode} -> mode) (\s@AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {} a -> s {mode = a} :: AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails)

-- | The number of years that you want to specify for the default retention
-- period.
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_years :: Lens.Lens' AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails (Prelude.Maybe Prelude.Int)
awsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails_years = Lens.lens (\AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {years} -> years) (\s@AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {} a -> s {years = a} :: AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails)

instance
  Data.FromJSON
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails"
      ( \x ->
          AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails'
            Prelude.<$> (x Data..:? "Days")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "Years")
      )

instance
  Prelude.Hashable
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {..} =
      _salt
        `Prelude.hashWithSalt` days
        `Prelude.hashWithSalt` mode
        `Prelude.hashWithSalt` years

instance
  Prelude.NFData
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
  where
  rnf
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {..} =
      Prelude.rnf days
        `Prelude.seq` Prelude.rnf mode
        `Prelude.seq` Prelude.rnf years

instance
  Data.ToJSON
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails
  where
  toJSON
    AwsS3BucketObjectLockConfigurationRuleDefaultRetentionDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Days" Data..=) Prelude.<$> days,
              ("Mode" Data..=) Prelude.<$> mode,
              ("Years" Data..=) Prelude.<$> years
            ]
        )

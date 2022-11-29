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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A rule for when objects transition to specific storage classes.
--
-- /See:/ 'newAwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' smart constructor.
data AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails = AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails'
  { -- | A date on which to transition objects to the specified storage class. If
    -- you provide @Date@, you cannot provide @Days@.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    date :: Prelude.Maybe Prelude.Text,
    -- | The number of days after which to transition the object to the specified
    -- storage class. If you provide @Days@, you cannot provide @Date@.
    days :: Prelude.Maybe Prelude.Int,
    -- | The storage class to transition the object to. Valid values are as
    -- follows:
    --
    -- -   @DEEP_ARCHIVE@
    --
    -- -   @GLACIER@
    --
    -- -   @INTELLIGENT_TIERING@
    --
    -- -   @ONEZONE_IA@
    --
    -- -   @STANDARD_IA@
    storageClass :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'date', 'awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_date' - A date on which to transition objects to the specified storage class. If
-- you provide @Date@, you cannot provide @Days@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'days', 'awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_days' - The number of days after which to transition the object to the specified
-- storage class. If you provide @Days@, you cannot provide @Date@.
--
-- 'storageClass', 'awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_storageClass' - The storage class to transition the object to. Valid values are as
-- follows:
--
-- -   @DEEP_ARCHIVE@
--
-- -   @GLACIER@
--
-- -   @INTELLIGENT_TIERING@
--
-- -   @ONEZONE_IA@
--
-- -   @STANDARD_IA@
newAwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails ::
  AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
newAwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails =
  AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails'
    { date =
        Prelude.Nothing,
      days =
        Prelude.Nothing,
      storageClass =
        Prelude.Nothing
    }

-- | A date on which to transition objects to the specified storage class. If
-- you provide @Date@, you cannot provide @Days@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_date :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_date = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {date} -> date) (\s@AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {} a -> s {date = a} :: AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails)

-- | The number of days after which to transition the object to the specified
-- storage class. If you provide @Days@, you cannot provide @Date@.
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_days :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails (Prelude.Maybe Prelude.Int)
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_days = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {days} -> days) (\s@AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {} a -> s {days = a} :: AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails)

-- | The storage class to transition the object to. Valid values are as
-- follows:
--
-- -   @DEEP_ARCHIVE@
--
-- -   @GLACIER@
--
-- -   @INTELLIGENT_TIERING@
--
-- -   @ONEZONE_IA@
--
-- -   @STANDARD_IA@
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_storageClass :: Lens.Lens' AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails (Prelude.Maybe Prelude.Text)
awsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails_storageClass = Lens.lens (\AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {storageClass} -> storageClass) (\s@AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {} a -> s {storageClass = a} :: AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails)

instance
  Core.FromJSON
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails"
      ( \x ->
          AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails'
            Prelude.<$> (x Core..:? "Date") Prelude.<*> (x Core..:? "Days")
              Prelude.<*> (x Core..:? "StorageClass")
      )

instance
  Prelude.Hashable
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
  where
  hashWithSalt
    _salt
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {..} =
      _salt `Prelude.hashWithSalt` date
        `Prelude.hashWithSalt` days
        `Prelude.hashWithSalt` storageClass

instance
  Prelude.NFData
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
  where
  rnf
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {..} =
      Prelude.rnf date
        `Prelude.seq` Prelude.rnf days
        `Prelude.seq` Prelude.rnf storageClass

instance
  Core.ToJSON
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails
  where
  toJSON
    AwsS3BucketBucketLifecycleConfigurationRulesTransitionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Date" Core..=) Prelude.<$> date,
              ("Days" Core..=) Prelude.<$> days,
              ("StorageClass" Core..=) Prelude.<$> storageClass
            ]
        )

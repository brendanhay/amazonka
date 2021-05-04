{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.Types.UsageReportSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportSubscription where

import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.UsageReportSchedule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information about the usage report subscription.
--
-- /See:/ 'newUsageReportSubscription' smart constructor.
data UsageReportSubscription = UsageReportSubscription'
  { -- | The errors that were returned if usage reports couldn\'t be generated.
    subscriptionErrors :: Prelude.Maybe [LastReportGenerationExecutionError],
    -- | The time when the last usage report was generated.
    lastGeneratedReportDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon S3 bucket where generated reports are stored.
    --
    -- If you enabled on-instance session scripts and Amazon S3 logging for
    -- your session script configuration, AppStream 2.0 created an S3 bucket to
    -- store the script output. The bucket is unique to your account and
    -- Region. When you enable usage reporting in this case, AppStream 2.0 uses
    -- the same bucket to store your usage reports. If you haven\'t already
    -- enabled on-instance session scripts, when you enable usage reports,
    -- AppStream 2.0 creates a new S3 bucket.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The schedule for generating usage reports.
    schedule :: Prelude.Maybe UsageReportSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UsageReportSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionErrors', 'usageReportSubscription_subscriptionErrors' - The errors that were returned if usage reports couldn\'t be generated.
--
-- 'lastGeneratedReportDate', 'usageReportSubscription_lastGeneratedReportDate' - The time when the last usage report was generated.
--
-- 's3BucketName', 'usageReportSubscription_s3BucketName' - The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for
-- your session script configuration, AppStream 2.0 created an S3 bucket to
-- store the script output. The bucket is unique to your account and
-- Region. When you enable usage reporting in this case, AppStream 2.0 uses
-- the same bucket to store your usage reports. If you haven\'t already
-- enabled on-instance session scripts, when you enable usage reports,
-- AppStream 2.0 creates a new S3 bucket.
--
-- 'schedule', 'usageReportSubscription_schedule' - The schedule for generating usage reports.
newUsageReportSubscription ::
  UsageReportSubscription
newUsageReportSubscription =
  UsageReportSubscription'
    { subscriptionErrors =
        Prelude.Nothing,
      lastGeneratedReportDate = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The errors that were returned if usage reports couldn\'t be generated.
usageReportSubscription_subscriptionErrors :: Lens.Lens' UsageReportSubscription (Prelude.Maybe [LastReportGenerationExecutionError])
usageReportSubscription_subscriptionErrors = Lens.lens (\UsageReportSubscription' {subscriptionErrors} -> subscriptionErrors) (\s@UsageReportSubscription' {} a -> s {subscriptionErrors = a} :: UsageReportSubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | The time when the last usage report was generated.
usageReportSubscription_lastGeneratedReportDate :: Lens.Lens' UsageReportSubscription (Prelude.Maybe Prelude.UTCTime)
usageReportSubscription_lastGeneratedReportDate = Lens.lens (\UsageReportSubscription' {lastGeneratedReportDate} -> lastGeneratedReportDate) (\s@UsageReportSubscription' {} a -> s {lastGeneratedReportDate = a} :: UsageReportSubscription) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for
-- your session script configuration, AppStream 2.0 created an S3 bucket to
-- store the script output. The bucket is unique to your account and
-- Region. When you enable usage reporting in this case, AppStream 2.0 uses
-- the same bucket to store your usage reports. If you haven\'t already
-- enabled on-instance session scripts, when you enable usage reports,
-- AppStream 2.0 creates a new S3 bucket.
usageReportSubscription_s3BucketName :: Lens.Lens' UsageReportSubscription (Prelude.Maybe Prelude.Text)
usageReportSubscription_s3BucketName = Lens.lens (\UsageReportSubscription' {s3BucketName} -> s3BucketName) (\s@UsageReportSubscription' {} a -> s {s3BucketName = a} :: UsageReportSubscription)

-- | The schedule for generating usage reports.
usageReportSubscription_schedule :: Lens.Lens' UsageReportSubscription (Prelude.Maybe UsageReportSchedule)
usageReportSubscription_schedule = Lens.lens (\UsageReportSubscription' {schedule} -> schedule) (\s@UsageReportSubscription' {} a -> s {schedule = a} :: UsageReportSubscription)

instance Prelude.FromJSON UsageReportSubscription where
  parseJSON =
    Prelude.withObject
      "UsageReportSubscription"
      ( \x ->
          UsageReportSubscription'
            Prelude.<$> ( x Prelude..:? "SubscriptionErrors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LastGeneratedReportDate")
            Prelude.<*> (x Prelude..:? "S3BucketName")
            Prelude.<*> (x Prelude..:? "Schedule")
      )

instance Prelude.Hashable UsageReportSubscription

instance Prelude.NFData UsageReportSubscription

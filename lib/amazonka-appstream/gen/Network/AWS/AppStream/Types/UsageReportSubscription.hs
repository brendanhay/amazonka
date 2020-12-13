{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportSubscription
  ( UsageReportSubscription (..),

    -- * Smart constructor
    mkUsageReportSubscription,

    -- * Lenses
    ursLastGeneratedReportDate,
    ursSchedule,
    ursSubscriptionErrors,
    ursS3BucketName,
  )
where

import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.UsageReportSchedule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes information about the usage report subscription.
--
-- /See:/ 'mkUsageReportSubscription' smart constructor.
data UsageReportSubscription = UsageReportSubscription'
  { -- | The time when the last usage report was generated.
    lastGeneratedReportDate :: Lude.Maybe Lude.Timestamp,
    -- | The schedule for generating usage reports.
    schedule :: Lude.Maybe UsageReportSchedule,
    -- | The errors that were returned if usage reports couldn't be generated.
    subscriptionErrors :: Lude.Maybe [LastReportGenerationExecutionError],
    -- | The Amazon S3 bucket where generated reports are stored.
    --
    -- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
    s3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageReportSubscription' with the minimum fields required to make a request.
--
-- * 'lastGeneratedReportDate' - The time when the last usage report was generated.
-- * 'schedule' - The schedule for generating usage reports.
-- * 'subscriptionErrors' - The errors that were returned if usage reports couldn't be generated.
-- * 's3BucketName' - The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
mkUsageReportSubscription ::
  UsageReportSubscription
mkUsageReportSubscription =
  UsageReportSubscription'
    { lastGeneratedReportDate = Lude.Nothing,
      schedule = Lude.Nothing,
      subscriptionErrors = Lude.Nothing,
      s3BucketName = Lude.Nothing
    }

-- | The time when the last usage report was generated.
--
-- /Note:/ Consider using 'lastGeneratedReportDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursLastGeneratedReportDate :: Lens.Lens' UsageReportSubscription (Lude.Maybe Lude.Timestamp)
ursLastGeneratedReportDate = Lens.lens (lastGeneratedReportDate :: UsageReportSubscription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastGeneratedReportDate = a} :: UsageReportSubscription)
{-# DEPRECATED ursLastGeneratedReportDate "Use generic-lens or generic-optics with 'lastGeneratedReportDate' instead." #-}

-- | The schedule for generating usage reports.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursSchedule :: Lens.Lens' UsageReportSubscription (Lude.Maybe UsageReportSchedule)
ursSchedule = Lens.lens (schedule :: UsageReportSubscription -> Lude.Maybe UsageReportSchedule) (\s a -> s {schedule = a} :: UsageReportSubscription)
{-# DEPRECATED ursSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The errors that were returned if usage reports couldn't be generated.
--
-- /Note:/ Consider using 'subscriptionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursSubscriptionErrors :: Lens.Lens' UsageReportSubscription (Lude.Maybe [LastReportGenerationExecutionError])
ursSubscriptionErrors = Lens.lens (subscriptionErrors :: UsageReportSubscription -> Lude.Maybe [LastReportGenerationExecutionError]) (\s a -> s {subscriptionErrors = a} :: UsageReportSubscription)
{-# DEPRECATED ursSubscriptionErrors "Use generic-lens or generic-optics with 'subscriptionErrors' instead." #-}

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursS3BucketName :: Lens.Lens' UsageReportSubscription (Lude.Maybe Lude.Text)
ursS3BucketName = Lens.lens (s3BucketName :: UsageReportSubscription -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: UsageReportSubscription)
{-# DEPRECATED ursS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON UsageReportSubscription where
  parseJSON =
    Lude.withObject
      "UsageReportSubscription"
      ( \x ->
          UsageReportSubscription'
            Lude.<$> (x Lude..:? "LastGeneratedReportDate")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "SubscriptionErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "S3BucketName")
      )

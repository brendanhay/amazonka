{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.UsageReportSubscription
  ( UsageReportSubscription (..)
  -- * Smart constructor
  , mkUsageReportSubscription
  -- * Lenses
  , ursLastGeneratedReportDate
  , ursS3BucketName
  , ursSchedule
  , ursSubscriptionErrors
  ) where

import qualified Network.AWS.AppStream.Types.LastReportGenerationExecutionError as Types
import qualified Network.AWS.AppStream.Types.UsageReportSchedule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information about the usage report subscription.
--
-- /See:/ 'mkUsageReportSubscription' smart constructor.
data UsageReportSubscription = UsageReportSubscription'
  { lastGeneratedReportDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the last usage report was generated.
  , s3BucketName :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
  , schedule :: Core.Maybe Types.UsageReportSchedule
    -- ^ The schedule for generating usage reports.
  , subscriptionErrors :: Core.Maybe [Types.LastReportGenerationExecutionError]
    -- ^ The errors that were returned if usage reports couldn't be generated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UsageReportSubscription' value with any optional fields omitted.
mkUsageReportSubscription
    :: UsageReportSubscription
mkUsageReportSubscription
  = UsageReportSubscription'{lastGeneratedReportDate = Core.Nothing,
                             s3BucketName = Core.Nothing, schedule = Core.Nothing,
                             subscriptionErrors = Core.Nothing}

-- | The time when the last usage report was generated.
--
-- /Note:/ Consider using 'lastGeneratedReportDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursLastGeneratedReportDate :: Lens.Lens' UsageReportSubscription (Core.Maybe Core.NominalDiffTime)
ursLastGeneratedReportDate = Lens.field @"lastGeneratedReportDate"
{-# INLINEABLE ursLastGeneratedReportDate #-}
{-# DEPRECATED lastGeneratedReportDate "Use generic-lens or generic-optics with 'lastGeneratedReportDate' instead"  #-}

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursS3BucketName :: Lens.Lens' UsageReportSubscription (Core.Maybe Core.Text)
ursS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE ursS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The schedule for generating usage reports.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursSchedule :: Lens.Lens' UsageReportSubscription (Core.Maybe Types.UsageReportSchedule)
ursSchedule = Lens.field @"schedule"
{-# INLINEABLE ursSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The errors that were returned if usage reports couldn't be generated.
--
-- /Note:/ Consider using 'subscriptionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursSubscriptionErrors :: Lens.Lens' UsageReportSubscription (Core.Maybe [Types.LastReportGenerationExecutionError])
ursSubscriptionErrors = Lens.field @"subscriptionErrors"
{-# INLINEABLE ursSubscriptionErrors #-}
{-# DEPRECATED subscriptionErrors "Use generic-lens or generic-optics with 'subscriptionErrors' instead"  #-}

instance Core.FromJSON UsageReportSubscription where
        parseJSON
          = Core.withObject "UsageReportSubscription" Core.$
              \ x ->
                UsageReportSubscription' Core.<$>
                  (x Core..:? "LastGeneratedReportDate") Core.<*>
                    x Core..:? "S3BucketName"
                    Core.<*> x Core..:? "Schedule"
                    Core.<*> x Core..:? "SubscriptionErrors"

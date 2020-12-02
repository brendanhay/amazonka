{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportSubscription where

import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.UsageReportSchedule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information about the usage report subscription.
--
--
--
-- /See:/ 'usageReportSubscription' smart constructor.
data UsageReportSubscription = UsageReportSubscription'
  { _ursLastGeneratedReportDate ::
      !(Maybe POSIX),
    _ursSchedule ::
      !(Maybe UsageReportSchedule),
    _ursSubscriptionErrors ::
      !( Maybe
           [LastReportGenerationExecutionError]
       ),
    _ursS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageReportSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursLastGeneratedReportDate' - The time when the last usage report was generated.
--
-- * 'ursSchedule' - The schedule for generating usage reports.
--
-- * 'ursSubscriptionErrors' - The errors that were returned if usage reports couldn't be generated.
--
-- * 'ursS3BucketName' - The Amazon S3 bucket where generated reports are stored. If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
usageReportSubscription ::
  UsageReportSubscription
usageReportSubscription =
  UsageReportSubscription'
    { _ursLastGeneratedReportDate = Nothing,
      _ursSchedule = Nothing,
      _ursSubscriptionErrors = Nothing,
      _ursS3BucketName = Nothing
    }

-- | The time when the last usage report was generated.
ursLastGeneratedReportDate :: Lens' UsageReportSubscription (Maybe UTCTime)
ursLastGeneratedReportDate = lens _ursLastGeneratedReportDate (\s a -> s {_ursLastGeneratedReportDate = a}) . mapping _Time

-- | The schedule for generating usage reports.
ursSchedule :: Lens' UsageReportSubscription (Maybe UsageReportSchedule)
ursSchedule = lens _ursSchedule (\s a -> s {_ursSchedule = a})

-- | The errors that were returned if usage reports couldn't be generated.
ursSubscriptionErrors :: Lens' UsageReportSubscription [LastReportGenerationExecutionError]
ursSubscriptionErrors = lens _ursSubscriptionErrors (\s a -> s {_ursSubscriptionErrors = a}) . _Default . _Coerce

-- | The Amazon S3 bucket where generated reports are stored. If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
ursS3BucketName :: Lens' UsageReportSubscription (Maybe Text)
ursS3BucketName = lens _ursS3BucketName (\s a -> s {_ursS3BucketName = a})

instance FromJSON UsageReportSubscription where
  parseJSON =
    withObject
      "UsageReportSubscription"
      ( \x ->
          UsageReportSubscription'
            <$> (x .:? "LastGeneratedReportDate")
            <*> (x .:? "Schedule")
            <*> (x .:? "SubscriptionErrors" .!= mempty)
            <*> (x .:? "S3BucketName")
      )

instance Hashable UsageReportSubscription

instance NFData UsageReportSubscription

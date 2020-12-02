{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportSchedule where

import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportFormat
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The schedule of the usage report.
--
--
--
-- /See:/ 'businessReportSchedule' smart constructor.
data BusinessReportSchedule = BusinessReportSchedule'
  { _brsS3KeyPrefix ::
      !(Maybe Text),
    _brsLastBusinessReport ::
      !(Maybe BusinessReport),
    _brsFormat :: !(Maybe BusinessReportFormat),
    _brsRecurrence ::
      !(Maybe BusinessReportRecurrence),
    _brsScheduleName :: !(Maybe Text),
    _brsScheduleARN :: !(Maybe Text),
    _brsContentRange ::
      !(Maybe BusinessReportContentRange),
    _brsS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BusinessReportSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsS3KeyPrefix' - The S3 key where the report is delivered.
--
-- * 'brsLastBusinessReport' - The details of the last business report delivery for a specified time interval.
--
-- * 'brsFormat' - The format of the generated report (individual CSV files or zipped files of individual files).
--
-- * 'brsRecurrence' - The recurrence of the reports.
--
-- * 'brsScheduleName' - The name identifier of the schedule.
--
-- * 'brsScheduleARN' - The ARN of the business report schedule.
--
-- * 'brsContentRange' - The content range of the reports.
--
-- * 'brsS3BucketName' - The S3 bucket name of the output reports.
businessReportSchedule ::
  BusinessReportSchedule
businessReportSchedule =
  BusinessReportSchedule'
    { _brsS3KeyPrefix = Nothing,
      _brsLastBusinessReport = Nothing,
      _brsFormat = Nothing,
      _brsRecurrence = Nothing,
      _brsScheduleName = Nothing,
      _brsScheduleARN = Nothing,
      _brsContentRange = Nothing,
      _brsS3BucketName = Nothing
    }

-- | The S3 key where the report is delivered.
brsS3KeyPrefix :: Lens' BusinessReportSchedule (Maybe Text)
brsS3KeyPrefix = lens _brsS3KeyPrefix (\s a -> s {_brsS3KeyPrefix = a})

-- | The details of the last business report delivery for a specified time interval.
brsLastBusinessReport :: Lens' BusinessReportSchedule (Maybe BusinessReport)
brsLastBusinessReport = lens _brsLastBusinessReport (\s a -> s {_brsLastBusinessReport = a})

-- | The format of the generated report (individual CSV files or zipped files of individual files).
brsFormat :: Lens' BusinessReportSchedule (Maybe BusinessReportFormat)
brsFormat = lens _brsFormat (\s a -> s {_brsFormat = a})

-- | The recurrence of the reports.
brsRecurrence :: Lens' BusinessReportSchedule (Maybe BusinessReportRecurrence)
brsRecurrence = lens _brsRecurrence (\s a -> s {_brsRecurrence = a})

-- | The name identifier of the schedule.
brsScheduleName :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleName = lens _brsScheduleName (\s a -> s {_brsScheduleName = a})

-- | The ARN of the business report schedule.
brsScheduleARN :: Lens' BusinessReportSchedule (Maybe Text)
brsScheduleARN = lens _brsScheduleARN (\s a -> s {_brsScheduleARN = a})

-- | The content range of the reports.
brsContentRange :: Lens' BusinessReportSchedule (Maybe BusinessReportContentRange)
brsContentRange = lens _brsContentRange (\s a -> s {_brsContentRange = a})

-- | The S3 bucket name of the output reports.
brsS3BucketName :: Lens' BusinessReportSchedule (Maybe Text)
brsS3BucketName = lens _brsS3BucketName (\s a -> s {_brsS3BucketName = a})

instance FromJSON BusinessReportSchedule where
  parseJSON =
    withObject
      "BusinessReportSchedule"
      ( \x ->
          BusinessReportSchedule'
            <$> (x .:? "S3KeyPrefix")
            <*> (x .:? "LastBusinessReport")
            <*> (x .:? "Format")
            <*> (x .:? "Recurrence")
            <*> (x .:? "ScheduleName")
            <*> (x .:? "ScheduleArn")
            <*> (x .:? "ContentRange")
            <*> (x .:? "S3BucketName")
      )

instance Hashable BusinessReportSchedule

instance NFData BusinessReportSchedule

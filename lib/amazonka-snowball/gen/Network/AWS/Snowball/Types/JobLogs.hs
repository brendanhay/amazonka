{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobLogs where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains job logs. Whenever a Snow device is used to import data into or export data out of Amazon S3, you'll have the option of downloading a PDF job report. Job logs are returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type. The job logs can be accessed for up to 60 minutes after this request has been made. To access any of the job logs after 60 minutes have passed, you'll have to make another call to the @DescribeJob@ action.
--
--
-- For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
--
-- The job report provides you insight into the state of your Amazon S3 data transfer. The report includes details about your job or job part for your records.
--
-- For deeper visibility into the status of your transferred objects, you can look at the two associated logs: a success log and a failure log. The logs are saved in comma-separated value (CSV) format, and the name of each log includes the ID of the job or job part that the log describes.
--
--
-- /See:/ 'jobLogs' smart constructor.
data JobLogs = JobLogs'
  { _jlJobFailureLogURI :: !(Maybe Text),
    _jlJobCompletionReportURI :: !(Maybe Text),
    _jlJobSuccessLogURI :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jlJobFailureLogURI' - A link to an Amazon S3 presigned URL where the job failure log is located.
--
-- * 'jlJobCompletionReportURI' - A link to an Amazon S3 presigned URL where the job completion report is located.
--
-- * 'jlJobSuccessLogURI' - A link to an Amazon S3 presigned URL where the job success log is located.
jobLogs ::
  JobLogs
jobLogs =
  JobLogs'
    { _jlJobFailureLogURI = Nothing,
      _jlJobCompletionReportURI = Nothing,
      _jlJobSuccessLogURI = Nothing
    }

-- | A link to an Amazon S3 presigned URL where the job failure log is located.
jlJobFailureLogURI :: Lens' JobLogs (Maybe Text)
jlJobFailureLogURI = lens _jlJobFailureLogURI (\s a -> s {_jlJobFailureLogURI = a})

-- | A link to an Amazon S3 presigned URL where the job completion report is located.
jlJobCompletionReportURI :: Lens' JobLogs (Maybe Text)
jlJobCompletionReportURI = lens _jlJobCompletionReportURI (\s a -> s {_jlJobCompletionReportURI = a})

-- | A link to an Amazon S3 presigned URL where the job success log is located.
jlJobSuccessLogURI :: Lens' JobLogs (Maybe Text)
jlJobSuccessLogURI = lens _jlJobSuccessLogURI (\s a -> s {_jlJobSuccessLogURI = a})

instance FromJSON JobLogs where
  parseJSON =
    withObject
      "JobLogs"
      ( \x ->
          JobLogs'
            <$> (x .:? "JobFailureLogURI")
            <*> (x .:? "JobCompletionReportURI")
            <*> (x .:? "JobSuccessLogURI")
      )

instance Hashable JobLogs

instance NFData JobLogs

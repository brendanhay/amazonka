{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the report delivery schedule with the specified schedule ARN.
module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
  ( -- * Creating a Request
    updateBusinessReportSchedule,
    UpdateBusinessReportSchedule,

    -- * Request Lenses
    ubrsS3KeyPrefix,
    ubrsFormat,
    ubrsRecurrence,
    ubrsScheduleName,
    ubrsS3BucketName,
    ubrsScheduleARN,

    -- * Destructuring the Response
    updateBusinessReportScheduleResponse,
    UpdateBusinessReportScheduleResponse,

    -- * Response Lenses
    ubrsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateBusinessReportSchedule' smart constructor.
data UpdateBusinessReportSchedule = UpdateBusinessReportSchedule'
  { _ubrsS3KeyPrefix ::
      !(Maybe Text),
    _ubrsFormat ::
      !(Maybe BusinessReportFormat),
    _ubrsRecurrence ::
      !(Maybe BusinessReportRecurrence),
    _ubrsScheduleName ::
      !(Maybe Text),
    _ubrsS3BucketName ::
      !(Maybe Text),
    _ubrsScheduleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBusinessReportSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsS3KeyPrefix' - The S3 key where the report is delivered.
--
-- * 'ubrsFormat' - The format of the generated report (individual CSV files or zipped files of individual files).
--
-- * 'ubrsRecurrence' - The recurrence of the reports.
--
-- * 'ubrsScheduleName' - The name identifier of the schedule.
--
-- * 'ubrsS3BucketName' - The S3 location of the output reports.
--
-- * 'ubrsScheduleARN' - The ARN of the business report schedule.
updateBusinessReportSchedule ::
  -- | 'ubrsScheduleARN'
  Text ->
  UpdateBusinessReportSchedule
updateBusinessReportSchedule pScheduleARN_ =
  UpdateBusinessReportSchedule'
    { _ubrsS3KeyPrefix = Nothing,
      _ubrsFormat = Nothing,
      _ubrsRecurrence = Nothing,
      _ubrsScheduleName = Nothing,
      _ubrsS3BucketName = Nothing,
      _ubrsScheduleARN = pScheduleARN_
    }

-- | The S3 key where the report is delivered.
ubrsS3KeyPrefix :: Lens' UpdateBusinessReportSchedule (Maybe Text)
ubrsS3KeyPrefix = lens _ubrsS3KeyPrefix (\s a -> s {_ubrsS3KeyPrefix = a})

-- | The format of the generated report (individual CSV files or zipped files of individual files).
ubrsFormat :: Lens' UpdateBusinessReportSchedule (Maybe BusinessReportFormat)
ubrsFormat = lens _ubrsFormat (\s a -> s {_ubrsFormat = a})

-- | The recurrence of the reports.
ubrsRecurrence :: Lens' UpdateBusinessReportSchedule (Maybe BusinessReportRecurrence)
ubrsRecurrence = lens _ubrsRecurrence (\s a -> s {_ubrsRecurrence = a})

-- | The name identifier of the schedule.
ubrsScheduleName :: Lens' UpdateBusinessReportSchedule (Maybe Text)
ubrsScheduleName = lens _ubrsScheduleName (\s a -> s {_ubrsScheduleName = a})

-- | The S3 location of the output reports.
ubrsS3BucketName :: Lens' UpdateBusinessReportSchedule (Maybe Text)
ubrsS3BucketName = lens _ubrsS3BucketName (\s a -> s {_ubrsS3BucketName = a})

-- | The ARN of the business report schedule.
ubrsScheduleARN :: Lens' UpdateBusinessReportSchedule Text
ubrsScheduleARN = lens _ubrsScheduleARN (\s a -> s {_ubrsScheduleARN = a})

instance AWSRequest UpdateBusinessReportSchedule where
  type
    Rs UpdateBusinessReportSchedule =
      UpdateBusinessReportScheduleResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      ( \s h x ->
          UpdateBusinessReportScheduleResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateBusinessReportSchedule

instance NFData UpdateBusinessReportSchedule

instance ToHeaders UpdateBusinessReportSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.UpdateBusinessReportSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateBusinessReportSchedule where
  toJSON UpdateBusinessReportSchedule' {..} =
    object
      ( catMaybes
          [ ("S3KeyPrefix" .=) <$> _ubrsS3KeyPrefix,
            ("Format" .=) <$> _ubrsFormat,
            ("Recurrence" .=) <$> _ubrsRecurrence,
            ("ScheduleName" .=) <$> _ubrsScheduleName,
            ("S3BucketName" .=) <$> _ubrsS3BucketName,
            Just ("ScheduleArn" .= _ubrsScheduleARN)
          ]
      )

instance ToPath UpdateBusinessReportSchedule where
  toPath = const "/"

instance ToQuery UpdateBusinessReportSchedule where
  toQuery = const mempty

-- | /See:/ 'updateBusinessReportScheduleResponse' smart constructor.
newtype UpdateBusinessReportScheduleResponse = UpdateBusinessReportScheduleResponse'
  { _ubrsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateBusinessReportScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsrsResponseStatus' - -- | The response status code.
updateBusinessReportScheduleResponse ::
  -- | 'ubrsrsResponseStatus'
  Int ->
  UpdateBusinessReportScheduleResponse
updateBusinessReportScheduleResponse pResponseStatus_ =
  UpdateBusinessReportScheduleResponse'
    { _ubrsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ubrsrsResponseStatus :: Lens' UpdateBusinessReportScheduleResponse Int
ubrsrsResponseStatus = lens _ubrsrsResponseStatus (\s a -> s {_ubrsrsResponseStatus = a})

instance NFData UpdateBusinessReportScheduleResponse

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recurring schedule for usage reports to deliver to the specified S3 location with a specified daily or weekly interval.
--
--
module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
    (
    -- * Creating a Request
      createBusinessReportSchedule
    , CreateBusinessReportSchedule
    -- * Request Lenses
    , cbrsS3KeyPrefix
    , cbrsRecurrence
    , cbrsScheduleName
    , cbrsClientRequestToken
    , cbrsS3BucketName
    , cbrsFormat
    , cbrsContentRange

    -- * Destructuring the Response
    , createBusinessReportScheduleResponse
    , CreateBusinessReportScheduleResponse
    -- * Response Lenses
    , cbrsrsScheduleARN
    , cbrsrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBusinessReportSchedule' smart constructor.
data CreateBusinessReportSchedule = CreateBusinessReportSchedule'
  { _cbrsS3KeyPrefix        :: !(Maybe Text)
  , _cbrsRecurrence         :: !(Maybe BusinessReportRecurrence)
  , _cbrsScheduleName       :: !(Maybe Text)
  , _cbrsClientRequestToken :: !(Maybe Text)
  , _cbrsS3BucketName       :: !(Maybe Text)
  , _cbrsFormat             :: !BusinessReportFormat
  , _cbrsContentRange       :: !BusinessReportContentRange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBusinessReportSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsS3KeyPrefix' - The S3 key where the report is delivered.
--
-- * 'cbrsRecurrence' - The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called.
--
-- * 'cbrsScheduleName' - The name identifier of the schedule.
--
-- * 'cbrsClientRequestToken' - The client request token.
--
-- * 'cbrsS3BucketName' - The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule.
--
-- * 'cbrsFormat' - The format of the generated report (individual CSV files or zipped files of individual files).
--
-- * 'cbrsContentRange' - The content range of the reports.
createBusinessReportSchedule
    :: BusinessReportFormat -- ^ 'cbrsFormat'
    -> BusinessReportContentRange -- ^ 'cbrsContentRange'
    -> CreateBusinessReportSchedule
createBusinessReportSchedule pFormat_ pContentRange_ =
  CreateBusinessReportSchedule'
    { _cbrsS3KeyPrefix = Nothing
    , _cbrsRecurrence = Nothing
    , _cbrsScheduleName = Nothing
    , _cbrsClientRequestToken = Nothing
    , _cbrsS3BucketName = Nothing
    , _cbrsFormat = pFormat_
    , _cbrsContentRange = pContentRange_
    }


-- | The S3 key where the report is delivered.
cbrsS3KeyPrefix :: Lens' CreateBusinessReportSchedule (Maybe Text)
cbrsS3KeyPrefix = lens _cbrsS3KeyPrefix (\ s a -> s{_cbrsS3KeyPrefix = a})

-- | The recurrence of the reports. If this isn't specified, the report will only be delivered one time when the API is called.
cbrsRecurrence :: Lens' CreateBusinessReportSchedule (Maybe BusinessReportRecurrence)
cbrsRecurrence = lens _cbrsRecurrence (\ s a -> s{_cbrsRecurrence = a})

-- | The name identifier of the schedule.
cbrsScheduleName :: Lens' CreateBusinessReportSchedule (Maybe Text)
cbrsScheduleName = lens _cbrsScheduleName (\ s a -> s{_cbrsScheduleName = a})

-- | The client request token.
cbrsClientRequestToken :: Lens' CreateBusinessReportSchedule (Maybe Text)
cbrsClientRequestToken = lens _cbrsClientRequestToken (\ s a -> s{_cbrsClientRequestToken = a})

-- | The S3 bucket name of the output reports. If this isn't specified, the report can be retrieved from a download link by calling ListBusinessReportSchedule.
cbrsS3BucketName :: Lens' CreateBusinessReportSchedule (Maybe Text)
cbrsS3BucketName = lens _cbrsS3BucketName (\ s a -> s{_cbrsS3BucketName = a})

-- | The format of the generated report (individual CSV files or zipped files of individual files).
cbrsFormat :: Lens' CreateBusinessReportSchedule BusinessReportFormat
cbrsFormat = lens _cbrsFormat (\ s a -> s{_cbrsFormat = a})

-- | The content range of the reports.
cbrsContentRange :: Lens' CreateBusinessReportSchedule BusinessReportContentRange
cbrsContentRange = lens _cbrsContentRange (\ s a -> s{_cbrsContentRange = a})

instance AWSRequest CreateBusinessReportSchedule
         where
        type Rs CreateBusinessReportSchedule =
             CreateBusinessReportScheduleResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateBusinessReportScheduleResponse' <$>
                   (x .?> "ScheduleArn") <*> (pure (fromEnum s)))

instance Hashable CreateBusinessReportSchedule where

instance NFData CreateBusinessReportSchedule where

instance ToHeaders CreateBusinessReportSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateBusinessReportSchedule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBusinessReportSchedule where
        toJSON CreateBusinessReportSchedule'{..}
          = object
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _cbrsS3KeyPrefix,
                  ("Recurrence" .=) <$> _cbrsRecurrence,
                  ("ScheduleName" .=) <$> _cbrsScheduleName,
                  ("ClientRequestToken" .=) <$>
                    _cbrsClientRequestToken,
                  ("S3BucketName" .=) <$> _cbrsS3BucketName,
                  Just ("Format" .= _cbrsFormat),
                  Just ("ContentRange" .= _cbrsContentRange)])

instance ToPath CreateBusinessReportSchedule where
        toPath = const "/"

instance ToQuery CreateBusinessReportSchedule where
        toQuery = const mempty

-- | /See:/ 'createBusinessReportScheduleResponse' smart constructor.
data CreateBusinessReportScheduleResponse = CreateBusinessReportScheduleResponse'
  { _cbrsrsScheduleARN    :: !(Maybe Text)
  , _cbrsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBusinessReportScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsrsScheduleARN' - The ARN of the business report schedule.
--
-- * 'cbrsrsResponseStatus' - -- | The response status code.
createBusinessReportScheduleResponse
    :: Int -- ^ 'cbrsrsResponseStatus'
    -> CreateBusinessReportScheduleResponse
createBusinessReportScheduleResponse pResponseStatus_ =
  CreateBusinessReportScheduleResponse'
    {_cbrsrsScheduleARN = Nothing, _cbrsrsResponseStatus = pResponseStatus_}


-- | The ARN of the business report schedule.
cbrsrsScheduleARN :: Lens' CreateBusinessReportScheduleResponse (Maybe Text)
cbrsrsScheduleARN = lens _cbrsrsScheduleARN (\ s a -> s{_cbrsrsScheduleARN = a})

-- | -- | The response status code.
cbrsrsResponseStatus :: Lens' CreateBusinessReportScheduleResponse Int
cbrsrsResponseStatus = lens _cbrsrsResponseStatus (\ s a -> s{_cbrsrsResponseStatus = a})

instance NFData CreateBusinessReportScheduleResponse
         where

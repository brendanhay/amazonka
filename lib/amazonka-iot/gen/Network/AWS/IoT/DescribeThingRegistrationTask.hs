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
-- Module      : Network.AWS.IoT.DescribeThingRegistrationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a bulk thing provisioning task.
--
--
module Network.AWS.IoT.DescribeThingRegistrationTask
    (
    -- * Creating a Request
      describeThingRegistrationTask
    , DescribeThingRegistrationTask
    -- * Request Lenses
    , dtrtTaskId

    -- * Destructuring the Response
    , describeThingRegistrationTaskResponse
    , DescribeThingRegistrationTaskResponse
    -- * Response Lenses
    , dtrtrsStatus
    , dtrtrsLastModifiedDate
    , dtrtrsInputFileKey
    , dtrtrsTaskId
    , dtrtrsCreationDate
    , dtrtrsPercentageProgress
    , dtrtrsTemplateBody
    , dtrtrsSuccessCount
    , dtrtrsMessage
    , dtrtrsFailureCount
    , dtrtrsInputFileBucket
    , dtrtrsRoleARN
    , dtrtrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeThingRegistrationTask' smart constructor.
newtype DescribeThingRegistrationTask = DescribeThingRegistrationTask'
  { _dtrtTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeThingRegistrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrtTaskId' - The task ID.
describeThingRegistrationTask
    :: Text -- ^ 'dtrtTaskId'
    -> DescribeThingRegistrationTask
describeThingRegistrationTask pTaskId_ =
  DescribeThingRegistrationTask' {_dtrtTaskId = pTaskId_}


-- | The task ID.
dtrtTaskId :: Lens' DescribeThingRegistrationTask Text
dtrtTaskId = lens _dtrtTaskId (\ s a -> s{_dtrtTaskId = a})

instance AWSRequest DescribeThingRegistrationTask
         where
        type Rs DescribeThingRegistrationTask =
             DescribeThingRegistrationTaskResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeThingRegistrationTaskResponse' <$>
                   (x .?> "status") <*> (x .?> "lastModifiedDate") <*>
                     (x .?> "inputFileKey")
                     <*> (x .?> "taskId")
                     <*> (x .?> "creationDate")
                     <*> (x .?> "percentageProgress")
                     <*> (x .?> "templateBody")
                     <*> (x .?> "successCount")
                     <*> (x .?> "message")
                     <*> (x .?> "failureCount")
                     <*> (x .?> "inputFileBucket")
                     <*> (x .?> "roleArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeThingRegistrationTask where

instance NFData DescribeThingRegistrationTask where

instance ToHeaders DescribeThingRegistrationTask
         where
        toHeaders = const mempty

instance ToPath DescribeThingRegistrationTask where
        toPath DescribeThingRegistrationTask'{..}
          = mconcat
              ["/thing-registration-tasks/", toBS _dtrtTaskId]

instance ToQuery DescribeThingRegistrationTask where
        toQuery = const mempty

-- | /See:/ 'describeThingRegistrationTaskResponse' smart constructor.
data DescribeThingRegistrationTaskResponse = DescribeThingRegistrationTaskResponse'
  { _dtrtrsStatus             :: !(Maybe TaskStatus)
  , _dtrtrsLastModifiedDate   :: !(Maybe POSIX)
  , _dtrtrsInputFileKey       :: !(Maybe Text)
  , _dtrtrsTaskId             :: !(Maybe Text)
  , _dtrtrsCreationDate       :: !(Maybe POSIX)
  , _dtrtrsPercentageProgress :: !(Maybe Nat)
  , _dtrtrsTemplateBody       :: !(Maybe Text)
  , _dtrtrsSuccessCount       :: !(Maybe Int)
  , _dtrtrsMessage            :: !(Maybe Text)
  , _dtrtrsFailureCount       :: !(Maybe Int)
  , _dtrtrsInputFileBucket    :: !(Maybe Text)
  , _dtrtrsRoleARN            :: !(Maybe Text)
  , _dtrtrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrtrsStatus' - The status of the bulk thing provisioning task.
--
-- * 'dtrtrsLastModifiedDate' - The date when the task was last modified.
--
-- * 'dtrtrsInputFileKey' - The input file key.
--
-- * 'dtrtrsTaskId' - The task ID.
--
-- * 'dtrtrsCreationDate' - The task creation date.
--
-- * 'dtrtrsPercentageProgress' - The progress of the bulk provisioning task expressed as a percentage.
--
-- * 'dtrtrsTemplateBody' - The task's template.
--
-- * 'dtrtrsSuccessCount' - The number of things successfully provisioned.
--
-- * 'dtrtrsMessage' - The message.
--
-- * 'dtrtrsFailureCount' - The number of things that failed to be provisioned.
--
-- * 'dtrtrsInputFileBucket' - The S3 bucket that contains the input file.
--
-- * 'dtrtrsRoleARN' - The role ARN that grants access to the input file bucket.
--
-- * 'dtrtrsResponseStatus' - -- | The response status code.
describeThingRegistrationTaskResponse
    :: Int -- ^ 'dtrtrsResponseStatus'
    -> DescribeThingRegistrationTaskResponse
describeThingRegistrationTaskResponse pResponseStatus_ =
  DescribeThingRegistrationTaskResponse'
    { _dtrtrsStatus = Nothing
    , _dtrtrsLastModifiedDate = Nothing
    , _dtrtrsInputFileKey = Nothing
    , _dtrtrsTaskId = Nothing
    , _dtrtrsCreationDate = Nothing
    , _dtrtrsPercentageProgress = Nothing
    , _dtrtrsTemplateBody = Nothing
    , _dtrtrsSuccessCount = Nothing
    , _dtrtrsMessage = Nothing
    , _dtrtrsFailureCount = Nothing
    , _dtrtrsInputFileBucket = Nothing
    , _dtrtrsRoleARN = Nothing
    , _dtrtrsResponseStatus = pResponseStatus_
    }


-- | The status of the bulk thing provisioning task.
dtrtrsStatus :: Lens' DescribeThingRegistrationTaskResponse (Maybe TaskStatus)
dtrtrsStatus = lens _dtrtrsStatus (\ s a -> s{_dtrtrsStatus = a})

-- | The date when the task was last modified.
dtrtrsLastModifiedDate :: Lens' DescribeThingRegistrationTaskResponse (Maybe UTCTime)
dtrtrsLastModifiedDate = lens _dtrtrsLastModifiedDate (\ s a -> s{_dtrtrsLastModifiedDate = a}) . mapping _Time

-- | The input file key.
dtrtrsInputFileKey :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsInputFileKey = lens _dtrtrsInputFileKey (\ s a -> s{_dtrtrsInputFileKey = a})

-- | The task ID.
dtrtrsTaskId :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsTaskId = lens _dtrtrsTaskId (\ s a -> s{_dtrtrsTaskId = a})

-- | The task creation date.
dtrtrsCreationDate :: Lens' DescribeThingRegistrationTaskResponse (Maybe UTCTime)
dtrtrsCreationDate = lens _dtrtrsCreationDate (\ s a -> s{_dtrtrsCreationDate = a}) . mapping _Time

-- | The progress of the bulk provisioning task expressed as a percentage.
dtrtrsPercentageProgress :: Lens' DescribeThingRegistrationTaskResponse (Maybe Natural)
dtrtrsPercentageProgress = lens _dtrtrsPercentageProgress (\ s a -> s{_dtrtrsPercentageProgress = a}) . mapping _Nat

-- | The task's template.
dtrtrsTemplateBody :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsTemplateBody = lens _dtrtrsTemplateBody (\ s a -> s{_dtrtrsTemplateBody = a})

-- | The number of things successfully provisioned.
dtrtrsSuccessCount :: Lens' DescribeThingRegistrationTaskResponse (Maybe Int)
dtrtrsSuccessCount = lens _dtrtrsSuccessCount (\ s a -> s{_dtrtrsSuccessCount = a})

-- | The message.
dtrtrsMessage :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsMessage = lens _dtrtrsMessage (\ s a -> s{_dtrtrsMessage = a})

-- | The number of things that failed to be provisioned.
dtrtrsFailureCount :: Lens' DescribeThingRegistrationTaskResponse (Maybe Int)
dtrtrsFailureCount = lens _dtrtrsFailureCount (\ s a -> s{_dtrtrsFailureCount = a})

-- | The S3 bucket that contains the input file.
dtrtrsInputFileBucket :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsInputFileBucket = lens _dtrtrsInputFileBucket (\ s a -> s{_dtrtrsInputFileBucket = a})

-- | The role ARN that grants access to the input file bucket.
dtrtrsRoleARN :: Lens' DescribeThingRegistrationTaskResponse (Maybe Text)
dtrtrsRoleARN = lens _dtrtrsRoleARN (\ s a -> s{_dtrtrsRoleARN = a})

-- | -- | The response status code.
dtrtrsResponseStatus :: Lens' DescribeThingRegistrationTaskResponse Int
dtrtrsResponseStatus = lens _dtrtrsResponseStatus (\ s a -> s{_dtrtrsResponseStatus = a})

instance NFData DescribeThingRegistrationTaskResponse
         where

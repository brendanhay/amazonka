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
-- Module      : Network.AWS.IoT.StartThingRegistrationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bulk thing provisioning task.
--
--
module Network.AWS.IoT.StartThingRegistrationTask
    (
    -- * Creating a Request
      startThingRegistrationTask
    , StartThingRegistrationTask
    -- * Request Lenses
    , strtTemplateBody
    , strtInputFileBucket
    , strtInputFileKey
    , strtRoleARN

    -- * Destructuring the Response
    , startThingRegistrationTaskResponse
    , StartThingRegistrationTaskResponse
    -- * Response Lenses
    , strtrsTaskId
    , strtrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startThingRegistrationTask' smart constructor.
data StartThingRegistrationTask = StartThingRegistrationTask'
  { _strtTemplateBody    :: !Text
  , _strtInputFileBucket :: !Text
  , _strtInputFileKey    :: !Text
  , _strtRoleARN         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartThingRegistrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strtTemplateBody' - The provisioning template.
--
-- * 'strtInputFileBucket' - The S3 bucket that contains the input file.
--
-- * 'strtInputFileKey' - The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
--
-- * 'strtRoleARN' - The IAM role ARN that grants permission the input file.
startThingRegistrationTask
    :: Text -- ^ 'strtTemplateBody'
    -> Text -- ^ 'strtInputFileBucket'
    -> Text -- ^ 'strtInputFileKey'
    -> Text -- ^ 'strtRoleARN'
    -> StartThingRegistrationTask
startThingRegistrationTask pTemplateBody_ pInputFileBucket_ pInputFileKey_ pRoleARN_ =
  StartThingRegistrationTask'
    { _strtTemplateBody = pTemplateBody_
    , _strtInputFileBucket = pInputFileBucket_
    , _strtInputFileKey = pInputFileKey_
    , _strtRoleARN = pRoleARN_
    }


-- | The provisioning template.
strtTemplateBody :: Lens' StartThingRegistrationTask Text
strtTemplateBody = lens _strtTemplateBody (\ s a -> s{_strtTemplateBody = a})

-- | The S3 bucket that contains the input file.
strtInputFileBucket :: Lens' StartThingRegistrationTask Text
strtInputFileBucket = lens _strtInputFileBucket (\ s a -> s{_strtInputFileBucket = a})

-- | The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
strtInputFileKey :: Lens' StartThingRegistrationTask Text
strtInputFileKey = lens _strtInputFileKey (\ s a -> s{_strtInputFileKey = a})

-- | The IAM role ARN that grants permission the input file.
strtRoleARN :: Lens' StartThingRegistrationTask Text
strtRoleARN = lens _strtRoleARN (\ s a -> s{_strtRoleARN = a})

instance AWSRequest StartThingRegistrationTask where
        type Rs StartThingRegistrationTask =
             StartThingRegistrationTaskResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 StartThingRegistrationTaskResponse' <$>
                   (x .?> "taskId") <*> (pure (fromEnum s)))

instance Hashable StartThingRegistrationTask where

instance NFData StartThingRegistrationTask where

instance ToHeaders StartThingRegistrationTask where
        toHeaders = const mempty

instance ToJSON StartThingRegistrationTask where
        toJSON StartThingRegistrationTask'{..}
          = object
              (catMaybes
                 [Just ("templateBody" .= _strtTemplateBody),
                  Just ("inputFileBucket" .= _strtInputFileBucket),
                  Just ("inputFileKey" .= _strtInputFileKey),
                  Just ("roleArn" .= _strtRoleARN)])

instance ToPath StartThingRegistrationTask where
        toPath = const "/thing-registration-tasks"

instance ToQuery StartThingRegistrationTask where
        toQuery = const mempty

-- | /See:/ 'startThingRegistrationTaskResponse' smart constructor.
data StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse'
  { _strtrsTaskId         :: !(Maybe Text)
  , _strtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strtrsTaskId' - The bulk thing provisioning task ID.
--
-- * 'strtrsResponseStatus' - -- | The response status code.
startThingRegistrationTaskResponse
    :: Int -- ^ 'strtrsResponseStatus'
    -> StartThingRegistrationTaskResponse
startThingRegistrationTaskResponse pResponseStatus_ =
  StartThingRegistrationTaskResponse'
    {_strtrsTaskId = Nothing, _strtrsResponseStatus = pResponseStatus_}


-- | The bulk thing provisioning task ID.
strtrsTaskId :: Lens' StartThingRegistrationTaskResponse (Maybe Text)
strtrsTaskId = lens _strtrsTaskId (\ s a -> s{_strtrsTaskId = a})

-- | -- | The response status code.
strtrsResponseStatus :: Lens' StartThingRegistrationTaskResponse Int
strtrsResponseStatus = lens _strtrsResponseStatus (\ s a -> s{_strtrsResponseStatus = a})

instance NFData StartThingRegistrationTaskResponse
         where

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
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
--
module Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
    (
    -- * Creating a Request
      addApplicationCloudWatchLoggingOption
    , AddApplicationCloudWatchLoggingOption
    -- * Request Lenses
    , aacwloApplicationName
    , aacwloCurrentApplicationVersionId
    , aacwloCloudWatchLoggingOption

    -- * Destructuring the Response
    , addApplicationCloudWatchLoggingOptionResponse
    , AddApplicationCloudWatchLoggingOptionResponse
    -- * Response Lenses
    , aacwlorsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addApplicationCloudWatchLoggingOption' smart constructor.
data AddApplicationCloudWatchLoggingOption = AddApplicationCloudWatchLoggingOption'
  { _aacwloApplicationName             :: !Text
  , _aacwloCurrentApplicationVersionId :: !Nat
  , _aacwloCloudWatchLoggingOption     :: !CloudWatchLoggingOption
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationCloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aacwloApplicationName' - The Kinesis Analytics application name.
--
-- * 'aacwloCurrentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- * 'aacwloCloudWatchLoggingOption' - Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
addApplicationCloudWatchLoggingOption
    :: Text -- ^ 'aacwloApplicationName'
    -> Natural -- ^ 'aacwloCurrentApplicationVersionId'
    -> CloudWatchLoggingOption -- ^ 'aacwloCloudWatchLoggingOption'
    -> AddApplicationCloudWatchLoggingOption
addApplicationCloudWatchLoggingOption pApplicationName_ pCurrentApplicationVersionId_ pCloudWatchLoggingOption_ =
  AddApplicationCloudWatchLoggingOption'
    { _aacwloApplicationName = pApplicationName_
    , _aacwloCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _aacwloCloudWatchLoggingOption = pCloudWatchLoggingOption_
    }


-- | The Kinesis Analytics application name.
aacwloApplicationName :: Lens' AddApplicationCloudWatchLoggingOption Text
aacwloApplicationName = lens _aacwloApplicationName (\ s a -> s{_aacwloApplicationName = a})

-- | The version ID of the Kinesis Analytics application.
aacwloCurrentApplicationVersionId :: Lens' AddApplicationCloudWatchLoggingOption Natural
aacwloCurrentApplicationVersionId = lens _aacwloCurrentApplicationVersionId (\ s a -> s{_aacwloCurrentApplicationVersionId = a}) . _Nat

-- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
aacwloCloudWatchLoggingOption :: Lens' AddApplicationCloudWatchLoggingOption CloudWatchLoggingOption
aacwloCloudWatchLoggingOption = lens _aacwloCloudWatchLoggingOption (\ s a -> s{_aacwloCloudWatchLoggingOption = a})

instance AWSRequest
           AddApplicationCloudWatchLoggingOption
         where
        type Rs AddApplicationCloudWatchLoggingOption =
             AddApplicationCloudWatchLoggingOptionResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 AddApplicationCloudWatchLoggingOptionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           AddApplicationCloudWatchLoggingOption
         where

instance NFData AddApplicationCloudWatchLoggingOption
         where

instance ToHeaders
           AddApplicationCloudWatchLoggingOption
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.AddApplicationCloudWatchLoggingOption"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddApplicationCloudWatchLoggingOption
         where
        toJSON AddApplicationCloudWatchLoggingOption'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _aacwloApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _aacwloCurrentApplicationVersionId),
                  Just
                    ("CloudWatchLoggingOption" .=
                       _aacwloCloudWatchLoggingOption)])

instance ToPath AddApplicationCloudWatchLoggingOption
         where
        toPath = const "/"

instance ToQuery
           AddApplicationCloudWatchLoggingOption
         where
        toQuery = const mempty

-- | /See:/ 'addApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse'
  { _aacwlorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationCloudWatchLoggingOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aacwlorsResponseStatus' - -- | The response status code.
addApplicationCloudWatchLoggingOptionResponse
    :: Int -- ^ 'aacwlorsResponseStatus'
    -> AddApplicationCloudWatchLoggingOptionResponse
addApplicationCloudWatchLoggingOptionResponse pResponseStatus_ =
  AddApplicationCloudWatchLoggingOptionResponse'
    {_aacwlorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aacwlorsResponseStatus :: Lens' AddApplicationCloudWatchLoggingOptionResponse Int
aacwlorsResponseStatus = lens _aacwlorsResponseStatus (\ s a -> s{_aacwlorsResponseStatus = a})

instance NFData
           AddApplicationCloudWatchLoggingOptionResponse
         where

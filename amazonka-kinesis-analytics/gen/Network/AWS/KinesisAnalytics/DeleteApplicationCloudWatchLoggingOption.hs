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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
--
module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
    (
    -- * Creating a Request
      deleteApplicationCloudWatchLoggingOption
    , DeleteApplicationCloudWatchLoggingOption
    -- * Request Lenses
    , dacwloApplicationName
    , dacwloCurrentApplicationVersionId
    , dacwloCloudWatchLoggingOptionId

    -- * Destructuring the Response
    , deleteApplicationCloudWatchLoggingOptionResponse
    , DeleteApplicationCloudWatchLoggingOptionResponse
    -- * Response Lenses
    , dacwlorsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { _dacwloApplicationName             :: !Text
  , _dacwloCurrentApplicationVersionId :: !Nat
  , _dacwloCloudWatchLoggingOptionId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationCloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacwloApplicationName' - The Kinesis Analytics application name.
--
-- * 'dacwloCurrentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- * 'dacwloCloudWatchLoggingOptionId' - The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the 'DescribeApplication' operation.
deleteApplicationCloudWatchLoggingOption
    :: Text -- ^ 'dacwloApplicationName'
    -> Natural -- ^ 'dacwloCurrentApplicationVersionId'
    -> Text -- ^ 'dacwloCloudWatchLoggingOptionId'
    -> DeleteApplicationCloudWatchLoggingOption
deleteApplicationCloudWatchLoggingOption pApplicationName_ pCurrentApplicationVersionId_ pCloudWatchLoggingOptionId_ =
  DeleteApplicationCloudWatchLoggingOption'
    { _dacwloApplicationName = pApplicationName_
    , _dacwloCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _dacwloCloudWatchLoggingOptionId = pCloudWatchLoggingOptionId_
    }


-- | The Kinesis Analytics application name.
dacwloApplicationName :: Lens' DeleteApplicationCloudWatchLoggingOption Text
dacwloApplicationName = lens _dacwloApplicationName (\ s a -> s{_dacwloApplicationName = a})

-- | The version ID of the Kinesis Analytics application.
dacwloCurrentApplicationVersionId :: Lens' DeleteApplicationCloudWatchLoggingOption Natural
dacwloCurrentApplicationVersionId = lens _dacwloCurrentApplicationVersionId (\ s a -> s{_dacwloCurrentApplicationVersionId = a}) . _Nat

-- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the 'DescribeApplication' operation.
dacwloCloudWatchLoggingOptionId :: Lens' DeleteApplicationCloudWatchLoggingOption Text
dacwloCloudWatchLoggingOptionId = lens _dacwloCloudWatchLoggingOptionId (\ s a -> s{_dacwloCloudWatchLoggingOptionId = a})

instance AWSRequest
           DeleteApplicationCloudWatchLoggingOption
         where
        type Rs DeleteApplicationCloudWatchLoggingOption =
             DeleteApplicationCloudWatchLoggingOptionResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteApplicationCloudWatchLoggingOptionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           DeleteApplicationCloudWatchLoggingOption
         where

instance NFData
           DeleteApplicationCloudWatchLoggingOption
         where

instance ToHeaders
           DeleteApplicationCloudWatchLoggingOption
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DeleteApplicationCloudWatchLoggingOption"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DeleteApplicationCloudWatchLoggingOption
         where
        toJSON DeleteApplicationCloudWatchLoggingOption'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _dacwloApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _dacwloCurrentApplicationVersionId),
                  Just
                    ("CloudWatchLoggingOptionId" .=
                       _dacwloCloudWatchLoggingOptionId)])

instance ToPath
           DeleteApplicationCloudWatchLoggingOption
         where
        toPath = const "/"

instance ToQuery
           DeleteApplicationCloudWatchLoggingOption
         where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { _dacwlorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationCloudWatchLoggingOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacwlorsResponseStatus' - -- | The response status code.
deleteApplicationCloudWatchLoggingOptionResponse
    :: Int -- ^ 'dacwlorsResponseStatus'
    -> DeleteApplicationCloudWatchLoggingOptionResponse
deleteApplicationCloudWatchLoggingOptionResponse pResponseStatus_ =
  DeleteApplicationCloudWatchLoggingOptionResponse'
    {_dacwlorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dacwlorsResponseStatus :: Lens' DeleteApplicationCloudWatchLoggingOptionResponse Int
dacwlorsResponseStatus = lens _dacwlorsResponseStatus (\ s a -> s{_dacwlorsResponseStatus = a})

instance NFData
           DeleteApplicationCloudWatchLoggingOptionResponse
         where

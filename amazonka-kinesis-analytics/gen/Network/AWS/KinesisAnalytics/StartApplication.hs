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
-- Module      : Network.AWS.KinesisAnalytics.StartApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified Amazon Kinesis Analytics application. After creating an application, you must exclusively call this operation to start your application.
--
--
-- After the application starts, it begins consuming the input data, processes it, and writes the output to the configured destination.
--
-- The application status must be @READY@ for you to start an application. You can get the application status in the console or using the 'DescribeApplication' operation.
--
-- After you start the application, you can stop the application from processing the input by calling the 'StopApplication' operation.
--
-- This operation requires permissions to perform the @kinesisanalytics:StartApplication@ action.
--
module Network.AWS.KinesisAnalytics.StartApplication
    (
    -- * Creating a Request
      startApplication
    , StartApplication
    -- * Request Lenses
    , saApplicationName
    , saInputConfigurations

    -- * Destructuring the Response
    , startApplicationResponse
    , StartApplicationResponse
    -- * Response Lenses
    , sarsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'startApplication' smart constructor.
data StartApplication = StartApplication'
  { _saApplicationName     :: !Text
  , _saInputConfigurations :: ![InputConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saApplicationName' - Name of the application.
--
-- * 'saInputConfigurations' - Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
startApplication
    :: Text -- ^ 'saApplicationName'
    -> StartApplication
startApplication pApplicationName_ =
  StartApplication'
    {_saApplicationName = pApplicationName_, _saInputConfigurations = mempty}


-- | Name of the application.
saApplicationName :: Lens' StartApplication Text
saApplicationName = lens _saApplicationName (\ s a -> s{_saApplicationName = a})

-- | Identifies the specific input, by ID, that the application starts consuming. Amazon Kinesis Analytics starts reading the streaming source associated with the input. You can also specify where in the streaming source you want Amazon Kinesis Analytics to start reading.
saInputConfigurations :: Lens' StartApplication [InputConfiguration]
saInputConfigurations = lens _saInputConfigurations (\ s a -> s{_saInputConfigurations = a}) . _Coerce

instance AWSRequest StartApplication where
        type Rs StartApplication = StartApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 StartApplicationResponse' <$> (pure (fromEnum s)))

instance Hashable StartApplication where

instance NFData StartApplication where

instance ToHeaders StartApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.StartApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartApplication where
        toJSON StartApplication'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _saApplicationName),
                  Just
                    ("InputConfigurations" .= _saInputConfigurations)])

instance ToPath StartApplication where
        toPath = const "/"

instance ToQuery StartApplication where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'startApplicationResponse' smart constructor.
newtype StartApplicationResponse = StartApplicationResponse'
  { _sarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarsResponseStatus' - -- | The response status code.
startApplicationResponse
    :: Int -- ^ 'sarsResponseStatus'
    -> StartApplicationResponse
startApplicationResponse pResponseStatus_ =
  StartApplicationResponse' {_sarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sarsResponseStatus :: Lens' StartApplicationResponse Int
sarsResponseStatus = lens _sarsResponseStatus (\ s a -> s{_sarsResponseStatus = a})

instance NFData StartApplicationResponse where

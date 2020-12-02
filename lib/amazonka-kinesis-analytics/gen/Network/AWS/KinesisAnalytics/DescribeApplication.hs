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
-- Module      : Network.AWS.KinesisAnalytics.DescribeApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Kinesis Analytics application.
--
--
-- If you want to retrieve a list of all applications in your account, use the 'ListApplications' operation.
--
-- This operation requires permissions to perform the @kinesisanalytics:DescribeApplication@ action. You can use @DescribeApplication@ to get the current application versionId, which you need to call other operations such as @Update@ .
--
module Network.AWS.KinesisAnalytics.DescribeApplication
    (
    -- * Creating a Request
      describeApplication
    , DescribeApplication
    -- * Request Lenses
    , daApplicationName

    -- * Destructuring the Response
    , describeApplicationResponse
    , DescribeApplicationResponse
    -- * Response Lenses
    , darsResponseStatus
    , darsApplicationDetail
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
-- /See:/ 'describeApplication' smart constructor.
newtype DescribeApplication = DescribeApplication'
  { _daApplicationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationName' - Name of the application.
describeApplication
    :: Text -- ^ 'daApplicationName'
    -> DescribeApplication
describeApplication pApplicationName_ =
  DescribeApplication' {_daApplicationName = pApplicationName_}


-- | Name of the application.
daApplicationName :: Lens' DescribeApplication Text
daApplicationName = lens _daApplicationName (\ s a -> s{_daApplicationName = a})

instance AWSRequest DescribeApplication where
        type Rs DescribeApplication =
             DescribeApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribeApplicationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ApplicationDetail"))

instance Hashable DescribeApplication where

instance NFData DescribeApplication where

instance ToHeaders DescribeApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DescribeApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeApplication where
        toJSON DescribeApplication'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _daApplicationName)])

instance ToPath DescribeApplication where
        toPath = const "/"

instance ToQuery DescribeApplication where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { _darsResponseStatus    :: !Int
  , _darsApplicationDetail :: !ApplicationDetail
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
--
-- * 'darsApplicationDetail' - Provides a description of the application, such as the application Amazon Resource Name (ARN), status, latest version, and input and output configuration details.
describeApplicationResponse
    :: Int -- ^ 'darsResponseStatus'
    -> ApplicationDetail -- ^ 'darsApplicationDetail'
    -> DescribeApplicationResponse
describeApplicationResponse pResponseStatus_ pApplicationDetail_ =
  DescribeApplicationResponse'
    { _darsResponseStatus = pResponseStatus_
    , _darsApplicationDetail = pApplicationDetail_
    }


-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeApplicationResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

-- | Provides a description of the application, such as the application Amazon Resource Name (ARN), status, latest version, and input and output configuration details.
darsApplicationDetail :: Lens' DescribeApplicationResponse ApplicationDetail
darsApplicationDetail = lens _darsApplicationDetail (\ s a -> s{_darsApplicationDetail = a})

instance NFData DescribeApplicationResponse where

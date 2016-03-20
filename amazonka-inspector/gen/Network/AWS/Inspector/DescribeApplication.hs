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
-- Module      : Network.AWS.Inspector.DescribeApplication
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the application specified by the application ARN.
module Network.AWS.Inspector.DescribeApplication
    (
    -- * Creating a Request
      describeApplication
    , DescribeApplication
    -- * Request Lenses
    , daApplicationARN

    -- * Destructuring the Response
    , describeApplicationResponse
    , DescribeApplicationResponse
    -- * Response Lenses
    , darsApplication
    , darsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeApplication' smart constructor.
newtype DescribeApplication = DescribeApplication'
    { _daApplicationARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationARN'
describeApplication
    :: Text -- ^ 'daApplicationARN'
    -> DescribeApplication
describeApplication pApplicationARN_ =
    DescribeApplication'
    { _daApplicationARN = pApplicationARN_
    }

-- | The ARN specifying the application that you want to describe.
daApplicationARN :: Lens' DescribeApplication Text
daApplicationARN = lens _daApplicationARN (\ s a -> s{_daApplicationARN = a});

instance AWSRequest DescribeApplication where
        type Rs DescribeApplication =
             DescribeApplicationResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeApplicationResponse' <$>
                   (x .?> "application") <*> (pure (fromEnum s)))

instance Hashable DescribeApplication

instance ToHeaders DescribeApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeApplication where
        toJSON DescribeApplication'{..}
          = object
              (catMaybes
                 [Just ("applicationArn" .= _daApplicationARN)])

instance ToPath DescribeApplication where
        toPath = const "/"

instance ToQuery DescribeApplication where
        toQuery = const mempty

-- | /See:/ 'describeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
    { _darsApplication    :: !(Maybe Application)
    , _darsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsApplication'
--
-- * 'darsResponseStatus'
describeApplicationResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeApplicationResponse
describeApplicationResponse pResponseStatus_ =
    DescribeApplicationResponse'
    { _darsApplication = Nothing
    , _darsResponseStatus = pResponseStatus_
    }

-- | Information about the application.
darsApplication :: Lens' DescribeApplicationResponse (Maybe Application)
darsApplication = lens _darsApplication (\ s a -> s{_darsApplication = a});

-- | The response status code.
darsResponseStatus :: Lens' DescribeApplicationResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a});

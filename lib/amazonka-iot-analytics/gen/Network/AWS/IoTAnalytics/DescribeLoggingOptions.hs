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
-- Module      : Network.AWS.IoTAnalytics.DescribeLoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current settings of the AWS IoT Analytics logging options.
--
--
module Network.AWS.IoTAnalytics.DescribeLoggingOptions
    (
    -- * Creating a Request
      describeLoggingOptions
    , DescribeLoggingOptions

    -- * Destructuring the Response
    , describeLoggingOptionsResponse
    , DescribeLoggingOptionsResponse
    -- * Response Lenses
    , dlorsLoggingOptions
    , dlorsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLoggingOptions' smart constructor.
data DescribeLoggingOptions =
  DescribeLoggingOptions'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoggingOptions' with the minimum fields required to make a request.
--
describeLoggingOptions
    :: DescribeLoggingOptions
describeLoggingOptions = DescribeLoggingOptions'


instance AWSRequest DescribeLoggingOptions where
        type Rs DescribeLoggingOptions =
             DescribeLoggingOptionsResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLoggingOptionsResponse' <$>
                   (x .?> "loggingOptions") <*> (pure (fromEnum s)))

instance Hashable DescribeLoggingOptions where

instance NFData DescribeLoggingOptions where

instance ToHeaders DescribeLoggingOptions where
        toHeaders = const mempty

instance ToPath DescribeLoggingOptions where
        toPath = const "/logging"

instance ToQuery DescribeLoggingOptions where
        toQuery = const mempty

-- | /See:/ 'describeLoggingOptionsResponse' smart constructor.
data DescribeLoggingOptionsResponse = DescribeLoggingOptionsResponse'
  { _dlorsLoggingOptions :: !(Maybe LoggingOptions)
  , _dlorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoggingOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlorsLoggingOptions' - The current settings of the AWS IoT Analytics logging options.
--
-- * 'dlorsResponseStatus' - -- | The response status code.
describeLoggingOptionsResponse
    :: Int -- ^ 'dlorsResponseStatus'
    -> DescribeLoggingOptionsResponse
describeLoggingOptionsResponse pResponseStatus_ =
  DescribeLoggingOptionsResponse'
    {_dlorsLoggingOptions = Nothing, _dlorsResponseStatus = pResponseStatus_}


-- | The current settings of the AWS IoT Analytics logging options.
dlorsLoggingOptions :: Lens' DescribeLoggingOptionsResponse (Maybe LoggingOptions)
dlorsLoggingOptions = lens _dlorsLoggingOptions (\ s a -> s{_dlorsLoggingOptions = a})

-- | -- | The response status code.
dlorsResponseStatus :: Lens' DescribeLoggingOptionsResponse Int
dlorsResponseStatus = lens _dlorsResponseStatus (\ s a -> s{_dlorsResponseStatus = a})

instance NFData DescribeLoggingOptionsResponse where

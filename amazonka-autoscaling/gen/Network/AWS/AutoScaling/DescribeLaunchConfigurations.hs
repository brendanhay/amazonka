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
-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch configurations.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    (
    -- * Creating a Request
      describeLaunchConfigurations
    , DescribeLaunchConfigurations
    -- * Request Lenses
    , dlcLaunchConfigurationNames
    , dlcNextToken
    , dlcMaxRecords

    -- * Destructuring the Response
    , describeLaunchConfigurationsResponse
    , DescribeLaunchConfigurationsResponse
    -- * Response Lenses
    , dlcrsNextToken
    , dlcrsResponseStatus
    , dlcrsLaunchConfigurations
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLaunchConfigurations' smart constructor.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
  { _dlcLaunchConfigurationNames :: !(Maybe [Text])
  , _dlcNextToken                :: !(Maybe Text)
  , _dlcMaxRecords               :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLaunchConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcLaunchConfigurationNames' - The launch configuration names. If you omit this parameter, all launch configurations are described.
--
-- * 'dlcNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dlcMaxRecords' - The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
describeLaunchConfigurations
    :: DescribeLaunchConfigurations
describeLaunchConfigurations =
  DescribeLaunchConfigurations'
    { _dlcLaunchConfigurationNames = Nothing
    , _dlcNextToken = Nothing
    , _dlcMaxRecords = Nothing
    }


-- | The launch configuration names. If you omit this parameter, all launch configurations are described.
dlcLaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlcLaunchConfigurationNames = lens _dlcLaunchConfigurationNames (\ s a -> s{_dlcLaunchConfigurationNames = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dlcNextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlcNextToken = lens _dlcNextToken (\ s a -> s{_dlcNextToken = a})

-- | The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
dlcMaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Int)
dlcMaxRecords = lens _dlcMaxRecords (\ s a -> s{_dlcMaxRecords = a})

instance AWSPager DescribeLaunchConfigurations where
        page rq rs
          | stop (rs ^. dlcrsNextToken) = Nothing
          | stop (rs ^. dlcrsLaunchConfigurations) = Nothing
          | otherwise =
            Just $ rq & dlcNextToken .~ rs ^. dlcrsNextToken

instance AWSRequest DescribeLaunchConfigurations
         where
        type Rs DescribeLaunchConfigurations =
             DescribeLaunchConfigurationsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeLaunchConfigurationsResult"
              (\ s h x ->
                 DescribeLaunchConfigurationsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "LaunchConfigurations" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeLaunchConfigurations where

instance NFData DescribeLaunchConfigurations where

instance ToHeaders DescribeLaunchConfigurations where
        toHeaders = const mempty

instance ToPath DescribeLaunchConfigurations where
        toPath = const "/"

instance ToQuery DescribeLaunchConfigurations where
        toQuery DescribeLaunchConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeLaunchConfigurations" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "LaunchConfigurationNames" =:
                 toQuery
                   (toQueryList "member" <$>
                      _dlcLaunchConfigurationNames),
               "NextToken" =: _dlcNextToken,
               "MaxRecords" =: _dlcMaxRecords]

-- | /See:/ 'describeLaunchConfigurationsResponse' smart constructor.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
  { _dlcrsNextToken            :: !(Maybe Text)
  , _dlcrsResponseStatus       :: !Int
  , _dlcrsLaunchConfigurations :: ![LaunchConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLaunchConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dlcrsResponseStatus' - -- | The response status code.
--
-- * 'dlcrsLaunchConfigurations' - The launch configurations.
describeLaunchConfigurationsResponse
    :: Int -- ^ 'dlcrsResponseStatus'
    -> DescribeLaunchConfigurationsResponse
describeLaunchConfigurationsResponse pResponseStatus_ =
  DescribeLaunchConfigurationsResponse'
    { _dlcrsNextToken = Nothing
    , _dlcrsResponseStatus = pResponseStatus_
    , _dlcrsLaunchConfigurations = mempty
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dlcrsNextToken :: Lens' DescribeLaunchConfigurationsResponse (Maybe Text)
dlcrsNextToken = lens _dlcrsNextToken (\ s a -> s{_dlcrsNextToken = a})

-- | -- | The response status code.
dlcrsResponseStatus :: Lens' DescribeLaunchConfigurationsResponse Int
dlcrsResponseStatus = lens _dlcrsResponseStatus (\ s a -> s{_dlcrsResponseStatus = a})

-- | The launch configurations.
dlcrsLaunchConfigurations :: Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrsLaunchConfigurations = lens _dlcrsLaunchConfigurations (\ s a -> s{_dlcrsLaunchConfigurations = a}) . _Coerce

instance NFData DescribeLaunchConfigurationsResponse
         where

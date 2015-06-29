{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more launch configurations. If you omit the list of
-- names, then the call describes all launch configurations.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurations.html>
module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    (
    -- * Request
      DescribeLaunchConfigurations
    -- ** Request constructor
    , describeLaunchConfigurations
    -- ** Request lenses
    , dlcLaunchConfigurationNames
    , dlcNextToken
    , dlcMaxRecords

    -- * Response
    , DescribeLaunchConfigurationsResponse
    -- ** Response constructor
    , describeLaunchConfigurationsResponse
    -- ** Response lenses
    , dlcrNextToken
    , dlcrStatus
    , dlcrLaunchConfigurations
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLaunchConfigurations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcLaunchConfigurationNames'
--
-- * 'dlcNextToken'
--
-- * 'dlcMaxRecords'
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
    { _dlcLaunchConfigurationNames :: !(Maybe [Text])
    , _dlcNextToken                :: !(Maybe Text)
    , _dlcMaxRecords               :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeLaunchConfigurations' smart constructor.
describeLaunchConfigurations :: DescribeLaunchConfigurations
describeLaunchConfigurations =
    DescribeLaunchConfigurations'
    { _dlcLaunchConfigurationNames = Nothing
    , _dlcNextToken = Nothing
    , _dlcMaxRecords = Nothing
    }

-- | The launch configuration names.
dlcLaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlcLaunchConfigurationNames = lens _dlcLaunchConfigurationNames (\ s a -> s{_dlcLaunchConfigurationNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dlcNextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlcNextToken = lens _dlcNextToken (\ s a -> s{_dlcNextToken = a});

-- | The maximum number of items to return with this call. The default is
-- 100.
dlcMaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Int)
dlcMaxRecords = lens _dlcMaxRecords (\ s a -> s{_dlcMaxRecords = a});

instance AWSPager DescribeLaunchConfigurations where
        page rq rs
          | stop (rs ^. dlcrNextToken) = Nothing
          | stop (rs ^. dlcrLaunchConfigurations) = Nothing
          | otherwise =
            Just $ rq & dlcNextToken .~ rs ^. dlcrNextToken

instance AWSRequest DescribeLaunchConfigurations
         where
        type Sv DescribeLaunchConfigurations = AutoScaling
        type Rs DescribeLaunchConfigurations =
             DescribeLaunchConfigurationsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeLaunchConfigurationsResult"
              (\ s h x ->
                 DescribeLaunchConfigurationsResponse' <$>
                   (x .@? "NextToken") <*> (pure s) <*>
                     (x .@? "LaunchConfigurations" .!@ mempty >>=
                        parseXMLList "member"))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrNextToken'
--
-- * 'dlcrStatus'
--
-- * 'dlcrLaunchConfigurations'
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
    { _dlcrNextToken            :: !(Maybe Text)
    , _dlcrStatus               :: !Status
    , _dlcrLaunchConfigurations :: ![LaunchConfiguration]
    } deriving (Eq,Show)

-- | 'DescribeLaunchConfigurationsResponse' smart constructor.
describeLaunchConfigurationsResponse :: Status -> DescribeLaunchConfigurationsResponse
describeLaunchConfigurationsResponse pStatus =
    DescribeLaunchConfigurationsResponse'
    { _dlcrNextToken = Nothing
    , _dlcrStatus = pStatus
    , _dlcrLaunchConfigurations = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dlcrNextToken :: Lens' DescribeLaunchConfigurationsResponse (Maybe Text)
dlcrNextToken = lens _dlcrNextToken (\ s a -> s{_dlcrNextToken = a});

-- | FIXME: Undocumented member.
dlcrStatus :: Lens' DescribeLaunchConfigurationsResponse Status
dlcrStatus = lens _dlcrStatus (\ s a -> s{_dlcrStatus = a});

-- | The launch configurations.
dlcrLaunchConfigurations :: Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrLaunchConfigurations = lens _dlcrLaunchConfigurations (\ s a -> s{_dlcrLaunchConfigurations = a});

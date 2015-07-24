{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch configurations. If you omit the list of
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
    , dlcrsNextToken
    , dlcrsStatus
    , dlcrsLaunchConfigurations
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. dlcrsNextToken) = Nothing
          | stop (rs ^. dlcrsLaunchConfigurations) = Nothing
          | otherwise =
            Just $ rq & dlcNextToken .~ rs ^. dlcrsNextToken

instance AWSRequest DescribeLaunchConfigurations
         where
        type Sv DescribeLaunchConfigurations = AutoScaling
        type Rs DescribeLaunchConfigurations =
             DescribeLaunchConfigurationsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeLaunchConfigurationsResult"
              (\ s h x ->
                 DescribeLaunchConfigurationsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
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
-- * 'dlcrsNextToken'
--
-- * 'dlcrsStatus'
--
-- * 'dlcrsLaunchConfigurations'
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
    { _dlcrsNextToken            :: !(Maybe Text)
    , _dlcrsStatus               :: !Int
    , _dlcrsLaunchConfigurations :: ![LaunchConfiguration]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLaunchConfigurationsResponse' smart constructor.
describeLaunchConfigurationsResponse :: Int -> DescribeLaunchConfigurationsResponse
describeLaunchConfigurationsResponse pStatus_ =
    DescribeLaunchConfigurationsResponse'
    { _dlcrsNextToken = Nothing
    , _dlcrsStatus = pStatus_
    , _dlcrsLaunchConfigurations = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dlcrsNextToken :: Lens' DescribeLaunchConfigurationsResponse (Maybe Text)
dlcrsNextToken = lens _dlcrsNextToken (\ s a -> s{_dlcrsNextToken = a});

-- | FIXME: Undocumented member.
dlcrsStatus :: Lens' DescribeLaunchConfigurationsResponse Int
dlcrsStatus = lens _dlcrsStatus (\ s a -> s{_dlcrsStatus = a});

-- | The launch configurations.
dlcrsLaunchConfigurations :: Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrsLaunchConfigurations = lens _dlcrsLaunchConfigurations (\ s a -> s{_dlcrsLaunchConfigurations = a});

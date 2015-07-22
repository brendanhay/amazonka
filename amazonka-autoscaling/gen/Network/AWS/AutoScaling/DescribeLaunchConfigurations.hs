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
    , dlcrqLaunchConfigurationNames
    , dlcrqNextToken
    , dlcrqMaxRecords

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
-- * 'dlcrqLaunchConfigurationNames'
--
-- * 'dlcrqNextToken'
--
-- * 'dlcrqMaxRecords'
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
    { _dlcrqLaunchConfigurationNames :: !(Maybe [Text])
    , _dlcrqNextToken                :: !(Maybe Text)
    , _dlcrqMaxRecords               :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLaunchConfigurations' smart constructor.
describeLaunchConfigurations :: DescribeLaunchConfigurations
describeLaunchConfigurations =
    DescribeLaunchConfigurations'
    { _dlcrqLaunchConfigurationNames = Nothing
    , _dlcrqNextToken = Nothing
    , _dlcrqMaxRecords = Nothing
    }

-- | The launch configuration names.
dlcrqLaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlcrqLaunchConfigurationNames = lens _dlcrqLaunchConfigurationNames (\ s a -> s{_dlcrqLaunchConfigurationNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dlcrqNextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlcrqNextToken = lens _dlcrqNextToken (\ s a -> s{_dlcrqNextToken = a});

-- | The maximum number of items to return with this call. The default is
-- 100.
dlcrqMaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Int)
dlcrqMaxRecords = lens _dlcrqMaxRecords (\ s a -> s{_dlcrqMaxRecords = a});

instance AWSPager DescribeLaunchConfigurations where
        page rq rs
          | stop (rs ^. dlcrsNextToken) = Nothing
          | stop (rs ^. dlcrsLaunchConfigurations) = Nothing
          | otherwise =
            Just $ rq & dlcrqNextToken .~ rs ^. dlcrsNextToken

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
                      _dlcrqLaunchConfigurationNames),
               "NextToken" =: _dlcrqNextToken,
               "MaxRecords" =: _dlcrqMaxRecords]

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

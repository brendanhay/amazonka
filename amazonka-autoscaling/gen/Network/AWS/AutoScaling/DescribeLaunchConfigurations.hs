{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more launch configurations. If you omit the list of names,
-- then the call describes all launch configurations. You can specify a
-- maximum number of items to be returned with a single call. If there are
-- more items to return, the call returns a token. To get the next set of
-- items, repeat the call with the returned token in the 'NextToken'
-- parameter.
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
    , dlcMaxRecords
    , dlcNextToken

    -- * Response
    , DescribeLaunchConfigurationsResponse
    -- ** Response constructor
    , describeLaunchConfigurationsResponse
    -- ** Response lenses
    , dlcrLaunchConfigurations
    , dlcrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { _dlcLaunchConfigurationNames :: List "LaunchConfigurationNames" Text
    , _dlcMaxRecords               :: Maybe Int
    , _dlcNextToken                :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeLaunchConfigurations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcLaunchConfigurationNames' @::@ ['Text']
--
-- * 'dlcMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dlcNextToken' @::@ 'Maybe' 'Text'
--
describeLaunchConfigurations :: DescribeLaunchConfigurations
describeLaunchConfigurations = DescribeLaunchConfigurations
    { _dlcLaunchConfigurationNames = mempty
    , _dlcNextToken                = Nothing
    , _dlcMaxRecords               = Nothing
    }

-- | The launch configuration names.
dlcLaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlcLaunchConfigurationNames =
    lens _dlcLaunchConfigurationNames
        (\s a -> s { _dlcLaunchConfigurationNames = a })
            . _List

-- | The maximum number of items to return with this call. The default is 100.
dlcMaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Int)
dlcMaxRecords = lens _dlcMaxRecords (\s a -> s { _dlcMaxRecords = a })

-- | The token for the next set of items to return. (You received this token
-- from a previous call.).
dlcNextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlcNextToken = lens _dlcNextToken (\s a -> s { _dlcNextToken = a })

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { _dlcrLaunchConfigurations :: List "LaunchConfigurations" LaunchConfiguration
    , _dlcrNextToken            :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeLaunchConfigurationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrLaunchConfigurations' @::@ ['LaunchConfiguration']
--
-- * 'dlcrNextToken' @::@ 'Maybe' 'Text'
--
describeLaunchConfigurationsResponse :: DescribeLaunchConfigurationsResponse
describeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { _dlcrLaunchConfigurations = mempty
    , _dlcrNextToken            = Nothing
    }

-- | The launch configurations.
dlcrLaunchConfigurations :: Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrLaunchConfigurations =
    lens _dlcrLaunchConfigurations
        (\s a -> s { _dlcrLaunchConfigurations = a })
            . _List

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dlcrNextToken :: Lens' DescribeLaunchConfigurationsResponse (Maybe Text)
dlcrNextToken = lens _dlcrNextToken (\s a -> s { _dlcrNextToken = a })

instance ToPath DescribeLaunchConfigurations where
    toPath = const "/"

instance ToQuery DescribeLaunchConfigurations where
    toQuery DescribeLaunchConfigurations{..} = mconcat
        [ "LaunchConfigurationNames" =? _dlcLaunchConfigurationNames
        , "MaxRecords"               =? _dlcMaxRecords
        , "NextToken"                =? _dlcNextToken
        ]

instance ToHeaders DescribeLaunchConfigurations

instance AWSRequest DescribeLaunchConfigurations where
    type Sv DescribeLaunchConfigurations = AutoScaling
    type Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse

    request  = post "DescribeLaunchConfigurations"
    response = xmlResponse

instance FromXML DescribeLaunchConfigurationsResponse where
    parseXML = withElement "DescribeLaunchConfigurationsResult" $ \x -> DescribeLaunchConfigurationsResponse
        <$> x .@  "LaunchConfigurations"
        <*> x .@? "NextToken"

instance AWSPager DescribeLaunchConfigurations where
    page rq rs
        | stop (rq ^. dlcNextToken) = Nothing
        | otherwise = (\x -> rq & dlcNextToken ?~ x)
            <$> (rs ^. dlcrNextToken)

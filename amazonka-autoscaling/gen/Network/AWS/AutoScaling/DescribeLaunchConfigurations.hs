{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of the launch configurations, or the specified
-- launch configurations, if they exist. If no name is specified, then the
-- full details of all launch configurations are returned.
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

data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { _dlcLaunchConfigurationNames :: [Text]
    , _dlcMaxRecords               :: Maybe Int
    , _dlcNextToken                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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

-- | A list of launch configuration names.
dlcLaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlcLaunchConfigurationNames =
    lens _dlcLaunchConfigurationNames
        (\s a -> s { _dlcLaunchConfigurationNames = a })

-- | The maximum number of launch configurations. The default is 100.
dlcMaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Int)
dlcMaxRecords = lens _dlcMaxRecords (\s a -> s { _dlcMaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
dlcNextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlcNextToken = lens _dlcNextToken (\s a -> s { _dlcNextToken = a })

instance ToQuery DescribeLaunchConfigurations

instance ToPath DescribeLaunchConfigurations where
    toPath = const "/"

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { _dlcrLaunchConfigurations :: [LaunchConfiguration]
    , _dlcrNextToken            :: Maybe Text
    } deriving (Eq, Show, Generic)

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

-- | A list of launch configurations.
dlcrLaunchConfigurations :: Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
dlcrLaunchConfigurations =
    lens _dlcrLaunchConfigurations
        (\s a -> s { _dlcrLaunchConfigurations = a })

-- | A string that marks the start of the next batch of returned results.
dlcrNextToken :: Lens' DescribeLaunchConfigurationsResponse (Maybe Text)
dlcrNextToken = lens _dlcrNextToken (\s a -> s { _dlcrNextToken = a })

instance FromXML DescribeLaunchConfigurationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeLaunchConfigurationsResponse"

instance AWSRequest DescribeLaunchConfigurations where
    type Sv DescribeLaunchConfigurations = AutoScaling
    type Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse

    request  = post "DescribeLaunchConfigurations"
    response = xmlResponse $ \h x -> DescribeLaunchConfigurationsResponse
        <$> x %| "LaunchConfigurations"
        <*> x %| "NextToken"

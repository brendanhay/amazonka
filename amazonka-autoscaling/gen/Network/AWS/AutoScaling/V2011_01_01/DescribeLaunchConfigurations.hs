{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations
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
-- https://autoscaling.amazonaws.com/?LaunchConfigurationNames.member.1=my-test-lc
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeLaunchConfigurations
-- &AUTHPARAMS true dedicated 2013-01-21T23:04:42.200Z my-test-lc m1.small
-- arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:
-- 9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc
-- ami-514ac838 true false d05a22f8-b690-11e2-bf8e-2113fEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations
    (
    -- * Request
      DescribeLaunchConfigurations
    -- ** Request constructor
    , mkDescribeLaunchConfigurations
    -- ** Request lenses
    , dlc1LaunchConfigurationNames
    , dlc1NextToken
    , dlc1MaxRecords

    -- * Response
    , DescribeLaunchConfigurationsResponse
    -- ** Response lenses
    , dlcrLaunchConfigurations
    , dlcrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | The LaunchConfigurationNamesType data type.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { _dlc1LaunchConfigurationNames :: [Text]
    , _dlc1NextToken :: Maybe Text
    , _dlc1MaxRecords :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLaunchConfigurations' request.
mkDescribeLaunchConfigurations :: DescribeLaunchConfigurations
mkDescribeLaunchConfigurations = DescribeLaunchConfigurations
    { _dlc1LaunchConfigurationNames = mempty
    , _dlc1NextToken = Nothing
    , _dlc1MaxRecords = Nothing
    }

-- | A list of launch configuration names.
dlc1LaunchConfigurationNames :: Lens' DescribeLaunchConfigurations [Text]
dlc1LaunchConfigurationNames =
    lens _dlc1LaunchConfigurationNames
         (\s a -> s { _dlc1LaunchConfigurationNames = a })

-- | A string that marks the start of the next batch of returned results.
dlc1NextToken :: Lens' DescribeLaunchConfigurations (Maybe Text)
dlc1NextToken = lens _dlc1NextToken (\s a -> s { _dlc1NextToken = a })

-- | The maximum number of launch configurations. The default is 100.
dlc1MaxRecords :: Lens' DescribeLaunchConfigurations (Maybe Integer)
dlc1MaxRecords = lens _dlc1MaxRecords (\s a -> s { _dlc1MaxRecords = a })

instance ToQuery DescribeLaunchConfigurations where
    toQuery = genericQuery def

-- | The LaunchConfigurationsType data type.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { _dlcrLaunchConfigurations :: [LaunchConfiguration]
    , _dlcrNextToken :: Maybe Text
    } deriving (Show, Generic)

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

instance AWSRequest DescribeLaunchConfigurations where
    type Sv DescribeLaunchConfigurations = AutoScaling
    type Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse

    request = post "DescribeLaunchConfigurations"
    response _ = xmlResponse

instance AWSPager DescribeLaunchConfigurations where
    next rq rs = (\x -> rq & dlc1NextToken ?~ x)
        <$> (rs ^. dlcrNextToken)

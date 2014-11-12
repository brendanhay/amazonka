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
      LaunchConfigurationNamesType
    -- ** Request constructor
    , launchConfigurationNamesType
    -- ** Request lenses
    , lcntLaunchConfigurationNames
    , lcntMaxRecords
    , lcntNextToken

    -- * Response
    , LaunchConfigurationsType
    -- ** Response constructor
    , launchConfigurationsType
    -- ** Response lenses
    , lctLaunchConfigurations
    , lctNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data LaunchConfigurationNamesType = LaunchConfigurationNamesType
    { _lcntLaunchConfigurationNames :: [Text]
    , _lcntMaxRecords               :: Maybe Int
    , _lcntNextToken                :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'LaunchConfigurationNamesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcntLaunchConfigurationNames' @::@ ['Text']
--
-- * 'lcntMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'lcntNextToken' @::@ 'Maybe' 'Text'
--
launchConfigurationNamesType :: LaunchConfigurationNamesType
launchConfigurationNamesType = LaunchConfigurationNamesType
    { _lcntLaunchConfigurationNames = mempty
    , _lcntNextToken                = Nothing
    , _lcntMaxRecords               = Nothing
    }

-- | A list of launch configuration names.
lcntLaunchConfigurationNames :: Lens' LaunchConfigurationNamesType [Text]
lcntLaunchConfigurationNames =
    lens _lcntLaunchConfigurationNames
        (\s a -> s { _lcntLaunchConfigurationNames = a })

-- | The maximum number of launch configurations. The default is 100.
lcntMaxRecords :: Lens' LaunchConfigurationNamesType (Maybe Int)
lcntMaxRecords = lens _lcntMaxRecords (\s a -> s { _lcntMaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
lcntNextToken :: Lens' LaunchConfigurationNamesType (Maybe Text)
lcntNextToken = lens _lcntNextToken (\s a -> s { _lcntNextToken = a })
instance ToQuery LaunchConfigurationNamesType

instance ToPath LaunchConfigurationNamesType where
    toPath = const "/"

data LaunchConfigurationsType = LaunchConfigurationsType
    { _lctLaunchConfigurations :: [LaunchConfiguration]
    , _lctNextToken            :: Maybe Text
    } (Eq, Show, Generic)

-- | 'LaunchConfigurationsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lctLaunchConfigurations' @::@ ['LaunchConfiguration']
--
-- * 'lctNextToken' @::@ 'Maybe' 'Text'
--
launchConfigurationsType :: LaunchConfigurationsType
launchConfigurationsType = LaunchConfigurationsType
    { _lctLaunchConfigurations = mempty
    , _lctNextToken            = Nothing
    }

-- | A list of launch configurations.
lctLaunchConfigurations :: Lens' LaunchConfigurationsType [LaunchConfiguration]
lctLaunchConfigurations =
    lens _lctLaunchConfigurations (\s a -> s { _lctLaunchConfigurations = a })

-- | A string that marks the start of the next batch of returned results.
lctNextToken :: Lens' LaunchConfigurationsType (Maybe Text)
lctNextToken = lens _lctNextToken (\s a -> s { _lctNextToken = a })

instance FromXML LaunchConfigurationsType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfigurationsType"

instance AWSRequest LaunchConfigurationNamesType where
    type Sv LaunchConfigurationNamesType = AutoScaling
    type Rs LaunchConfigurationNamesType = LaunchConfigurationsType

    request  = post "DescribeLaunchConfigurations"
    response = xmlResponse $ \h x -> LaunchConfigurationsType
        <$> x %| "LaunchConfigurations"
        <*> x %| "NextToken"

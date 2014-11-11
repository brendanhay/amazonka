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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution stack
-- defines. The description includes the values the options, their default
-- values, and an indication of the required action on a running environment
-- if an option value is changed.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Request
      DescribeConfigurationOptionsMessage
    -- ** Request constructor
    , describeConfigurationOptionsMessage
    -- ** Request lenses
    , dcomApplicationName
    , dcomEnvironmentName
    , dcomOptions
    , dcomSolutionStackName
    , dcomTemplateName

    -- * Response
    , ConfigurationOptionsDescription
    -- ** Response constructor
    , configurationOptionsDescription
    -- ** Response lenses
    , codOptions
    , codSolutionStackName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeConfigurationOptionsMessage = DescribeConfigurationOptionsMessage
    { _dcomApplicationName   :: Maybe Text
    , _dcomEnvironmentName   :: Maybe Text
    , _dcomOptions           :: [OptionSpecification]
    , _dcomSolutionStackName :: Maybe Text
    , _dcomTemplateName      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeConfigurationOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcomApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'dcomEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'dcomOptions' @::@ ['OptionSpecification']
--
-- * 'dcomSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'dcomTemplateName' @::@ 'Maybe' 'Text'
--
describeConfigurationOptionsMessage :: DescribeConfigurationOptionsMessage
describeConfigurationOptionsMessage = DescribeConfigurationOptionsMessage
    { _dcomApplicationName   = Nothing
    , _dcomTemplateName      = Nothing
    , _dcomEnvironmentName   = Nothing
    , _dcomSolutionStackName = Nothing
    , _dcomOptions           = mempty
    }

-- | The name of the application associated with the configuration template or
-- environment. Only needed if you want to describe the configuration
-- options associated with either the configuration template or environment.
dcomApplicationName :: Lens' DescribeConfigurationOptionsMessage (Maybe Text)
dcomApplicationName =
    lens _dcomApplicationName (\s a -> s { _dcomApplicationName = a })

-- | The name of the environment whose configuration options you want to
-- describe.
dcomEnvironmentName :: Lens' DescribeConfigurationOptionsMessage (Maybe Text)
dcomEnvironmentName =
    lens _dcomEnvironmentName (\s a -> s { _dcomEnvironmentName = a })

-- | If specified, restricts the descriptions to only the specified options.
dcomOptions :: Lens' DescribeConfigurationOptionsMessage [OptionSpecification]
dcomOptions = lens _dcomOptions (\s a -> s { _dcomOptions = a })

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcomSolutionStackName :: Lens' DescribeConfigurationOptionsMessage (Maybe Text)
dcomSolutionStackName =
    lens _dcomSolutionStackName (\s a -> s { _dcomSolutionStackName = a })

-- | The name of the configuration template whose configuration options you
-- want to describe.
dcomTemplateName :: Lens' DescribeConfigurationOptionsMessage (Maybe Text)
dcomTemplateName = lens _dcomTemplateName (\s a -> s { _dcomTemplateName = a })
instance ToQuery DescribeConfigurationOptionsMessage

instance ToPath DescribeConfigurationOptionsMessage where
    toPath = const "/"

data ConfigurationOptionsDescription = ConfigurationOptionsDescription
    { _codOptions           :: [ConfigurationOptionDescription]
    , _codSolutionStackName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ConfigurationOptionsDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'codOptions' @::@ ['ConfigurationOptionDescription']
--
-- * 'codSolutionStackName' @::@ 'Maybe' 'Text'
--
configurationOptionsDescription :: ConfigurationOptionsDescription
configurationOptionsDescription = ConfigurationOptionsDescription
    { _codSolutionStackName = Nothing
    , _codOptions           = mempty
    }

-- | A list of ConfigurationOptionDescription.
codOptions :: Lens' ConfigurationOptionsDescription [ConfigurationOptionDescription]
codOptions = lens _codOptions (\s a -> s { _codOptions = a })

-- | The name of the solution stack these configuration options belong to.
codSolutionStackName :: Lens' ConfigurationOptionsDescription (Maybe Text)
codSolutionStackName =
    lens _codSolutionStackName (\s a -> s { _codSolutionStackName = a })
instance FromXML ConfigurationOptionsDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionsDescription"

instance AWSRequest DescribeConfigurationOptionsMessage where
    type Sv DescribeConfigurationOptionsMessage = ElasticBeanstalk
    type Rs DescribeConfigurationOptionsMessage = ConfigurationOptionsDescription

    request  = post "DescribeConfigurationOptions"
    response = xmlResponse $ \h x -> ConfigurationOptionsDescription
        <$> x %| "Options"
        <*> x %| "SolutionStackName"

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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the settings for the specified configuration set,
-- that is, either a configuration template or the configuration set
-- associated with a running environment. When describing the settings for the
-- configuration set associated with a running environment, it is possible to
-- receive two sets of setting descriptions. One is the deployed configuration
-- set, and the other is a draft configuration of an environment that is
-- either in the process of deployment or that failed to deploy. Related
-- Topics DeleteEnvironmentConfiguration.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    (
    -- * Request
      DescribeConfigurationSettings
    -- ** Request constructor
    , describeConfigurationSettings
    -- ** Request lenses
    , dcsApplicationName
    , dcsEnvironmentName
    , dcsTemplateName

    -- * Response
    , DescribeConfigurationSettingsResponse
    -- ** Response constructor
    , describeConfigurationSettingsResponse
    -- ** Response lenses
    , dcsrConfigurationSettings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeConfigurationSettings = DescribeConfigurationSettings
    { _dcsApplicationName :: Text
    , _dcsEnvironmentName :: Maybe Text
    , _dcsTemplateName    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeConfigurationSettings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsApplicationName' @::@ 'Text'
--
-- * 'dcsEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'dcsTemplateName' @::@ 'Maybe' 'Text'
--
describeConfigurationSettings :: Text -- ^ 'dcsApplicationName'
                              -> DescribeConfigurationSettings
describeConfigurationSettings p1 = DescribeConfigurationSettings
    { _dcsApplicationName = p1
    , _dcsTemplateName    = Nothing
    , _dcsEnvironmentName = Nothing
    }

-- | The application for the environment or configuration template.
dcsApplicationName :: Lens' DescribeConfigurationSettings Text
dcsApplicationName =
    lens _dcsApplicationName (\s a -> s { _dcsApplicationName = a })

-- | The name of the environment to describe. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
dcsEnvironmentName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsEnvironmentName =
    lens _dcsEnvironmentName (\s a -> s { _dcsEnvironmentName = a })

-- | The name of the configuration template to describe. Conditional: You must
-- specify either this parameter or an EnvironmentName, but not both. If you
-- specify both, AWS Elastic Beanstalk returns an
-- InvalidParameterCombination error. If you do not specify either, AWS
-- Elastic Beanstalk returns a MissingRequiredParameter error.
dcsTemplateName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsTemplateName = lens _dcsTemplateName (\s a -> s { _dcsTemplateName = a })

instance ToQuery DescribeConfigurationSettings

instance ToPath DescribeConfigurationSettings where
    toPath = const "/"

newtype DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse
    { _dcsrConfigurationSettings :: [ConfigurationSettingsDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeConfigurationSettingsResponse where
    type Item DescribeConfigurationSettingsResponse = ConfigurationSettingsDescription

    fromList = DescribeConfigurationSettingsResponse . fromList
    toList   = toList . _dcsrConfigurationSettings

-- | 'DescribeConfigurationSettingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrConfigurationSettings' @::@ ['ConfigurationSettingsDescription']
--
describeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse
describeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse
    { _dcsrConfigurationSettings = mempty
    }

-- | A list of ConfigurationSettingsDescription.
dcsrConfigurationSettings :: Lens' DescribeConfigurationSettingsResponse [ConfigurationSettingsDescription]
dcsrConfigurationSettings =
    lens _dcsrConfigurationSettings
        (\s a -> s { _dcsrConfigurationSettings = a })

instance FromXML DescribeConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeConfigurationSettingsResponse"

instance AWSRequest DescribeConfigurationSettings where
    type Sv DescribeConfigurationSettings = ElasticBeanstalk
    type Rs DescribeConfigurationSettings = DescribeConfigurationSettingsResponse

    request  = post "DescribeConfigurationSettings"
    response = xmlResponse $ \h x -> DescribeConfigurationSettingsResponse
        <$> x %| "ConfigurationSettings"

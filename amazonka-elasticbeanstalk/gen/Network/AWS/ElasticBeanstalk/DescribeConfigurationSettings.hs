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
      DescribeConfigurationSettingsMessage
    -- ** Request constructor
    , describeConfigurationSettingsMessage
    -- ** Request lenses
    , dcsmApplicationName
    , dcsmEnvironmentName
    , dcsmTemplateName

    -- * Response
    , ConfigurationSettingsDescriptions
    -- ** Response constructor
    , configurationSettingsDescriptions
    -- ** Response lenses
    , csdConfigurationSettings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeConfigurationSettingsMessage = DescribeConfigurationSettingsMessage
    { _dcsmApplicationName :: Text
    , _dcsmEnvironmentName :: Maybe Text
    , _dcsmTemplateName    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeConfigurationSettingsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsmApplicationName' @::@ 'Text'
--
-- * 'dcsmEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'dcsmTemplateName' @::@ 'Maybe' 'Text'
--
describeConfigurationSettingsMessage :: Text -- ^ 'dcsmApplicationName'
                                     -> DescribeConfigurationSettingsMessage
describeConfigurationSettingsMessage p1 = DescribeConfigurationSettingsMessage
    { _dcsmApplicationName = p1
    , _dcsmTemplateName    = Nothing
    , _dcsmEnvironmentName = Nothing
    }

-- | The application for the environment or configuration template.
dcsmApplicationName :: Lens' DescribeConfigurationSettingsMessage Text
dcsmApplicationName =
    lens _dcsmApplicationName (\s a -> s { _dcsmApplicationName = a })

-- | The name of the environment to describe. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
dcsmEnvironmentName :: Lens' DescribeConfigurationSettingsMessage (Maybe Text)
dcsmEnvironmentName =
    lens _dcsmEnvironmentName (\s a -> s { _dcsmEnvironmentName = a })

-- | The name of the configuration template to describe. Conditional: You must
-- specify either this parameter or an EnvironmentName, but not both. If you
-- specify both, AWS Elastic Beanstalk returns an
-- InvalidParameterCombination error. If you do not specify either, AWS
-- Elastic Beanstalk returns a MissingRequiredParameter error.
dcsmTemplateName :: Lens' DescribeConfigurationSettingsMessage (Maybe Text)
dcsmTemplateName = lens _dcsmTemplateName (\s a -> s { _dcsmTemplateName = a })
instance ToQuery DescribeConfigurationSettingsMessage

instance ToPath DescribeConfigurationSettingsMessage where
    toPath = const "/"

newtype ConfigurationSettingsDescriptions = ConfigurationSettingsDescriptions
    { _csdConfigurationSettings :: [ConfigurationSettingsDescription]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'ConfigurationSettingsDescriptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdConfigurationSettings' @::@ ['ConfigurationSettingsDescription']
--
configurationSettingsDescriptions :: ConfigurationSettingsDescriptions
configurationSettingsDescriptions = ConfigurationSettingsDescriptions
    { _csdConfigurationSettings = mempty
    }

-- | A list of ConfigurationSettingsDescription.
csdConfigurationSettings :: Lens' ConfigurationSettingsDescriptions [ConfigurationSettingsDescription]
csdConfigurationSettings =
    lens _csdConfigurationSettings
        (\s a -> s { _csdConfigurationSettings = a })
instance FromXML ConfigurationSettingsDescriptions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsDescriptions"

instance AWSRequest DescribeConfigurationSettingsMessage where
    type Sv DescribeConfigurationSettingsMessage = ElasticBeanstalk
    type Rs DescribeConfigurationSettingsMessage = ConfigurationSettingsDescriptions

    request  = post "DescribeConfigurationSettings"
    response = xmlResponse $ \h x -> ConfigurationSettingsDescriptions
        <$> x %| "ConfigurationSettings"

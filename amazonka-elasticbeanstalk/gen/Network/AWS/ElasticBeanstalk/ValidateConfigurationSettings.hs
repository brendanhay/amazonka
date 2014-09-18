{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a set of configuration settings and either a configuration template
-- or environment, and determines whether those values are valid. This action
-- returns a list of messages indicating any errors or warnings associated
-- with the selection of option values.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleAppVersion
-- &OptionSettings.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionSettings.member.1.OptionName=LowerThreshold
-- &OptionSettings.member.1.Value=1000000
-- &Operation=ValidateConfigurationSettings &AuthParams
-- 06f1cfff-f28f-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    (
    -- * Request
      ValidateConfigurationSettings
    -- ** Request constructor
    , validateConfigurationSettings
    -- ** Request lenses
    , vcsApplicationName
    , vcsTemplateName
    , vcsEnvironmentName
    , vcsOptionSettings

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response constructor
    , validateConfigurationSettingsResponse
    -- ** Response lenses
    , vcsrMessages
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | A list of validation messages for a specified configuration template.
data ValidateConfigurationSettings = ValidateConfigurationSettings
    { _vcsApplicationName :: Text
    , _vcsTemplateName :: Maybe Text
    , _vcsEnvironmentName :: Maybe Text
    , _vcsOptionSettings :: [ConfigurationOptionSetting]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateConfigurationSettings' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @OptionSettings ::@ @[ConfigurationOptionSetting]@
--
validateConfigurationSettings :: Text -- ^ 'vcsApplicationName'
                                -> [ConfigurationOptionSetting] -- ^ 'vcsOptionSettings'
                                -> ValidateConfigurationSettings
validateConfigurationSettings p1 p4 = ValidateConfigurationSettings
    { _vcsApplicationName = p1
    , _vcsTemplateName = Nothing
    , _vcsEnvironmentName = Nothing
    , _vcsOptionSettings = p4
    }

-- | The name of the application that the configuration template or environment
-- belongs to.
vcsApplicationName :: Lens' ValidateConfigurationSettings Text
vcsApplicationName =
    lens _vcsApplicationName (\s a -> s { _vcsApplicationName = a })

-- | The name of the configuration template to validate the settings against.
-- Condition: You cannot specify both this and an environment name.
vcsTemplateName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsTemplateName = lens _vcsTemplateName (\s a -> s { _vcsTemplateName = a })

-- | The name of the environment to validate the settings against. Condition:
-- You cannot specify both this and a configuration template name.
vcsEnvironmentName :: Lens' ValidateConfigurationSettings (Maybe Text)
vcsEnvironmentName =
    lens _vcsEnvironmentName (\s a -> s { _vcsEnvironmentName = a })

-- | A list of the options and desired values to evaluate.
vcsOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings =
    lens _vcsOptionSettings (\s a -> s { _vcsOptionSettings = a })

instance ToQuery ValidateConfigurationSettings where
    toQuery = genericQuery def

-- | Provides a list of validation messages.
newtype ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _vcsrMessages :: [ValidationMessage]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateConfigurationSettingsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Messages ::@ @[ValidationMessage]@
--
validateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse
validateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { _vcsrMessages = mempty
    }

-- | A list of ValidationMessage.
vcsrMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrMessages = lens _vcsrMessages (\s a -> s { _vcsrMessages = a })

instance FromXML ValidateConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateConfigurationSettings where
    type Sv ValidateConfigurationSettings = ElasticBeanstalk
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse

    request = post "ValidateConfigurationSettings"
    response _ = xmlResponse

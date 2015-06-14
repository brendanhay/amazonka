{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Takes a set of configuration settings and either a configuration
-- template or environment, and determines whether those values are valid.
--
-- This action returns a list of messages indicating any errors or warnings
-- associated with the selection of option values.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ValidateConfigurationSettings.html>
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    (
    -- * Request
      ValidateConfigurationSettings
    -- ** Request constructor
    , validateConfigurationSettings
    -- ** Request lenses
    , vcsApplicationName
    , vcsOptionSettings
    , vcsTemplateName
    , vcsEnvironmentName

    -- * Response
    , ValidateConfigurationSettingsResponse
    -- ** Response constructor
    , validateConfigurationSettingsResponse
    -- ** Response lenses
    , vcsrMessages
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'validateConfigurationSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsApplicationName'
--
-- * 'vcsOptionSettings'
--
-- * 'vcsTemplateName'
--
-- * 'vcsEnvironmentName'
data ValidateConfigurationSettings = ValidateConfigurationSettings'{_vcsApplicationName :: Text, _vcsOptionSettings :: [ConfigurationOptionSetting], _vcsTemplateName :: Text, _vcsEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'ValidateConfigurationSettings' smart constructor.
validateConfigurationSettings :: Text -> [ConfigurationOptionSetting] -> Text -> Text -> ValidateConfigurationSettings
validateConfigurationSettings pApplicationName pOptionSettings pTemplateName pEnvironmentName = ValidateConfigurationSettings'{_vcsApplicationName = pApplicationName, _vcsOptionSettings = pOptionSettings, _vcsTemplateName = pTemplateName, _vcsEnvironmentName = pEnvironmentName};

-- | The name of the application that the configuration template or
-- environment belongs to.
vcsApplicationName :: Lens' ValidateConfigurationSettings Text
vcsApplicationName = lens _vcsApplicationName (\ s a -> s{_vcsApplicationName = a});

-- | A list of the options and desired values to evaluate.
vcsOptionSettings :: Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
vcsOptionSettings = lens _vcsOptionSettings (\ s a -> s{_vcsOptionSettings = a});

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
vcsTemplateName :: Lens' ValidateConfigurationSettings Text
vcsTemplateName = lens _vcsTemplateName (\ s a -> s{_vcsTemplateName = a});

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template
-- name.
vcsEnvironmentName :: Lens' ValidateConfigurationSettings Text
vcsEnvironmentName = lens _vcsEnvironmentName (\ s a -> s{_vcsEnvironmentName = a});

instance AWSRequest ValidateConfigurationSettings
         where
        type Sv ValidateConfigurationSettings =
             ElasticBeanstalk
        type Rs ValidateConfigurationSettings =
             ValidateConfigurationSettingsResponse
        request = post
        response
          = receiveXMLWrapper
              "ValidateConfigurationSettingsResult"
              (\ s h x ->
                 ValidateConfigurationSettingsResponse' <$>
                   (x .@? "Messages" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders ValidateConfigurationSettings
         where
        toHeaders = const mempty

instance ToPath ValidateConfigurationSettings where
        toPath = const "/"

instance ToQuery ValidateConfigurationSettings where
        toQuery ValidateConfigurationSettings'{..}
          = mconcat
              ["Action" =:
                 ("ValidateConfigurationSettings" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationName" =: _vcsApplicationName,
               "OptionSettings" =: "member" =: _vcsOptionSettings,
               "TemplateName" =: _vcsTemplateName,
               "EnvironmentName" =: _vcsEnvironmentName]

-- | /See:/ 'validateConfigurationSettingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcsrMessages'
newtype ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'{_vcsrMessages :: [ValidationMessage]} deriving (Eq, Read, Show)

-- | 'ValidateConfigurationSettingsResponse' smart constructor.
validateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse
validateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'{_vcsrMessages = mempty};

-- | A list of ValidationMessage.
vcsrMessages :: Lens' ValidateConfigurationSettingsResponse [ValidationMessage]
vcsrMessages = lens _vcsrMessages (\ s a -> s{_vcsrMessages = a});

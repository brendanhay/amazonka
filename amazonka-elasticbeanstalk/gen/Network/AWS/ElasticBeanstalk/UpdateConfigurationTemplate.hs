{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
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

-- | Updates the specified configuration template to have the specified
-- properties or configuration option values.
--
-- If a property (for example, @ApplicationName@) is not provided, its
-- value remains unchanged. To clear such properties, specify an empty
-- string.
--
-- Related Topics
--
-- -   DescribeConfigurationOptions
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateConfigurationTemplate.html>
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    (
    -- * Request
      UpdateConfigurationTemplate
    -- ** Request constructor
    , updateConfigurationTemplate
    -- ** Request lenses
    , uctOptionsToRemove
    , uctOptionSettings
    , uctDescription
    , uctApplicationName
    , uctTemplateName

    -- * Response
    , ConfigurationSettingsDescription
    -- ** Response constructor
    , configurationSettingsDescription
    -- ** Response lenses
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    , csdTemplateName
    , csdEnvironmentName
    , csdApplicationName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'updateConfigurationTemplate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uctOptionsToRemove'
--
-- * 'uctOptionSettings'
--
-- * 'uctDescription'
--
-- * 'uctApplicationName'
--
-- * 'uctTemplateName'
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'{_uctOptionsToRemove :: [OptionSpecification], _uctOptionSettings :: [ConfigurationOptionSetting], _uctDescription :: Maybe Text, _uctApplicationName :: Text, _uctTemplateName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateConfigurationTemplate' smart constructor.
updateConfigurationTemplate :: Text -> Text -> UpdateConfigurationTemplate
updateConfigurationTemplate pApplicationName pTemplateName = UpdateConfigurationTemplate'{_uctOptionsToRemove = mempty, _uctOptionSettings = mempty, _uctDescription = Nothing, _uctApplicationName = pApplicationName, _uctTemplateName = pTemplateName};

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
uctOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctOptionsToRemove = lens _uctOptionsToRemove (\ s a -> s{_uctOptionsToRemove = a});

-- | A list of configuration option settings to update with the new specified
-- option value.
uctOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctOptionSettings = lens _uctOptionSettings (\ s a -> s{_uctOptionSettings = a});

-- | A new description for the configuration.
uctDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctDescription = lens _uctDescription (\ s a -> s{_uctDescription = a});

-- | The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
uctApplicationName :: Lens' UpdateConfigurationTemplate Text
uctApplicationName = lens _uctApplicationName (\ s a -> s{_uctApplicationName = a});

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
uctTemplateName :: Lens' UpdateConfigurationTemplate Text
uctTemplateName = lens _uctTemplateName (\ s a -> s{_uctTemplateName = a});

instance AWSRequest UpdateConfigurationTemplate where
        type Sv UpdateConfigurationTemplate =
             ElasticBeanstalk
        type Rs UpdateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = post
        response
          = receiveXMLWrapper
              "UpdateConfigurationTemplateResult"
              (\ s h x -> parseXML x)

instance ToHeaders UpdateConfigurationTemplate where
        toHeaders = const mempty

instance ToPath UpdateConfigurationTemplate where
        toPath = const "/"

instance ToQuery UpdateConfigurationTemplate where
        toQuery UpdateConfigurationTemplate'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "OptionsToRemove" =: "member" =: _uctOptionsToRemove,
               "OptionSettings" =: "member" =: _uctOptionSettings,
               "Description" =: _uctDescription,
               "ApplicationName" =: _uctApplicationName,
               "TemplateName" =: _uctTemplateName]

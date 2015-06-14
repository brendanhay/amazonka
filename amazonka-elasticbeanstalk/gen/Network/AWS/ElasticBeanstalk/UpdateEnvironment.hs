{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
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

-- | Updates the environment description, deploys a new application version,
-- updates the configuration settings to an entirely new configuration
-- template, or updates select configuration option values in the running
-- environment.
--
-- Attempting to update both the release and configuration is not allowed
-- and AWS Elastic Beanstalk returns an @InvalidParameterCombination@
-- error.
--
-- When updating the configuration settings to a new template or individual
-- settings, a draft configuration is created and
-- DescribeConfigurationSettings for this environment returns two setting
-- descriptions with different @DeploymentStatus@ values.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateEnvironment.html>
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    (
    -- * Request
      UpdateEnvironment
    -- ** Request constructor
    , updateEnvironment
    -- ** Request lenses
    , ueOptionsToRemove
    , ueOptionSettings
    , ueTier
    , ueEnvironmentId
    , ueSolutionStackName
    , ueDescription
    , ueTemplateName
    , ueVersionLabel
    , ueEnvironmentName

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , envStatus
    , envAbortableOperationInProgress
    , envEndpointURL
    , envDateUpdated
    , envResources
    , envHealth
    , envDateCreated
    , envTier
    , envEnvironmentId
    , envSolutionStackName
    , envDescription
    , envCNAME
    , envTemplateName
    , envVersionLabel
    , envEnvironmentName
    , envApplicationName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'updateEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueOptionsToRemove'
--
-- * 'ueOptionSettings'
--
-- * 'ueTier'
--
-- * 'ueEnvironmentId'
--
-- * 'ueSolutionStackName'
--
-- * 'ueDescription'
--
-- * 'ueTemplateName'
--
-- * 'ueVersionLabel'
--
-- * 'ueEnvironmentName'
data UpdateEnvironment = UpdateEnvironment'{_ueOptionsToRemove :: [OptionSpecification], _ueOptionSettings :: [ConfigurationOptionSetting], _ueTier :: Maybe EnvironmentTier, _ueEnvironmentId :: Maybe Text, _ueSolutionStackName :: Maybe Text, _ueDescription :: Maybe Text, _ueTemplateName :: Text, _ueVersionLabel :: Text, _ueEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateEnvironment' smart constructor.
updateEnvironment :: Text -> Text -> Text -> UpdateEnvironment
updateEnvironment pTemplateName pVersionLabel pEnvironmentName = UpdateEnvironment'{_ueOptionsToRemove = mempty, _ueOptionSettings = mempty, _ueTier = Nothing, _ueEnvironmentId = Nothing, _ueSolutionStackName = Nothing, _ueDescription = Nothing, _ueTemplateName = pTemplateName, _ueVersionLabel = pVersionLabel, _ueEnvironmentName = pEnvironmentName};

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
ueOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
ueOptionsToRemove = lens _ueOptionsToRemove (\ s a -> s{_ueOptionsToRemove = a});

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
ueOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
ueOptionSettings = lens _ueOptionSettings (\ s a -> s{_ueOptionSettings = a});

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type,
-- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
ueTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
ueTier = lens _ueTier (\ s a -> s{_ueTier = a});

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
ueEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentId = lens _ueEnvironmentId (\ s a -> s{_ueEnvironmentId = a});

-- | This specifies the platform version that the environment will run after
-- the environment is updated.
ueSolutionStackName :: Lens' UpdateEnvironment (Maybe Text)
ueSolutionStackName = lens _ueSolutionStackName (\ s a -> s{_ueSolutionStackName = a});

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\ s a -> s{_ueDescription = a});

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
ueTemplateName :: Lens' UpdateEnvironment Text
ueTemplateName = lens _ueTemplateName (\ s a -> s{_ueTemplateName = a});

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version
-- is found, returns an @InvalidParameterValue@ error.
ueVersionLabel :: Lens' UpdateEnvironment Text
ueVersionLabel = lens _ueVersionLabel (\ s a -> s{_ueVersionLabel = a});

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
ueEnvironmentName :: Lens' UpdateEnvironment Text
ueEnvironmentName = lens _ueEnvironmentName (\ s a -> s{_ueEnvironmentName = a});

instance AWSRequest UpdateEnvironment where
        type Sv UpdateEnvironment = ElasticBeanstalk
        type Rs UpdateEnvironment = EnvironmentDescription
        request = post
        response
          = receiveXMLWrapper "UpdateEnvironmentResult"
              (\ s h x -> parseXML x)

instance ToHeaders UpdateEnvironment where
        toHeaders = const mempty

instance ToPath UpdateEnvironment where
        toPath = const "/"

instance ToQuery UpdateEnvironment where
        toQuery UpdateEnvironment'{..}
          = mconcat
              ["Action" =: ("UpdateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "OptionsToRemove" =: "member" =: _ueOptionsToRemove,
               "OptionSettings" =: "member" =: _ueOptionSettings,
               "Tier" =: _ueTier,
               "EnvironmentId" =: _ueEnvironmentId,
               "SolutionStackName" =: _ueSolutionStackName,
               "Description" =: _ueDescription,
               "TemplateName" =: _ueTemplateName,
               "VersionLabel" =: _ueVersionLabel,
               "EnvironmentName" =: _ueEnvironmentName]

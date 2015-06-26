{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
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

-- | Launches an environment for the specified application using the
-- specified configuration.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateEnvironment.html>
module Network.AWS.ElasticBeanstalk.CreateEnvironment
    (
    -- * Request
      CreateEnvironment
    -- ** Request constructor
    , createEnvironment
    -- ** Request lenses
    , ceCNAMEPrefix
    , ceTemplateName
    , ceOptionsToRemove
    , ceOptionSettings
    , ceVersionLabel
    , ceTier
    , ceSolutionStackName
    , ceDescription
    , ceTags
    , ceApplicationName
    , ceEnvironmentName

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , envCNAME
    , envStatus
    , envTemplateName
    , envAbortableOperationInProgress
    , envEndpointURL
    , envDateUpdated
    , envResources
    , envHealth
    , envVersionLabel
    , envDateCreated
    , envTier
    , envEnvironmentName
    , envApplicationName
    , envEnvironmentId
    , envSolutionStackName
    , envDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'createEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ceCNAMEPrefix'
--
-- * 'ceTemplateName'
--
-- * 'ceOptionsToRemove'
--
-- * 'ceOptionSettings'
--
-- * 'ceVersionLabel'
--
-- * 'ceTier'
--
-- * 'ceSolutionStackName'
--
-- * 'ceDescription'
--
-- * 'ceTags'
--
-- * 'ceApplicationName'
--
-- * 'ceEnvironmentName'
data CreateEnvironment = CreateEnvironment'{_ceCNAMEPrefix :: Maybe Text, _ceTemplateName :: Maybe Text, _ceOptionsToRemove :: Maybe [OptionSpecification], _ceOptionSettings :: Maybe [ConfigurationOptionSetting], _ceVersionLabel :: Maybe Text, _ceTier :: Maybe EnvironmentTier, _ceSolutionStackName :: Maybe Text, _ceDescription :: Maybe Text, _ceTags :: Maybe [Tag], _ceApplicationName :: Text, _ceEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'CreateEnvironment' smart constructor.
createEnvironment :: Text -> Text -> CreateEnvironment
createEnvironment pApplicationName pEnvironmentName = CreateEnvironment'{_ceCNAMEPrefix = Nothing, _ceTemplateName = Nothing, _ceOptionsToRemove = Nothing, _ceOptionSettings = Nothing, _ceVersionLabel = Nothing, _ceTier = Nothing, _ceSolutionStackName = Nothing, _ceDescription = Nothing, _ceTags = Nothing, _ceApplicationName = pApplicationName, _ceEnvironmentName = pEnvironmentName};

-- | If specified, the environment attempts to use this value as the prefix
-- for the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
ceCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
ceCNAMEPrefix = lens _ceCNAMEPrefix (\ s a -> s{_ceCNAMEPrefix = a});

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this parameter or a
-- @SolutionStackName@, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
ceTemplateName :: Lens' CreateEnvironment (Maybe Text)
ceTemplateName = lens _ceTemplateName (\ s a -> s{_ceTemplateName = a});

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
ceOptionsToRemove :: Lens' CreateEnvironment [OptionSpecification]
ceOptionsToRemove = lens _ceOptionsToRemove (\ s a -> s{_ceOptionsToRemove = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
ceOptionSettings :: Lens' CreateEnvironment [ConfigurationOptionSetting]
ceOptionSettings = lens _ceOptionSettings (\ s a -> s{_ceOptionSettings = a}) . _Default;

-- | The name of the application version to deploy.
--
-- If the specified application has no associated application versions, AWS
-- Elastic Beanstalk @UpdateEnvironment@ returns an @InvalidParameterValue@
-- error.
--
-- Default: If not specified, AWS Elastic Beanstalk attempts to launch the
-- sample application in the container.
ceVersionLabel :: Lens' CreateEnvironment (Maybe Text)
ceVersionLabel = lens _ceVersionLabel (\ s a -> s{_ceVersionLabel = a});

-- | This specifies the tier to use for creating this environment.
ceTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
ceTier = lens _ceTier (\ s a -> s{_ceTier = a});

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default
-- values associated with the specified solution stack.
--
-- Condition: You must specify either this or a @TemplateName@, but not
-- both. If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns a @MissingRequiredParameter@ error.
ceSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
ceSolutionStackName = lens _ceSolutionStackName (\ s a -> s{_ceSolutionStackName = a});

-- | Describes this environment.
ceDescription :: Lens' CreateEnvironment (Maybe Text)
ceDescription = lens _ceDescription (\ s a -> s{_ceDescription = a});

-- | This specifies the tags applied to resources in the environment.
ceTags :: Lens' CreateEnvironment [Tag]
ceTags = lens _ceTags (\ s a -> s{_ceTags = a}) . _Default;

-- | The name of the application that contains the version to be deployed.
--
-- If no application is found with this name, @CreateEnvironment@ returns
-- an @InvalidParameterValue@ error.
ceApplicationName :: Lens' CreateEnvironment Text
ceApplicationName = lens _ceApplicationName (\ s a -> s{_ceApplicationName = a});

-- | A unique name for the deployment environment. Used in the application
-- URL.
--
-- Constraint: Must be from 4 to 23 characters in length. The name can
-- contain only letters, numbers, and hyphens. It cannot start or end with
-- a hyphen. This name must be unique in your account. If the specified
-- name already exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Default: If the CNAME parameter is not specified, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for
-- your application.
ceEnvironmentName :: Lens' CreateEnvironment Text
ceEnvironmentName = lens _ceEnvironmentName (\ s a -> s{_ceEnvironmentName = a});

instance AWSRequest CreateEnvironment where
        type Sv CreateEnvironment = ElasticBeanstalk
        type Rs CreateEnvironment = EnvironmentDescription
        request = post
        response
          = receiveXMLWrapper "CreateEnvironmentResult"
              (\ s h x -> parseXML x)

instance ToHeaders CreateEnvironment where
        toHeaders = const mempty

instance ToPath CreateEnvironment where
        toPath = const "/"

instance ToQuery CreateEnvironment where
        toQuery CreateEnvironment'{..}
          = mconcat
              ["Action" =: ("CreateEnvironment" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "CNAMEPrefix" =: _ceCNAMEPrefix,
               "TemplateName" =: _ceTemplateName,
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _ceOptionsToRemove),
               "OptionSettings" =:
                 toQuery (toQueryList "member" <$> _ceOptionSettings),
               "VersionLabel" =: _ceVersionLabel, "Tier" =: _ceTier,
               "SolutionStackName" =: _ceSolutionStackName,
               "Description" =: _ceDescription,
               "Tags" =: toQuery (toQueryList "member" <$> _ceTags),
               "ApplicationName" =: _ceApplicationName,
               "EnvironmentName" =: _ceEnvironmentName]

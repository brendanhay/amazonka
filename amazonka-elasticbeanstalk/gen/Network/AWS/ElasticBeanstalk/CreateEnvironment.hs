{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Launches an environment for the specified application using the
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
    , cerqCNAMEPrefix
    , cerqTemplateName
    , cerqOptionsToRemove
    , cerqOptionSettings
    , cerqVersionLabel
    , cerqTier
    , cerqSolutionStackName
    , cerqDescription
    , cerqTags
    , cerqApplicationName
    , cerqEnvironmentName

    -- * Response
    , EnvironmentDescription
    -- ** Response constructor
    , environmentDescription
    -- ** Response lenses
    , cersCNAME
    , cersStatus
    , cersTemplateName
    , cersAbortableOperationInProgress
    , cersEndpointURL
    , cersDateUpdated
    , cersResources
    , cersHealth
    , cersVersionLabel
    , cersDateCreated
    , cersTier
    , cersEnvironmentName
    , cersApplicationName
    , cersEnvironmentId
    , cersSolutionStackName
    , cersDescription
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createEnvironment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerqCNAMEPrefix'
--
-- * 'cerqTemplateName'
--
-- * 'cerqOptionsToRemove'
--
-- * 'cerqOptionSettings'
--
-- * 'cerqVersionLabel'
--
-- * 'cerqTier'
--
-- * 'cerqSolutionStackName'
--
-- * 'cerqDescription'
--
-- * 'cerqTags'
--
-- * 'cerqApplicationName'
--
-- * 'cerqEnvironmentName'
data CreateEnvironment = CreateEnvironment'
    { _cerqCNAMEPrefix       :: !(Maybe Text)
    , _cerqTemplateName      :: !(Maybe Text)
    , _cerqOptionsToRemove   :: !(Maybe [OptionSpecification])
    , _cerqOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
    , _cerqVersionLabel      :: !(Maybe Text)
    , _cerqTier              :: !(Maybe EnvironmentTier)
    , _cerqSolutionStackName :: !(Maybe Text)
    , _cerqDescription       :: !(Maybe Text)
    , _cerqTags              :: !(Maybe [Tag])
    , _cerqApplicationName   :: !Text
    , _cerqEnvironmentName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateEnvironment' smart constructor.
createEnvironment :: Text -> Text -> CreateEnvironment
createEnvironment pApplicationName pEnvironmentName =
    CreateEnvironment'
    { _cerqCNAMEPrefix = Nothing
    , _cerqTemplateName = Nothing
    , _cerqOptionsToRemove = Nothing
    , _cerqOptionSettings = Nothing
    , _cerqVersionLabel = Nothing
    , _cerqTier = Nothing
    , _cerqSolutionStackName = Nothing
    , _cerqDescription = Nothing
    , _cerqTags = Nothing
    , _cerqApplicationName = pApplicationName
    , _cerqEnvironmentName = pEnvironmentName
    }

-- | If specified, the environment attempts to use this value as the prefix
-- for the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
cerqCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
cerqCNAMEPrefix = lens _cerqCNAMEPrefix (\ s a -> s{_cerqCNAMEPrefix = a});

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this parameter or a
-- @SolutionStackName@, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
cerqTemplateName :: Lens' CreateEnvironment (Maybe Text)
cerqTemplateName = lens _cerqTemplateName (\ s a -> s{_cerqTemplateName = a});

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
cerqOptionsToRemove :: Lens' CreateEnvironment [OptionSpecification]
cerqOptionsToRemove = lens _cerqOptionsToRemove (\ s a -> s{_cerqOptionsToRemove = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack
-- or the configuration template.
cerqOptionSettings :: Lens' CreateEnvironment [ConfigurationOptionSetting]
cerqOptionSettings = lens _cerqOptionSettings (\ s a -> s{_cerqOptionSettings = a}) . _Default;

-- | The name of the application version to deploy.
--
-- If the specified application has no associated application versions, AWS
-- Elastic Beanstalk @UpdateEnvironment@ returns an @InvalidParameterValue@
-- error.
--
-- Default: If not specified, AWS Elastic Beanstalk attempts to launch the
-- sample application in the container.
cerqVersionLabel :: Lens' CreateEnvironment (Maybe Text)
cerqVersionLabel = lens _cerqVersionLabel (\ s a -> s{_cerqVersionLabel = a});

-- | This specifies the tier to use for creating this environment.
cerqTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
cerqTier = lens _cerqTier (\ s a -> s{_cerqTier = a});

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default
-- values associated with the specified solution stack.
--
-- Condition: You must specify either this or a @TemplateName@, but not
-- both. If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns a @MissingRequiredParameter@ error.
cerqSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
cerqSolutionStackName = lens _cerqSolutionStackName (\ s a -> s{_cerqSolutionStackName = a});

-- | Describes this environment.
cerqDescription :: Lens' CreateEnvironment (Maybe Text)
cerqDescription = lens _cerqDescription (\ s a -> s{_cerqDescription = a});

-- | This specifies the tags applied to resources in the environment.
cerqTags :: Lens' CreateEnvironment [Tag]
cerqTags = lens _cerqTags (\ s a -> s{_cerqTags = a}) . _Default;

-- | The name of the application that contains the version to be deployed.
--
-- If no application is found with this name, @CreateEnvironment@ returns
-- an @InvalidParameterValue@ error.
cerqApplicationName :: Lens' CreateEnvironment Text
cerqApplicationName = lens _cerqApplicationName (\ s a -> s{_cerqApplicationName = a});

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
cerqEnvironmentName :: Lens' CreateEnvironment Text
cerqEnvironmentName = lens _cerqEnvironmentName (\ s a -> s{_cerqEnvironmentName = a});

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
               "CNAMEPrefix" =: _cerqCNAMEPrefix,
               "TemplateName" =: _cerqTemplateName,
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _cerqOptionsToRemove),
               "OptionSettings" =:
                 toQuery
                   (toQueryList "member" <$> _cerqOptionSettings),
               "VersionLabel" =: _cerqVersionLabel,
               "Tier" =: _cerqTier,
               "SolutionStackName" =: _cerqSolutionStackName,
               "Description" =: _cerqDescription,
               "Tags" =:
                 toQuery (toQueryList "member" <$> _cerqTags),
               "ApplicationName" =: _cerqApplicationName,
               "EnvironmentName" =: _cerqEnvironmentName]

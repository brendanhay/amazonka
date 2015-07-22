{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration template. Templates are associated with a
-- specific application and are used to deploy different versions of the
-- application with the same configuration settings.
--
-- Related Topics
--
-- -   DescribeConfigurationOptions
-- -   DescribeConfigurationSettings
-- -   ListAvailableSolutionStacks
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateConfigurationTemplate.html>
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    (
    -- * Request
      CreateConfigurationTemplate
    -- ** Request constructor
    , createConfigurationTemplate
    -- ** Request lenses
    , cctrqOptionSettings
    , cctrqSourceConfiguration
    , cctrqEnvironmentId
    , cctrqSolutionStackName
    , cctrqDescription
    , cctrqApplicationName
    , cctrqTemplateName

    -- * Response
    , ConfigurationSettingsDescription
    -- ** Response constructor
    , configurationSettingsDescription
    -- ** Response lenses
    , csdTemplateName
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'createConfigurationTemplate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cctrqOptionSettings'
--
-- * 'cctrqSourceConfiguration'
--
-- * 'cctrqEnvironmentId'
--
-- * 'cctrqSolutionStackName'
--
-- * 'cctrqDescription'
--
-- * 'cctrqApplicationName'
--
-- * 'cctrqTemplateName'
data CreateConfigurationTemplate = CreateConfigurationTemplate'
    { _cctrqOptionSettings      :: !(Maybe [ConfigurationOptionSetting])
    , _cctrqSourceConfiguration :: !(Maybe SourceConfiguration)
    , _cctrqEnvironmentId       :: !(Maybe Text)
    , _cctrqSolutionStackName   :: !(Maybe Text)
    , _cctrqDescription         :: !(Maybe Text)
    , _cctrqApplicationName     :: !Text
    , _cctrqTemplateName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateConfigurationTemplate' smart constructor.
createConfigurationTemplate :: Text -> Text -> CreateConfigurationTemplate
createConfigurationTemplate pApplicationName_ pTemplateName_ =
    CreateConfigurationTemplate'
    { _cctrqOptionSettings = Nothing
    , _cctrqSourceConfiguration = Nothing
    , _cctrqEnvironmentId = Nothing
    , _cctrqSolutionStackName = Nothing
    , _cctrqDescription = Nothing
    , _cctrqApplicationName = pApplicationName_
    , _cctrqTemplateName = pTemplateName_
    }

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- option to the requested value. The new value overrides the value
-- obtained from the solution stack or the source configuration template.
cctrqOptionSettings :: Lens' CreateConfigurationTemplate [ConfigurationOptionSetting]
cctrqOptionSettings = lens _cctrqOptionSettings (\ s a -> s{_cctrqOptionSettings = a}) . _Default;

-- | If specified, AWS Elastic Beanstalk uses the configuration values from
-- the specified configuration template to create a new configuration.
--
-- Values specified in the @OptionSettings@ parameter of this call
-- overrides any values obtained from the @SourceConfiguration@.
--
-- If no configuration template is found, returns an
-- @InvalidParameterValue@ error.
--
-- Constraint: If both the solution stack name parameter and the source
-- configuration parameters are specified, the solution stack of the source
-- configuration template must match the specified solution stack name or
-- else AWS Elastic Beanstalk returns an @InvalidParameterCombination@
-- error.
cctrqSourceConfiguration :: Lens' CreateConfigurationTemplate (Maybe SourceConfiguration)
cctrqSourceConfiguration = lens _cctrqSourceConfiguration (\ s a -> s{_cctrqSourceConfiguration = a});

-- | The ID of the environment used with this configuration template.
cctrqEnvironmentId :: Lens' CreateConfigurationTemplate (Maybe Text)
cctrqEnvironmentId = lens _cctrqEnvironmentId (\ s a -> s{_cctrqEnvironmentId = a});

-- | The name of the solution stack used by this configuration. The solution
-- stack specifies the operating system, architecture, and application
-- server for a configuration template. It determines the set of
-- configuration options as well as the possible and default values.
--
-- Use ListAvailableSolutionStacks to obtain a list of available solution
-- stacks.
--
-- A solution stack name or a source configuration parameter must be
-- specified, otherwise AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- If a solution stack name is not specified and the source configuration
-- parameter is specified, AWS Elastic Beanstalk uses the same solution
-- stack as the source configuration template.
cctrqSolutionStackName :: Lens' CreateConfigurationTemplate (Maybe Text)
cctrqSolutionStackName = lens _cctrqSolutionStackName (\ s a -> s{_cctrqSolutionStackName = a});

-- | Describes this configuration.
cctrqDescription :: Lens' CreateConfigurationTemplate (Maybe Text)
cctrqDescription = lens _cctrqDescription (\ s a -> s{_cctrqDescription = a});

-- | The name of the application to associate with this configuration
-- template. If no application is found with this name, AWS Elastic
-- Beanstalk returns an @InvalidParameterValue@ error.
cctrqApplicationName :: Lens' CreateConfigurationTemplate Text
cctrqApplicationName = lens _cctrqApplicationName (\ s a -> s{_cctrqApplicationName = a});

-- | The name of the configuration template.
--
-- Constraint: This name must be unique per application.
--
-- Default: If a configuration template already exists with this name, AWS
-- Elastic Beanstalk returns an @InvalidParameterValue@ error.
cctrqTemplateName :: Lens' CreateConfigurationTemplate Text
cctrqTemplateName = lens _cctrqTemplateName (\ s a -> s{_cctrqTemplateName = a});

instance AWSRequest CreateConfigurationTemplate where
        type Sv CreateConfigurationTemplate =
             ElasticBeanstalk
        type Rs CreateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = post
        response
          = receiveXMLWrapper
              "CreateConfigurationTemplateResult"
              (\ s h x -> parseXML x)

instance ToHeaders CreateConfigurationTemplate where
        toHeaders = const mempty

instance ToPath CreateConfigurationTemplate where
        toPath = const "/"

instance ToQuery CreateConfigurationTemplate where
        toQuery CreateConfigurationTemplate'{..}
          = mconcat
              ["Action" =:
                 ("CreateConfigurationTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "OptionSettings" =:
                 toQuery
                   (toQueryList "member" <$> _cctrqOptionSettings),
               "SourceConfiguration" =: _cctrqSourceConfiguration,
               "EnvironmentId" =: _cctrqEnvironmentId,
               "SolutionStackName" =: _cctrqSolutionStackName,
               "Description" =: _cctrqDescription,
               "ApplicationName" =: _cctrqApplicationName,
               "TemplateName" =: _cctrqTemplateName]

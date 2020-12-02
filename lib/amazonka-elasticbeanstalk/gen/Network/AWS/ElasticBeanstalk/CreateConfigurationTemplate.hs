{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration template. Templates are associated with a specific application and are used to deploy different versions of the application with the same configuration settings.
--
--
-- Related Topics
--
--     * 'DescribeConfigurationOptions'
--
--     * 'DescribeConfigurationSettings'
--
--     * 'ListAvailableSolutionStacks'
--
--
--
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    (
    -- * Creating a Request
      createConfigurationTemplate
    , CreateConfigurationTemplate
    -- * Request Lenses
    , cctOptionSettings
    , cctPlatformARN
    , cctSourceConfiguration
    , cctSolutionStackName
    , cctEnvironmentId
    , cctDescription
    , cctApplicationName
    , cctTemplateName

    -- * Destructuring the Response
    , configurationSettingsDescription
    , ConfigurationSettingsDescription
    -- * Response Lenses
    , csdTemplateName
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdPlatformARN
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to create a configuration template.
--
--
--
-- /See:/ 'createConfigurationTemplate' smart constructor.
data CreateConfigurationTemplate = CreateConfigurationTemplate'
  { _cctOptionSettings      :: !(Maybe [ConfigurationOptionSetting])
  , _cctPlatformARN         :: !(Maybe Text)
  , _cctSourceConfiguration :: !(Maybe SourceConfiguration)
  , _cctSolutionStackName   :: !(Maybe Text)
  , _cctEnvironmentId       :: !(Maybe Text)
  , _cctDescription         :: !(Maybe Text)
  , _cctApplicationName     :: !Text
  , _cctTemplateName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctOptionSettings' - If specified, AWS Elastic Beanstalk sets the specified configuration option to the requested value. The new value overrides the value obtained from the solution stack or the source configuration template.
--
-- * 'cctPlatformARN' - The ARN of the custom platform.
--
-- * 'cctSourceConfiguration' - If specified, AWS Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration. Values specified in the @OptionSettings@ parameter of this call overrides any values obtained from the @SourceConfiguration@ .  If no configuration template is found, returns an @InvalidParameterValue@ error.  Constraint: If both the solution stack name parameter and the source configuration parameters are specified, the solution stack of the source configuration template must match the specified solution stack name or else AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error.
--
-- * 'cctSolutionStackName' - The name of the solution stack used by this configuration. The solution stack specifies the operating system, architecture, and application server for a configuration template. It determines the set of configuration options as well as the possible and default values. Use 'ListAvailableSolutionStacks' to obtain a list of available solution stacks.  A solution stack name or a source configuration parameter must be specified, otherwise AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  If a solution stack name is not specified and the source configuration parameter is specified, AWS Elastic Beanstalk uses the same solution stack as the source configuration template.
--
-- * 'cctEnvironmentId' - The ID of the environment used with this configuration template.
--
-- * 'cctDescription' - Describes this configuration.
--
-- * 'cctApplicationName' - The name of the application to associate with this configuration template. If no application is found with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- * 'cctTemplateName' - The name of the configuration template. Constraint: This name must be unique per application. Default: If a configuration template already exists with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
createConfigurationTemplate
    :: Text -- ^ 'cctApplicationName'
    -> Text -- ^ 'cctTemplateName'
    -> CreateConfigurationTemplate
createConfigurationTemplate pApplicationName_ pTemplateName_ =
  CreateConfigurationTemplate'
    { _cctOptionSettings = Nothing
    , _cctPlatformARN = Nothing
    , _cctSourceConfiguration = Nothing
    , _cctSolutionStackName = Nothing
    , _cctEnvironmentId = Nothing
    , _cctDescription = Nothing
    , _cctApplicationName = pApplicationName_
    , _cctTemplateName = pTemplateName_
    }


-- | If specified, AWS Elastic Beanstalk sets the specified configuration option to the requested value. The new value overrides the value obtained from the solution stack or the source configuration template.
cctOptionSettings :: Lens' CreateConfigurationTemplate [ConfigurationOptionSetting]
cctOptionSettings = lens _cctOptionSettings (\ s a -> s{_cctOptionSettings = a}) . _Default . _Coerce

-- | The ARN of the custom platform.
cctPlatformARN :: Lens' CreateConfigurationTemplate (Maybe Text)
cctPlatformARN = lens _cctPlatformARN (\ s a -> s{_cctPlatformARN = a})

-- | If specified, AWS Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration. Values specified in the @OptionSettings@ parameter of this call overrides any values obtained from the @SourceConfiguration@ .  If no configuration template is found, returns an @InvalidParameterValue@ error.  Constraint: If both the solution stack name parameter and the source configuration parameters are specified, the solution stack of the source configuration template must match the specified solution stack name or else AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error.
cctSourceConfiguration :: Lens' CreateConfigurationTemplate (Maybe SourceConfiguration)
cctSourceConfiguration = lens _cctSourceConfiguration (\ s a -> s{_cctSourceConfiguration = a})

-- | The name of the solution stack used by this configuration. The solution stack specifies the operating system, architecture, and application server for a configuration template. It determines the set of configuration options as well as the possible and default values. Use 'ListAvailableSolutionStacks' to obtain a list of available solution stacks.  A solution stack name or a source configuration parameter must be specified, otherwise AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.  If a solution stack name is not specified and the source configuration parameter is specified, AWS Elastic Beanstalk uses the same solution stack as the source configuration template.
cctSolutionStackName :: Lens' CreateConfigurationTemplate (Maybe Text)
cctSolutionStackName = lens _cctSolutionStackName (\ s a -> s{_cctSolutionStackName = a})

-- | The ID of the environment used with this configuration template.
cctEnvironmentId :: Lens' CreateConfigurationTemplate (Maybe Text)
cctEnvironmentId = lens _cctEnvironmentId (\ s a -> s{_cctEnvironmentId = a})

-- | Describes this configuration.
cctDescription :: Lens' CreateConfigurationTemplate (Maybe Text)
cctDescription = lens _cctDescription (\ s a -> s{_cctDescription = a})

-- | The name of the application to associate with this configuration template. If no application is found with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
cctApplicationName :: Lens' CreateConfigurationTemplate Text
cctApplicationName = lens _cctApplicationName (\ s a -> s{_cctApplicationName = a})

-- | The name of the configuration template. Constraint: This name must be unique per application. Default: If a configuration template already exists with this name, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
cctTemplateName :: Lens' CreateConfigurationTemplate Text
cctTemplateName = lens _cctTemplateName (\ s a -> s{_cctTemplateName = a})

instance AWSRequest CreateConfigurationTemplate where
        type Rs CreateConfigurationTemplate =
             ConfigurationSettingsDescription
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "CreateConfigurationTemplateResult"
              (\ s h x -> parseXML x)

instance Hashable CreateConfigurationTemplate where

instance NFData CreateConfigurationTemplate where

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
                   (toQueryList "member" <$> _cctOptionSettings),
               "PlatformArn" =: _cctPlatformARN,
               "SourceConfiguration" =: _cctSourceConfiguration,
               "SolutionStackName" =: _cctSolutionStackName,
               "EnvironmentId" =: _cctEnvironmentId,
               "Description" =: _cctDescription,
               "ApplicationName" =: _cctApplicationName,
               "TemplateName" =: _cctTemplateName]

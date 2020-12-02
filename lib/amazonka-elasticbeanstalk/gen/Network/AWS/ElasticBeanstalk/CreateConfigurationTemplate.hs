{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Elastic Beanstalk configuration template, associated with a specific Elastic Beanstalk application. You define application configuration settings in a configuration template. You can then use the configuration template to deploy different versions of the application with the same configuration settings.
--
--
-- Templates aren't associated with any environment. The @EnvironmentName@ response element is always @null@ .
--
-- Related Topics
--
--     * 'DescribeConfigurationOptions'
--
--     * 'DescribeConfigurationSettings'
--
--     * 'ListAvailableSolutionStacks'
module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
  ( -- * Creating a Request
    createConfigurationTemplate,
    CreateConfigurationTemplate,

    -- * Request Lenses
    cctOptionSettings,
    cctPlatformARN,
    cctSourceConfiguration,
    cctSolutionStackName,
    cctEnvironmentId,
    cctDescription,
    cctTags,
    cctApplicationName,
    cctTemplateName,

    -- * Destructuring the Response
    configurationSettingsDescription,
    ConfigurationSettingsDescription,

    -- * Response Lenses
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
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
  { _cctOptionSettings ::
      !( Maybe
           [ConfigurationOptionSetting]
       ),
    _cctPlatformARN :: !(Maybe Text),
    _cctSourceConfiguration ::
      !(Maybe SourceConfiguration),
    _cctSolutionStackName ::
      !(Maybe Text),
    _cctEnvironmentId :: !(Maybe Text),
    _cctDescription :: !(Maybe Text),
    _cctTags :: !(Maybe [Tag]),
    _cctApplicationName :: !Text,
    _cctTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateConfigurationTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctOptionSettings' - Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- * 'cctPlatformARN' - The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
--
-- * 'cctSourceConfiguration' - An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration. Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ . You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ . Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
--
-- * 'cctSolutionStackName' - The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ . You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ . Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
--
-- * 'cctEnvironmentId' - The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
--
-- * 'cctDescription' - An optional description for this configuration.
--
-- * 'cctTags' - Specifies the tags applied to the configuration template.
--
-- * 'cctApplicationName' - The name of the Elastic Beanstalk application to associate with this configuration template.
--
-- * 'cctTemplateName' - The name of the configuration template. Constraint: This name must be unique per application.
createConfigurationTemplate ::
  -- | 'cctApplicationName'
  Text ->
  -- | 'cctTemplateName'
  Text ->
  CreateConfigurationTemplate
createConfigurationTemplate pApplicationName_ pTemplateName_ =
  CreateConfigurationTemplate'
    { _cctOptionSettings = Nothing,
      _cctPlatformARN = Nothing,
      _cctSourceConfiguration = Nothing,
      _cctSolutionStackName = Nothing,
      _cctEnvironmentId = Nothing,
      _cctDescription = Nothing,
      _cctTags = Nothing,
      _cctApplicationName = pApplicationName_,
      _cctTemplateName = pTemplateName_
    }

-- | Option values for the Elastic Beanstalk configuration, such as the instance type. If specified, these values override the values obtained from the solution stack or the source configuration template. For a complete list of Elastic Beanstalk configuration options, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
cctOptionSettings :: Lens' CreateConfigurationTemplate [ConfigurationOptionSetting]
cctOptionSettings = lens _cctOptionSettings (\s a -> s {_cctOptionSettings = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the custom platform. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms> in the /AWS Elastic Beanstalk Developer Guide/ .
cctPlatformARN :: Lens' CreateConfigurationTemplate (Maybe Text)
cctPlatformARN = lens _cctPlatformARN (\s a -> s {_cctPlatformARN = a})

-- | An Elastic Beanstalk configuration template to base this one on. If specified, Elastic Beanstalk uses the configuration values from the specified configuration template to create a new configuration. Values specified in @OptionSettings@ override any values obtained from the @SourceConfiguration@ . You must specify @SourceConfiguration@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SolutionStackName@ . Constraint: If both solution stack name and source configuration are specified, the solution stack of the source configuration template must match the specified solution stack name.
cctSourceConfiguration :: Lens' CreateConfigurationTemplate (Maybe SourceConfiguration)
cctSourceConfiguration = lens _cctSourceConfiguration (\s a -> s {_cctSourceConfiguration = a})

-- | The name of an Elastic Beanstalk solution stack (platform version) that this configuration uses. For example, @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@ . A solution stack specifies the operating system, runtime, and application server for a configuration template. It also determines the set of configuration options as well as the possible and default values. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms> in the /AWS Elastic Beanstalk Developer Guide/ . You must specify @SolutionStackName@ if you don't specify @PlatformArn@ , @EnvironmentId@ , or @SourceConfiguration@ . Use the <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html @ListAvailableSolutionStacks@ > API to obtain a list of available solution stacks.
cctSolutionStackName :: Lens' CreateConfigurationTemplate (Maybe Text)
cctSolutionStackName = lens _cctSolutionStackName (\s a -> s {_cctSolutionStackName = a})

-- | The ID of an environment whose settings you want to use to create the configuration template. You must specify @EnvironmentId@ if you don't specify @PlatformArn@ , @SolutionStackName@ , or @SourceConfiguration@ .
cctEnvironmentId :: Lens' CreateConfigurationTemplate (Maybe Text)
cctEnvironmentId = lens _cctEnvironmentId (\s a -> s {_cctEnvironmentId = a})

-- | An optional description for this configuration.
cctDescription :: Lens' CreateConfigurationTemplate (Maybe Text)
cctDescription = lens _cctDescription (\s a -> s {_cctDescription = a})

-- | Specifies the tags applied to the configuration template.
cctTags :: Lens' CreateConfigurationTemplate [Tag]
cctTags = lens _cctTags (\s a -> s {_cctTags = a}) . _Default . _Coerce

-- | The name of the Elastic Beanstalk application to associate with this configuration template.
cctApplicationName :: Lens' CreateConfigurationTemplate Text
cctApplicationName = lens _cctApplicationName (\s a -> s {_cctApplicationName = a})

-- | The name of the configuration template. Constraint: This name must be unique per application.
cctTemplateName :: Lens' CreateConfigurationTemplate Text
cctTemplateName = lens _cctTemplateName (\s a -> s {_cctTemplateName = a})

instance AWSRequest CreateConfigurationTemplate where
  type
    Rs CreateConfigurationTemplate =
      ConfigurationSettingsDescription
  request = postQuery elasticBeanstalk
  response =
    receiveXMLWrapper
      "CreateConfigurationTemplateResult"
      (\s h x -> parseXML x)

instance Hashable CreateConfigurationTemplate

instance NFData CreateConfigurationTemplate

instance ToHeaders CreateConfigurationTemplate where
  toHeaders = const mempty

instance ToPath CreateConfigurationTemplate where
  toPath = const "/"

instance ToQuery CreateConfigurationTemplate where
  toQuery CreateConfigurationTemplate' {..} =
    mconcat
      [ "Action" =: ("CreateConfigurationTemplate" :: ByteString),
        "Version" =: ("2010-12-01" :: ByteString),
        "OptionSettings"
          =: toQuery (toQueryList "member" <$> _cctOptionSettings),
        "PlatformArn" =: _cctPlatformARN,
        "SourceConfiguration" =: _cctSourceConfiguration,
        "SolutionStackName" =: _cctSolutionStackName,
        "EnvironmentId" =: _cctEnvironmentId,
        "Description" =: _cctDescription,
        "Tags" =: toQuery (toQueryList "member" <$> _cctTags),
        "ApplicationName" =: _cctApplicationName,
        "TemplateName" =: _cctTemplateName
      ]

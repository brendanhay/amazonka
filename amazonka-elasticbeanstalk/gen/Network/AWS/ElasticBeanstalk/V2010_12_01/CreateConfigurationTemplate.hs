{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a configuration template. Templates are associated with a specific
-- application and are used to deploy different versions of the application
-- with the same configuration settings. Related Topics
-- DescribeConfigurationOptions DescribeConfigurationSettings
-- ListAvailableSolutionStacks
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=AppTemplate
-- &SolutionStackName=32bit%20Amazon%20Linux%20running%20Tomcat%207
-- &Description=ConfigTemplateDescription
-- &Operation=CreateConfigurationTemplate &AuthParams 32bit Amazon Linux
-- running Tomcat 7 ImageId ami-f2f0069b aws:autoscaling:launchconfiguration
-- Notification Endpoint aws:elasticbeanstalk:sns:topics PARAM4
-- aws:elasticbeanstalk:application:environment JDBC_CONNECTION_STRING
-- aws:elasticbeanstalk:application:environment SecurityGroups
-- elasticbeanstalk-default aws:autoscaling:launchconfiguration
-- UnhealthyThreshold 5 aws:elb:healthcheck InstanceType t1.micro
-- aws:autoscaling:launchconfiguration Statistic Average
-- aws:autoscaling:trigger LoadBalancerHTTPSPort OFF aws:elb:loadbalancer
-- Stickiness Cookie Expiration 0 aws:elb:policies PARAM5
-- aws:elasticbeanstalk:application:environment MeasureName NetworkOut
-- aws:autoscaling:trigger Interval 30 aws:elb:healthcheck Application
-- Healthcheck URL / aws:elasticbeanstalk:application Notification Topic ARN
-- aws:elasticbeanstalk:sns:topics LowerBreachScaleIncrement -1
-- aws:autoscaling:trigger XX:MaxPermSize 64m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions UpperBreachScaleIncrement
-- 1 aws:autoscaling:trigger MinSize 1 aws:autoscaling:asg Custom Availability
-- Zones us-east-1a aws:autoscaling:asg Availability Zones Any 1
-- aws:autoscaling:asg LogPublicationControl false
-- aws:elasticbeanstalk:hostmanager JVM Options
-- aws:elasticbeanstalk:container:tomcat:jvmoptions Notification Topic Name
-- aws:elasticbeanstalk:sns:topics PARAM2
-- aws:elasticbeanstalk:application:environment LoadBalancerHTTPPort 80
-- aws:elb:loadbalancer Timeout 5 aws:elb:healthcheck BreachDuration 2
-- aws:autoscaling:trigger MonitoringInterval 5 minute
-- aws:autoscaling:launchconfiguration PARAM1
-- aws:elasticbeanstalk:application:environment MaxSize 4 aws:autoscaling:asg
-- LowerThreshold 2000000 aws:autoscaling:trigger AWS_SECRET_KEY
-- aws:elasticbeanstalk:application:environment AWS_ACCESS_KEY_ID
-- aws:elasticbeanstalk:application:environment UpperThreshold 6000000
-- aws:autoscaling:trigger Notification Protocol email
-- aws:elasticbeanstalk:sns:topics Unit Bytes aws:autoscaling:trigger Xmx 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions Cooldown 360
-- aws:autoscaling:asg Period 1 aws:autoscaling:trigger Xms 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions EC2KeyName
-- aws:autoscaling:launchconfiguration Stickiness Policy false
-- aws:elb:policies PARAM3 aws:elasticbeanstalk:application:environment
-- HealthyThreshold 3 aws:elb:healthcheck SSLCertificateId
-- aws:elb:loadbalancer ConfigTemplateDescription SampleApp
-- 2010-11-17T03:48:19.640Z AppTemplate 2010-11-17T03:48:19.640Z
-- 846cd905-f1fd-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate
    (
    -- * Request
      CreateConfigurationTemplate
    -- ** Request constructor
    , mkCreateConfigurationTemplateMessage
    -- ** Request lenses
    , cctmApplicationName
    , cctmTemplateName
    , cctmSolutionStackName
    , cctmSourceConfiguration
    , cctmEnvironmentId
    , cctmDescription
    , cctmOptionSettings

    -- * Response
    , CreateConfigurationTemplateResponse
    -- ** Response lenses
    , csdSolutionStackName
    , csdApplicationName
    , csdTemplateName
    , csdDescription
    , csdEnvironmentName
    , csdDeploymentStatus
    , csdDateCreated
    , csdDateUpdated
    , csdOptionSettings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateConfigurationTemplate' request.
mkCreateConfigurationTemplateMessage :: Text -- ^ 'cctmApplicationName'
                                     -> Text -- ^ 'cctmTemplateName'
                                     -> CreateConfigurationTemplate
mkCreateConfigurationTemplateMessage p1 p2 = CreateConfigurationTemplate
    { _cctmApplicationName = p1
    , _cctmTemplateName = p2
    , _cctmSolutionStackName = Nothing
    , _cctmSourceConfiguration = Nothing
    , _cctmEnvironmentId = Nothing
    , _cctmDescription = Nothing
    , _cctmOptionSettings = mempty
    }
{-# INLINE mkCreateConfigurationTemplateMessage #-}

data CreateConfigurationTemplate = CreateConfigurationTemplate
    { _cctmApplicationName :: Text
      -- ^ The name of the application to associate with this configuration
      -- template. If no application is found with this name, AWS Elastic
      -- Beanstalk returns an InvalidParameterValue error.
    , _cctmTemplateName :: Text
      -- ^ The name of the configuration template. Constraint: This name
      -- must be unique per application. Default: If a configuration
      -- template already exists with this name, AWS Elastic Beanstalk
      -- returns an InvalidParameterValue error.
    , _cctmSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack used by this configuration. The
      -- solution stack specifies the operating system, architecture, and
      -- application server for a configuration template. It determines
      -- the set of configuration options as well as the possible and
      -- default values. Use ListAvailableSolutionStacks to obtain a list
      -- of available solution stacks. A solution stack name or a source
      -- configuration parameter must be specified, otherwise AWS Elastic
      -- Beanstalk returns an InvalidParameterValue error. If a solution
      -- stack name is not specified and the source configuration
      -- parameter is specified, AWS Elastic Beanstalk uses the same
      -- solution stack as the source configuration template.
    , _cctmSourceConfiguration :: Maybe SourceConfiguration
      -- ^ If specified, AWS Elastic Beanstalk uses the configuration values
      -- from the specified configuration template to create a new
      -- configuration. Values specified in the OptionSettings parameter
      -- of this call overrides any values obtained from the
      -- SourceConfiguration. If no configuration template is found,
      -- returns an InvalidParameterValue error. Constraint: If both the
      -- solution stack name parameter and the source configuration
      -- parameters are specified, the solution stack of the source
      -- configuration template must match the specified solution stack
      -- name or else AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error.
    , _cctmEnvironmentId :: Maybe Text
      -- ^ The ID of the environment used with this configuration template.
    , _cctmDescription :: Maybe Text
      -- ^ Describes this configuration.
    , _cctmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk sets the specified
      -- configuration option to the requested value. The new value
      -- overrides the value obtained from the solution stack or the
      -- source configuration template.
    } deriving (Show, Generic)

-- | The name of the application to associate with this configuration template.
-- If no application is found with this name, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cctmApplicationName :: Lens' CreateConfigurationTemplate (Text)
cctmApplicationName = lens _cctmApplicationName (\s a -> s { _cctmApplicationName = a })
{-# INLINE cctmApplicationName #-}

-- | The name of the configuration template. Constraint: This name must be
-- unique per application. Default: If a configuration template already exists
-- with this name, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
cctmTemplateName :: Lens' CreateConfigurationTemplate (Text)
cctmTemplateName = lens _cctmTemplateName (\s a -> s { _cctmTemplateName = a })
{-# INLINE cctmTemplateName #-}

-- | The name of the solution stack used by this configuration. The solution
-- stack specifies the operating system, architecture, and application server
-- for a configuration template. It determines the set of configuration
-- options as well as the possible and default values. Use
-- ListAvailableSolutionStacks to obtain a list of available solution stacks.
-- A solution stack name or a source configuration parameter must be
-- specified, otherwise AWS Elastic Beanstalk returns an InvalidParameterValue
-- error. If a solution stack name is not specified and the source
-- configuration parameter is specified, AWS Elastic Beanstalk uses the same
-- solution stack as the source configuration template.
cctmSolutionStackName :: Lens' CreateConfigurationTemplate (Maybe Text)
cctmSolutionStackName = lens _cctmSolutionStackName (\s a -> s { _cctmSolutionStackName = a })
{-# INLINE cctmSolutionStackName #-}

-- | If specified, AWS Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration. Values
-- specified in the OptionSettings parameter of this call overrides any values
-- obtained from the SourceConfiguration. If no configuration template is
-- found, returns an InvalidParameterValue error. Constraint: If both the
-- solution stack name parameter and the source configuration parameters are
-- specified, the solution stack of the source configuration template must
-- match the specified solution stack name or else AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
cctmSourceConfiguration :: Lens' CreateConfigurationTemplate (Maybe SourceConfiguration)
cctmSourceConfiguration = lens _cctmSourceConfiguration (\s a -> s { _cctmSourceConfiguration = a })
{-# INLINE cctmSourceConfiguration #-}

-- | The ID of the environment used with this configuration template.
cctmEnvironmentId :: Lens' CreateConfigurationTemplate (Maybe Text)
cctmEnvironmentId = lens _cctmEnvironmentId (\s a -> s { _cctmEnvironmentId = a })
{-# INLINE cctmEnvironmentId #-}

-- | Describes this configuration.
cctmDescription :: Lens' CreateConfigurationTemplate (Maybe Text)
cctmDescription = lens _cctmDescription (\s a -> s { _cctmDescription = a })
{-# INLINE cctmDescription #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration option
-- to the requested value. The new value overrides the value obtained from the
-- solution stack or the source configuration template.
cctmOptionSettings :: Lens' CreateConfigurationTemplate ([ConfigurationOptionSetting])
cctmOptionSettings = lens _cctmOptionSettings (\s a -> s { _cctmOptionSettings = a })
{-# INLINE cctmOptionSettings #-}

instance ToQuery CreateConfigurationTemplate where
    toQuery = genericQuery def

data CreateConfigurationTemplateResponse = CreateConfigurationTemplateResponse
    { _csdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , _csdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration
      -- set.
    , _csdTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this
      -- configuration set.
    , _csdDescription :: Maybe Text
      -- ^ Describes this configuration set.
    , _csdEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration
      -- set.
    , _csdDeploymentStatus :: Maybe ConfigurationDeploymentStatus
      -- ^ If this configuration set is associated with an environment, the
      -- DeploymentStatus parameter indicates the deployment status of
      -- this configuration set: null: This configuration is not
      -- associated with a running environment. pending: This is a draft
      -- configuration that is not deployed to the associated environment
      -- but is in the process of deploying. deployed: This is the
      -- configuration that is currently deployed to the associated
      -- running environment. failed: This is a draft configuration, that
      -- failed to successfully deploy. null: This configuration is not
      -- associated with a running environment. pending: This is a draft
      -- configuration that is not deployed to the associated environment
      -- but is in the process of deploying. deployed: This is the
      -- configuration that is currently deployed to the associated
      -- running environment. failed: This is a draft configuration that
      -- failed to successfully deploy.
    , _csdDateCreated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was created.
    , _csdDateUpdated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was last
      -- modified.
    , _csdOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this
      -- configuration set.
    } deriving (Show, Generic)

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
csdSolutionStackName = lens _csdSolutionStackName (\s a -> s { _csdSolutionStackName = a })
{-# INLINE csdSolutionStackName #-}

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
csdApplicationName = lens _csdApplicationName (\s a -> s { _csdApplicationName = a })
{-# INLINE csdApplicationName #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csdTemplateName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
csdTemplateName = lens _csdTemplateName (\s a -> s { _csdTemplateName = a })
{-# INLINE csdTemplateName #-}

-- | Describes this configuration set.
csdDescription :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
csdDescription = lens _csdDescription (\s a -> s { _csdDescription = a })
{-# INLINE csdDescription #-}

-- | If not null, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' CreateConfigurationTemplateResponse (Maybe Text)
csdEnvironmentName = lens _csdEnvironmentName (\s a -> s { _csdEnvironmentName = a })
{-# INLINE csdEnvironmentName #-}

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of deploying.
-- deployed: This is the configuration that is currently deployed to the
-- associated running environment. failed: This is a draft configuration, that
-- failed to successfully deploy. null: This configuration is not associated
-- with a running environment. pending: This is a draft configuration that is
-- not deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration that failed to successfully deploy.
csdDeploymentStatus :: Lens' CreateConfigurationTemplateResponse (Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus = lens _csdDeploymentStatus (\s a -> s { _csdDeploymentStatus = a })
{-# INLINE csdDeploymentStatus #-}

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' CreateConfigurationTemplateResponse (Maybe ISO8601)
csdDateCreated = lens _csdDateCreated (\s a -> s { _csdDateCreated = a })
{-# INLINE csdDateCreated #-}

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' CreateConfigurationTemplateResponse (Maybe ISO8601)
csdDateUpdated = lens _csdDateUpdated (\s a -> s { _csdDateUpdated = a })
{-# INLINE csdDateUpdated #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csdOptionSettings :: Lens' CreateConfigurationTemplateResponse ([ConfigurationOptionSetting])
csdOptionSettings = lens _csdOptionSettings (\s a -> s { _csdOptionSettings = a })
{-# INLINE csdOptionSettings #-}

instance FromXML CreateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateConfigurationTemplate where
    type Sv CreateConfigurationTemplate = ElasticBeanstalk
    type Rs CreateConfigurationTemplate = CreateConfigurationTemplateResponse

    request = post "CreateConfigurationTemplate"
    response _ = xmlResponse

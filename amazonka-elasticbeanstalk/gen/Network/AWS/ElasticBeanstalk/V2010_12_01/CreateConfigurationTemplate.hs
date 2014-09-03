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
    , createConfigurationTemplate
    -- ** Request lenses
    , cctmApplicationName
    , cctmTemplateName
    , cctmOptionSettings
    , cctmDescription
    , cctmEnvironmentId
    , cctmSolutionStackName
    , cctmSourceConfiguration

    -- * Response
    , CreateConfigurationTemplateResponse
    -- ** Response lenses
    , csdApplicationName
    , csdDeploymentStatus
    , csdOptionSettings
    , csdTemplateName
    , csdDateCreated
    , csdDescription
    , csdEnvironmentName
    , csdSolutionStackName
    , csdDateUpdated
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateConfigurationTemplate' request.
createConfigurationTemplate :: Text -- ^ 'cctmApplicationName'
                            -> Text -- ^ 'cctmTemplateName'
                            -> CreateConfigurationTemplate
createConfigurationTemplate p1 p2 = CreateConfigurationTemplate
    { _cctmApplicationName = p1
    , _cctmTemplateName = p2
    , _cctmOptionSettings = mempty
    , _cctmDescription = Nothing
    , _cctmEnvironmentId = Nothing
    , _cctmSolutionStackName = Nothing
    , _cctmSourceConfiguration = Nothing
    }

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
    , _cctmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk sets the specified
      -- configuration option to the requested value. The new value
      -- overrides the value obtained from the solution stack or the
      -- source configuration template.
    , _cctmDescription :: Maybe Text
      -- ^ Describes this configuration.
    , _cctmEnvironmentId :: Maybe Text
      -- ^ The ID of the environment used with this configuration template.
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
    } deriving (Show, Generic)

-- | The name of the application to associate with this configuration template.
-- If no application is found with this name, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error.
cctmApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmApplicationName f x =
    (\y -> x { _cctmApplicationName = y })
       <$> f (_cctmApplicationName x)
{-# INLINE cctmApplicationName #-}

-- | The name of the configuration template. Constraint: This name must be
-- unique per application. Default: If a configuration template already exists
-- with this name, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
cctmTemplateName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmTemplateName f x =
    (\y -> x { _cctmTemplateName = y })
       <$> f (_cctmTemplateName x)
{-# INLINE cctmTemplateName #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration option
-- to the requested value. The new value overrides the value obtained from the
-- solution stack or the source configuration template.
cctmOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmOptionSettings f x =
    (\y -> x { _cctmOptionSettings = y })
       <$> f (_cctmOptionSettings x)
{-# INLINE cctmOptionSettings #-}

-- | Describes this configuration.
cctmDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmDescription f x =
    (\y -> x { _cctmDescription = y })
       <$> f (_cctmDescription x)
{-# INLINE cctmDescription #-}

-- | The ID of the environment used with this configuration template.
cctmEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmEnvironmentId f x =
    (\y -> x { _cctmEnvironmentId = y })
       <$> f (_cctmEnvironmentId x)
{-# INLINE cctmEnvironmentId #-}

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
cctmSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmSolutionStackName f x =
    (\y -> x { _cctmSolutionStackName = y })
       <$> f (_cctmSolutionStackName x)
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
cctmSourceConfiguration
    :: Functor f
    => (Maybe SourceConfiguration
    -> f (Maybe SourceConfiguration))
    -> CreateConfigurationTemplate
    -> f CreateConfigurationTemplate
cctmSourceConfiguration f x =
    (\y -> x { _cctmSourceConfiguration = y })
       <$> f (_cctmSourceConfiguration x)
{-# INLINE cctmSourceConfiguration #-}

instance ToQuery CreateConfigurationTemplate where
    toQuery = genericQuery def

data CreateConfigurationTemplateResponse = CreateConfigurationTemplateResponse
    { _csdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration
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
    , _csdOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this
      -- configuration set.
    , _csdTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this
      -- configuration set.
    , _csdDateCreated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was created.
    , _csdDescription :: Maybe Text
      -- ^ Describes this configuration set.
    , _csdEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration
      -- set.
    , _csdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , _csdDateUpdated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was last
      -- modified.
    } deriving (Show, Generic)

-- | The name of the application associated with this configuration set.
csdApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdApplicationName f x =
    (\y -> x { _csdApplicationName = y })
       <$> f (_csdApplicationName x)
{-# INLINE csdApplicationName #-}

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
csdDeploymentStatus
    :: Functor f
    => (Maybe ConfigurationDeploymentStatus
    -> f (Maybe ConfigurationDeploymentStatus))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdDeploymentStatus f x =
    (\y -> x { _csdDeploymentStatus = y })
       <$> f (_csdDeploymentStatus x)
{-# INLINE csdDeploymentStatus #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csdOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdOptionSettings f x =
    (\y -> x { _csdOptionSettings = y })
       <$> f (_csdOptionSettings x)
{-# INLINE csdOptionSettings #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csdTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdTemplateName f x =
    (\y -> x { _csdTemplateName = y })
       <$> f (_csdTemplateName x)
{-# INLINE csdTemplateName #-}

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdDateCreated f x =
    (\y -> x { _csdDateCreated = y })
       <$> f (_csdDateCreated x)
{-# INLINE csdDateCreated #-}

-- | Describes this configuration set.
csdDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdDescription f x =
    (\y -> x { _csdDescription = y })
       <$> f (_csdDescription x)
{-# INLINE csdDescription #-}

-- | If not null, the name of the environment for this configuration set.
csdEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdEnvironmentName f x =
    (\y -> x { _csdEnvironmentName = y })
       <$> f (_csdEnvironmentName x)
{-# INLINE csdEnvironmentName #-}

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdSolutionStackName f x =
    (\y -> x { _csdSolutionStackName = y })
       <$> f (_csdSolutionStackName x)
{-# INLINE csdSolutionStackName #-}

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CreateConfigurationTemplateResponse
    -> f CreateConfigurationTemplateResponse
csdDateUpdated f x =
    (\y -> x { _csdDateUpdated = y })
       <$> f (_csdDateUpdated x)
{-# INLINE csdDateUpdated #-}

instance FromXML CreateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateConfigurationTemplate where
    type Sv CreateConfigurationTemplate = ElasticBeanstalk
    type Rs CreateConfigurationTemplate = CreateConfigurationTemplateResponse

    request = post "CreateConfigurationTemplate"
    response _ = xmlResponse

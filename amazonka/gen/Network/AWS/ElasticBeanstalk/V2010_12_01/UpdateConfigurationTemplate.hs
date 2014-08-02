{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified configuration template to have the specified
-- properties or configuration option values. If a property (for example,
-- ApplicationName) is not provided, its value remains unchanged. To clear
-- such properties, specify an empty string. Related Topics
-- DescribeConfigurationOptions
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=default &Description=changed%20description
-- &OptionSettings.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionSettings.member.1.OptionName=LowerThreshold
-- &OptionSettings.member.1.Value=1000000
-- &Operation=UpdateConfigurationTemplate &AuthParams 32bit Amazon Linux
-- running Tomcat 7 Availability Zones Any 1 aws:autoscaling:asg PARAM5
-- aws:elasticbeanstalk:application:environment LowerThreshold 1000000
-- aws:autoscaling:trigger UpperThreshold 9000000 aws:autoscaling:trigger
-- LowerBreachScaleIncrement -1 aws:autoscaling:trigger MeasureName NetworkOut
-- aws:autoscaling:trigger Period 60 aws:autoscaling:trigger Xmx 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions PARAM3
-- aws:elasticbeanstalk:application:environment EC2KeyName
-- aws:autoscaling:launchconfiguration MinSize 1 aws:autoscaling:asg JVM
-- Options aws:elasticbeanstalk:container:tomcat:jvmoptions XX:MaxPermSize 64m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions AWS_SECRET_KEY
-- aws:elasticbeanstalk:application:environment UpperBreachScaleIncrement 1
-- aws:autoscaling:trigger Notification Topic ARN
-- aws:elasticbeanstalk:sns:topics InstanceType t1.micro
-- aws:autoscaling:launchconfiguration Custom Availability Zones us-east-1a
-- aws:autoscaling:asg Statistic Average aws:autoscaling:trigger Notification
-- Protocol email aws:elasticbeanstalk:sns:topics JDBC_CONNECTION_STRING
-- aws:elasticbeanstalk:application:environment PARAM2
-- aws:elasticbeanstalk:application:environment Stickiness Cookie Expiration 0
-- aws:elb:policies SSLCertificateId aws:elb:loadbalancer MaxSize 4
-- aws:autoscaling:asg Stickiness Policy false aws:elb:policies Notification
-- Topic Name aws:elasticbeanstalk:sns:topics SecurityGroups
-- elasticbeanstalk-default aws:autoscaling:launchconfiguration
-- LoadBalancerHTTPPort 80 aws:elb:loadbalancer Unit None
-- aws:autoscaling:trigger AWS_ACCESS_KEY_ID
-- aws:elasticbeanstalk:application:environment PARAM4
-- aws:elasticbeanstalk:application:environment Application Healthcheck URL /
-- aws:elasticbeanstalk:application LoadBalancerHTTPSPort OFF
-- aws:elb:loadbalancer HealthyThreshold 3 aws:elb:healthcheck Timeout 5
-- aws:elb:healthcheck Cooldown 0 aws:autoscaling:asg UnhealthyThreshold 5
-- aws:elb:healthcheck Interval 30 aws:elb:healthcheck LogPublicationControl
-- false aws:elasticbeanstalk:hostmanager BreachDuration 120
-- aws:autoscaling:trigger PARAM1 aws:elasticbeanstalk:application:environment
-- Notification Endpoint aws:elasticbeanstalk:sns:topics Protocol HTTP
-- aws:elb:loadbalancer Xms 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions changed description
-- SampleApp 2010-11-17T19:26:20.420Z Default 2010-11-17T20:58:27.508Z
-- 6cbcb09a-f28d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateConfigurationTemplate' request.
updateConfigurationTemplate :: Text -- ^ '_uctmApplicationName'
                            -> Text -- ^ '_uctmTemplateName'
                            -> UpdateConfigurationTemplate
updateConfigurationTemplate p1 p2 = UpdateConfigurationTemplate
    { _uctmApplicationName = p1
    , _uctmTemplateName = p2
    , _uctmOptionSettings = mempty
    , _uctmDescription = Nothing
    , _uctmOptionsToRemove = mempty
    }

data UpdateConfigurationTemplate = UpdateConfigurationTemplate
    { _uctmApplicationName :: Text
      -- ^ The name of the application associated with the configuration
      -- template to update. If no application is found with this name,
      -- UpdateConfigurationTemplate returns an InvalidParameterValue
      -- error.
    , _uctmTemplateName :: Text
      -- ^ The name of the configuration template to update. If no
      -- configuration template is found with this name,
      -- UpdateConfigurationTemplate returns an InvalidParameterValue
      -- error.
    , _uctmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of configuration option settings to update with the new
      -- specified option value.
    , _uctmDescription :: Maybe Text
      -- ^ A new description for the configuration.
    , _uctmOptionsToRemove :: [OptionSpecification]
      -- ^ A list of configuration options to remove from the configuration
      -- set. Constraint: You can remove only UserDefined configuration
      -- options.
    } deriving (Generic)

makeLenses ''UpdateConfigurationTemplate

instance ToQuery UpdateConfigurationTemplate where
    toQuery = genericToQuery def

data UpdateConfigurationTemplateResponse = UpdateConfigurationTemplateResponse
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
    } deriving (Generic)

makeLenses ''UpdateConfigurationTemplateResponse

instance FromXML UpdateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateConfigurationTemplate where
    type Sv UpdateConfigurationTemplate = ElasticBeanstalk
    type Rs UpdateConfigurationTemplate = UpdateConfigurationTemplateResponse

    request = post "UpdateConfigurationTemplate"
    response _ = xmlResponse

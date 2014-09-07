{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate
    (
    -- * Request
      UpdateConfigurationTemplate
    -- ** Request constructor
    , mkUpdateConfigurationTemplate
    -- ** Request lenses
    , uctApplicationName
    , uctTemplateName
    , uctDescription
    , uctOptionSettings
    , uctOptionsToRemove

    -- * Response
    , UpdateConfigurationTemplateResponse
    -- ** Response lenses
    , uctrsSolutionStackName
    , uctrsApplicationName
    , uctrsTemplateName
    , uctrsDescription
    , uctrsEnvironmentName
    , uctrsDeploymentStatus
    , uctrsDateCreated
    , uctrsDateUpdated
    , uctrsOptionSettings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | The result message containing the options for the specified solution stack.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate
    { _uctApplicationName :: Text
    , _uctTemplateName :: Text
    , _uctDescription :: Maybe Text
    , _uctOptionSettings :: [ConfigurationOptionSetting]
    , _uctOptionsToRemove :: [OptionSpecification]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateConfigurationTemplate' request.
mkUpdateConfigurationTemplate :: Text -- ^ 'uctApplicationName'
                              -> Text -- ^ 'uctTemplateName'
                              -> UpdateConfigurationTemplate
mkUpdateConfigurationTemplate p1 p2 = UpdateConfigurationTemplate
    { _uctApplicationName = p1
    , _uctTemplateName = p2
    , _uctDescription = Nothing
    , _uctOptionSettings = mempty
    , _uctOptionsToRemove = mempty
    }

-- | The name of the application associated with the configuration template to
-- update. If no application is found with this name,
-- UpdateConfigurationTemplate returns an InvalidParameterValue error.
uctApplicationName :: Lens' UpdateConfigurationTemplate Text
uctApplicationName =
    lens _uctApplicationName (\s a -> s { _uctApplicationName = a })

-- | The name of the configuration template to update. If no configuration
-- template is found with this name, UpdateConfigurationTemplate returns an
-- InvalidParameterValue error.
uctTemplateName :: Lens' UpdateConfigurationTemplate Text
uctTemplateName = lens _uctTemplateName (\s a -> s { _uctTemplateName = a })

-- | A new description for the configuration.
uctDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctDescription = lens _uctDescription (\s a -> s { _uctDescription = a })

-- | A list of configuration option settings to update with the new specified
-- option value.
uctOptionSettings :: Lens' UpdateConfigurationTemplate [ConfigurationOptionSetting]
uctOptionSettings =
    lens _uctOptionSettings (\s a -> s { _uctOptionSettings = a })

-- | A list of configuration options to remove from the configuration set.
-- Constraint: You can remove only UserDefined configuration options.
uctOptionsToRemove :: Lens' UpdateConfigurationTemplate [OptionSpecification]
uctOptionsToRemove =
    lens _uctOptionsToRemove (\s a -> s { _uctOptionsToRemove = a })

instance ToQuery UpdateConfigurationTemplate where
    toQuery = genericQuery def

-- | Describes the settings for a configuration set.
data UpdateConfigurationTemplateResponse = UpdateConfigurationTemplateResponse
    { _uctrsSolutionStackName :: Maybe Text
    , _uctrsApplicationName :: Maybe Text
    , _uctrsTemplateName :: Maybe Text
    , _uctrsDescription :: Maybe Text
    , _uctrsEnvironmentName :: Maybe Text
    , _uctrsDeploymentStatus :: Maybe ConfigurationDeploymentStatus
    , _uctrsDateCreated :: Maybe ISO8601
    , _uctrsDateUpdated :: Maybe ISO8601
    , _uctrsOptionSettings :: [ConfigurationOptionSetting]
    } deriving (Show, Generic)

-- | The name of the solution stack this configuration set uses.
uctrsSolutionStackName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrsSolutionStackName =
    lens _uctrsSolutionStackName (\s a -> s { _uctrsSolutionStackName = a })

-- | The name of the application associated with this configuration set.
uctrsApplicationName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrsApplicationName =
    lens _uctrsApplicationName (\s a -> s { _uctrsApplicationName = a })

-- | If not null, the name of the configuration template for this configuration
-- set.
uctrsTemplateName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrsTemplateName =
    lens _uctrsTemplateName (\s a -> s { _uctrsTemplateName = a })

-- | Describes this configuration set.
uctrsDescription :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrsDescription =
    lens _uctrsDescription (\s a -> s { _uctrsDescription = a })

-- | If not null, the name of the environment for this configuration set.
uctrsEnvironmentName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
uctrsEnvironmentName =
    lens _uctrsEnvironmentName (\s a -> s { _uctrsEnvironmentName = a })

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
uctrsDeploymentStatus :: Lens' UpdateConfigurationTemplateResponse (Maybe ConfigurationDeploymentStatus)
uctrsDeploymentStatus =
    lens _uctrsDeploymentStatus (\s a -> s { _uctrsDeploymentStatus = a })

-- | The date (in UTC time) when this configuration set was created.
uctrsDateCreated :: Lens' UpdateConfigurationTemplateResponse (Maybe ISO8601)
uctrsDateCreated =
    lens _uctrsDateCreated (\s a -> s { _uctrsDateCreated = a })

-- | The date (in UTC time) when this configuration set was last modified.
uctrsDateUpdated :: Lens' UpdateConfigurationTemplateResponse (Maybe ISO8601)
uctrsDateUpdated =
    lens _uctrsDateUpdated (\s a -> s { _uctrsDateUpdated = a })

-- | A list of the configuration options and their values in this configuration
-- set.
uctrsOptionSettings :: Lens' UpdateConfigurationTemplateResponse [ConfigurationOptionSetting]
uctrsOptionSettings =
    lens _uctrsOptionSettings (\s a -> s { _uctrsOptionSettings = a })

instance FromXML UpdateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateConfigurationTemplate where
    type Sv UpdateConfigurationTemplate = ElasticBeanstalk
    type Rs UpdateConfigurationTemplate = UpdateConfigurationTemplateResponse

    request = post "UpdateConfigurationTemplate"
    response _ = xmlResponse

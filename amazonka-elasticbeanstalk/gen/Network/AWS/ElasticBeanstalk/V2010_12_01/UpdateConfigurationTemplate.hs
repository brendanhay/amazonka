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
    , mkUpdateConfigurationTemplateMessage
    -- ** Request lenses
    , uctmApplicationName
    , uctmTemplateName
    , uctmDescription
    , uctmOptionSettings
    , uctmOptionsToRemove

    -- * Response
    , UpdateConfigurationTemplateResponse
    -- ** Response lenses
    , csgSolutionStackName
    , csgApplicationName
    , csgTemplateName
    , csgDescription
    , csgEnvironmentName
    , csgDeploymentStatus
    , csgDateCreated
    , csgDateUpdated
    , csgOptionSettings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateConfigurationTemplate' request.
mkUpdateConfigurationTemplateMessage :: Text -- ^ 'uctmApplicationName'
                                     -> Text -- ^ 'uctmTemplateName'
                                     -> UpdateConfigurationTemplate
mkUpdateConfigurationTemplateMessage p1 p2 = UpdateConfigurationTemplate
    { _uctmApplicationName = p1
    , _uctmTemplateName = p2
    , _uctmDescription = Nothing
    , _uctmOptionSettings = mempty
    , _uctmOptionsToRemove = mempty
    }
{-# INLINE mkUpdateConfigurationTemplateMessage #-}

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
    , _uctmDescription :: Maybe Text
      -- ^ A new description for the configuration.
    , _uctmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of configuration option settings to update with the new
      -- specified option value.
    , _uctmOptionsToRemove :: [OptionSpecification]
      -- ^ A list of configuration options to remove from the configuration
      -- set. Constraint: You can remove only UserDefined configuration
      -- options.
    } deriving (Show, Generic)

-- | The name of the application associated with the configuration template to
-- update. If no application is found with this name,
-- UpdateConfigurationTemplate returns an InvalidParameterValue error.
uctmApplicationName :: Lens' UpdateConfigurationTemplate (Text)
uctmApplicationName = lens _uctmApplicationName (\s a -> s { _uctmApplicationName = a })
{-# INLINE uctmApplicationName #-}

-- | The name of the configuration template to update. If no configuration
-- template is found with this name, UpdateConfigurationTemplate returns an
-- InvalidParameterValue error.
uctmTemplateName :: Lens' UpdateConfigurationTemplate (Text)
uctmTemplateName = lens _uctmTemplateName (\s a -> s { _uctmTemplateName = a })
{-# INLINE uctmTemplateName #-}

-- | A new description for the configuration.
uctmDescription :: Lens' UpdateConfigurationTemplate (Maybe Text)
uctmDescription = lens _uctmDescription (\s a -> s { _uctmDescription = a })
{-# INLINE uctmDescription #-}

-- | A list of configuration option settings to update with the new specified
-- option value.
uctmOptionSettings :: Lens' UpdateConfigurationTemplate ([ConfigurationOptionSetting])
uctmOptionSettings = lens _uctmOptionSettings (\s a -> s { _uctmOptionSettings = a })
{-# INLINE uctmOptionSettings #-}

-- | A list of configuration options to remove from the configuration set.
-- Constraint: You can remove only UserDefined configuration options.
uctmOptionsToRemove :: Lens' UpdateConfigurationTemplate ([OptionSpecification])
uctmOptionsToRemove = lens _uctmOptionsToRemove (\s a -> s { _uctmOptionsToRemove = a })
{-# INLINE uctmOptionsToRemove #-}

instance ToQuery UpdateConfigurationTemplate where
    toQuery = genericQuery def

data UpdateConfigurationTemplateResponse = UpdateConfigurationTemplateResponse
    { _csgSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , _csgApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration
      -- set.
    , _csgTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this
      -- configuration set.
    , _csgDescription :: Maybe Text
      -- ^ Describes this configuration set.
    , _csgEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration
      -- set.
    , _csgDeploymentStatus :: Maybe ConfigurationDeploymentStatus
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
    , _csgDateCreated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was created.
    , _csgDateUpdated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was last
      -- modified.
    , _csgOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this
      -- configuration set.
    } deriving (Show, Generic)

-- | The name of the solution stack this configuration set uses.
csgSolutionStackName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
csgSolutionStackName = lens _csgSolutionStackName (\s a -> s { _csgSolutionStackName = a })
{-# INLINE csgSolutionStackName #-}

-- | The name of the application associated with this configuration set.
csgApplicationName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
csgApplicationName = lens _csgApplicationName (\s a -> s { _csgApplicationName = a })
{-# INLINE csgApplicationName #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csgTemplateName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
csgTemplateName = lens _csgTemplateName (\s a -> s { _csgTemplateName = a })
{-# INLINE csgTemplateName #-}

-- | Describes this configuration set.
csgDescription :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })
{-# INLINE csgDescription #-}

-- | If not null, the name of the environment for this configuration set.
csgEnvironmentName :: Lens' UpdateConfigurationTemplateResponse (Maybe Text)
csgEnvironmentName = lens _csgEnvironmentName (\s a -> s { _csgEnvironmentName = a })
{-# INLINE csgEnvironmentName #-}

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
csgDeploymentStatus :: Lens' UpdateConfigurationTemplateResponse (Maybe ConfigurationDeploymentStatus)
csgDeploymentStatus = lens _csgDeploymentStatus (\s a -> s { _csgDeploymentStatus = a })
{-# INLINE csgDeploymentStatus #-}

-- | The date (in UTC time) when this configuration set was created.
csgDateCreated :: Lens' UpdateConfigurationTemplateResponse (Maybe ISO8601)
csgDateCreated = lens _csgDateCreated (\s a -> s { _csgDateCreated = a })
{-# INLINE csgDateCreated #-}

-- | The date (in UTC time) when this configuration set was last modified.
csgDateUpdated :: Lens' UpdateConfigurationTemplateResponse (Maybe ISO8601)
csgDateUpdated = lens _csgDateUpdated (\s a -> s { _csgDateUpdated = a })
{-# INLINE csgDateUpdated #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csgOptionSettings :: Lens' UpdateConfigurationTemplateResponse ([ConfigurationOptionSetting])
csgOptionSettings = lens _csgOptionSettings (\s a -> s { _csgOptionSettings = a })
{-# INLINE csgOptionSettings #-}

instance FromXML UpdateConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateConfigurationTemplate where
    type Sv UpdateConfigurationTemplate = ElasticBeanstalk
    type Rs UpdateConfigurationTemplate = UpdateConfigurationTemplateResponse

    request = post "UpdateConfigurationTemplate"
    response _ = xmlResponse

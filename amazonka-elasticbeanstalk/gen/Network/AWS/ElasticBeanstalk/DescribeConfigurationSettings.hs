{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the settings for the specified configuration set,
-- that is, either a configuration template or the configuration set
-- associated with a running environment. When describing the settings for the
-- configuration set associated with a running environment, it is possible to
-- receive two sets of setting descriptions. One is the deployed configuration
-- set, and the other is a draft configuration of an environment that is
-- either in the process of deployment or that failed to deploy. Related
-- Topics DeleteEnvironmentConfiguration
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=default &Operation=DescribeConfigurationSettings &AuthParams
-- 32bit Amazon Linux running Tomcat 7 32bit Amazon Linux running Tomcat 7
-- ImageId ami-f2f0069b aws:autoscaling:launchconfiguration Notification
-- Endpoint aws:elasticbeanstalk:sns:topics PARAM4
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
-- aws:elb:loadbalancer Default Configuration Template SampleApp
-- 2010-11-17T03:20:17.832Z Default 2010-11-17T03:20:17.832Z
-- 4bde8884-f273-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    (
    -- * Request
      DescribeConfigurationSettings
    -- ** Request constructor
    , describeConfigurationSettings
    -- ** Request lenses
    , dcsApplicationName
    , dcsTemplateName
    , dcsEnvironmentName

    -- * Response
    , DescribeConfigurationSettingsResponse
    -- ** Response constructor
    , describeConfigurationSettingsResponse
    -- ** Response lenses
    , dcsrConfigurationSettings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | Result message containing all of the configuration settings for a specified
-- solution stack or configuration template.
data DescribeConfigurationSettings = DescribeConfigurationSettings
    { _dcsApplicationName :: Text
    , _dcsTemplateName :: Maybe Text
    , _dcsEnvironmentName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConfigurationSettings' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
describeConfigurationSettings :: Text -- ^ 'dcsApplicationName'
                              -> DescribeConfigurationSettings
describeConfigurationSettings p1 = DescribeConfigurationSettings
    { _dcsApplicationName = p1
    , _dcsTemplateName = Nothing
    , _dcsEnvironmentName = Nothing
    }

-- | The application for the environment or configuration template.
dcsApplicationName :: Lens' DescribeConfigurationSettings Text
dcsApplicationName =
    lens _dcsApplicationName (\s a -> s { _dcsApplicationName = a })

-- | The name of the configuration template to describe. Conditional: You must
-- specify either this parameter or an EnvironmentName, but not both. If you
-- specify both, AWS Elastic Beanstalk returns an InvalidParameterCombination
-- error. If you do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
dcsTemplateName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsTemplateName = lens _dcsTemplateName (\s a -> s { _dcsTemplateName = a })

-- | The name of the environment to describe. Condition: You must specify either
-- this or a TemplateName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an InvalidParameterCombination error. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
dcsEnvironmentName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsEnvironmentName =
    lens _dcsEnvironmentName (\s a -> s { _dcsEnvironmentName = a })

instance ToQuery DescribeConfigurationSettings where
    toQuery = genericQuery def

-- | The results from a request to change the configuration settings of an
-- environment.
newtype DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse
    { _dcsrConfigurationSettings :: [ConfigurationSettingsDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConfigurationSettingsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConfigurationSettings ::@ @[ConfigurationSettingsDescription]@
--
describeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse
describeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse
    { _dcsrConfigurationSettings = mempty
    }

-- | A list of ConfigurationSettingsDescription.
dcsrConfigurationSettings :: Lens' DescribeConfigurationSettingsResponse [ConfigurationSettingsDescription]
dcsrConfigurationSettings =
    lens _dcsrConfigurationSettings
         (\s a -> s { _dcsrConfigurationSettings = a })

instance FromXML DescribeConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConfigurationSettings where
    type Sv DescribeConfigurationSettings = ElasticBeanstalk
    type Rs DescribeConfigurationSettings = DescribeConfigurationSettingsResponse

    request = post "DescribeConfigurationSettings"
    response _ = xmlResponse

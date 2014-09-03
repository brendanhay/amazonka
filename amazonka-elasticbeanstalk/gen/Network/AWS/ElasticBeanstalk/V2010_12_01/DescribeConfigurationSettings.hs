{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings
    (
    -- * Request
      DescribeConfigurationSettings
    -- ** Request constructor
    , describeConfigurationSettings
    -- ** Request lenses
    , dcsmApplicationName
    , dcsmTemplateName
    , dcsmEnvironmentName

    -- * Response
    , DescribeConfigurationSettingsResponse
    -- ** Response lenses
    , cseConfigurationSettings
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeConfigurationSettings' request.
describeConfigurationSettings :: Text -- ^ 'dcsmApplicationName'
                              -> DescribeConfigurationSettings
describeConfigurationSettings p1 = DescribeConfigurationSettings
    { _dcsmApplicationName = p1
    , _dcsmTemplateName = Nothing
    , _dcsmEnvironmentName = Nothing
    }

data DescribeConfigurationSettings = DescribeConfigurationSettings
    { _dcsmApplicationName :: Text
      -- ^ The application for the environment or configuration template.
    , _dcsmTemplateName :: Maybe Text
      -- ^ The name of the configuration template to describe. Conditional:
      -- You must specify either this parameter or an EnvironmentName, but
      -- not both. If you specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns a MissingRequiredParameter error.
    , _dcsmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to describe. Condition: You must
      -- specify either this or a TemplateName, but not both. If you
      -- specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns MissingRequiredParameter error.
    } deriving (Show, Generic)

-- | The application for the environment or configuration template.
dcsmApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeConfigurationSettings
    -> f DescribeConfigurationSettings
dcsmApplicationName f x =
    (\y -> x { _dcsmApplicationName = y })
       <$> f (_dcsmApplicationName x)
{-# INLINE dcsmApplicationName #-}

-- | The name of the configuration template to describe. Conditional: You must
-- specify either this parameter or an EnvironmentName, but not both. If you
-- specify both, AWS Elastic Beanstalk returns an InvalidParameterCombination
-- error. If you do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
dcsmTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeConfigurationSettings
    -> f DescribeConfigurationSettings
dcsmTemplateName f x =
    (\y -> x { _dcsmTemplateName = y })
       <$> f (_dcsmTemplateName x)
{-# INLINE dcsmTemplateName #-}

-- | The name of the environment to describe. Condition: You must specify either
-- this or a TemplateName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an InvalidParameterCombination error. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
dcsmEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeConfigurationSettings
    -> f DescribeConfigurationSettings
dcsmEnvironmentName f x =
    (\y -> x { _dcsmEnvironmentName = y })
       <$> f (_dcsmEnvironmentName x)
{-# INLINE dcsmEnvironmentName #-}

instance ToQuery DescribeConfigurationSettings where
    toQuery = genericQuery def

data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse
    { _cseConfigurationSettings :: [ConfigurationSettingsDescription]
      -- ^ A list of ConfigurationSettingsDescription.
    } deriving (Show, Generic)

-- | A list of ConfigurationSettingsDescription.
cseConfigurationSettings
    :: Functor f
    => ([ConfigurationSettingsDescription]
    -> f ([ConfigurationSettingsDescription]))
    -> DescribeConfigurationSettingsResponse
    -> f DescribeConfigurationSettingsResponse
cseConfigurationSettings f x =
    (\y -> x { _cseConfigurationSettings = y })
       <$> f (_cseConfigurationSettings x)
{-# INLINE cseConfigurationSettings #-}

instance FromXML DescribeConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConfigurationSettings where
    type Sv DescribeConfigurationSettings = ElasticBeanstalk
    type Rs DescribeConfigurationSettings = DescribeConfigurationSettingsResponse

    request = post "DescribeConfigurationSettings"
    response _ = xmlResponse

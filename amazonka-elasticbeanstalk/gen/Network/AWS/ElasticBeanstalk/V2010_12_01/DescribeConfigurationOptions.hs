{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution stack
-- defines. The description includes the values the options, their default
-- values, and an indication of the required action on a running environment
-- if an option value is changed.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=default &Operation=DescribeConfigurationOptions &AuthParams
-- 32bit Amazon Linux running Tomcat 7 false RestartEnvironment 2000 ImageId
-- Scalar ami-6036c009 aws:autoscaling:launchconfiguration false
-- NoInterruption 2000 Notification Endpoint Scalar
-- aws:elasticbeanstalk:sns:topics false RestartApplicationServer 2000 PARAM4
-- Scalar aws:elasticbeanstalk:application:environment false
-- RestartApplicationServer 2000 JDBC_CONNECTION_STRING Scalar
-- aws:elasticbeanstalk:application:environment false RestartEnvironment 2000
-- SecurityGroups Scalar elasticbeanstalk-default
-- aws:autoscaling:launchconfiguration false NoInterruption 2
-- UnhealthyThreshold Scalar 5 10 aws:elb:healthcheck false RestartEnvironment
-- InstanceType t1.micro m1.small Scalar t1.micro
-- aws:autoscaling:launchconfiguration false NoInterruption Statistic Minimum
-- Maximum Sum Average Scalar Average aws:autoscaling:trigger false
-- RestartEnvironment LoadBalancerHTTPSPort OFF 443 8443 5443 Scalar OFF
-- aws:elb:loadbalancer false NoInterruption 0 Stickiness Cookie Expiration
-- Scalar 0 1000000 aws:elb:policies false RestartApplicationServer 2000
-- PARAM5 Scalar aws:elasticbeanstalk:application:environment false
-- NoInterruption MeasureName CPUUtilization NetworkIn NetworkOut DiskWriteOps
-- DiskReadBytes DiskReadOps DiskWriteBytes Latency RequestCount
-- HealthyHostCount UnhealthyHostCount Scalar NetworkOut
-- aws:autoscaling:trigger false NoInterruption 5 Interval Scalar 30 300
-- aws:elb:healthcheck false NoInterruption 2000 Application Healthcheck URL
-- Scalar / aws:elasticbeanstalk:application false NoInterruption 2000
-- Notification Topic ARN Scalar aws:elasticbeanstalk:sns:topics false
-- NoInterruption 2000 LowerBreachScaleIncrement Scalar -1
-- aws:autoscaling:trigger false RestartApplicationServer 2000 ^\S*$ nospaces
-- XX:MaxPermSize Scalar 64m aws:elasticbeanstalk:container:tomcat:jvmoptions
-- false NoInterruption 2000 UpperBreachScaleIncrement Scalar 1
-- aws:autoscaling:trigger false NoInterruption 1 MinSize Scalar 1 10000
-- aws:autoscaling:asg false RestartEnvironment Custom Availability Zones
-- us-east-1a us-east-1b us-east-1c us-east-1d List us-east-1a
-- aws:autoscaling:asg false RestartEnvironment Availability Zones Any 1 Any 2
-- Scalar Any 1 aws:autoscaling:asg false NoInterruption LogPublicationControl
-- Boolean false aws:elasticbeanstalk:hostmanager false
-- RestartApplicationServer 2000 JVM Options Scalar
-- aws:elasticbeanstalk:container:tomcat:jvmoptions false NoInterruption 2000
-- Notification Topic Name Scalar aws:elasticbeanstalk:sns:topics false
-- RestartApplicationServer 2000 PARAM2 Scalar
-- aws:elasticbeanstalk:application:environment false RestartEnvironment
-- LoadBalancerHTTPPort OFF 80 8080 Scalar 80 aws:elb:loadbalancer false
-- NoInterruption 2 Timeout Scalar 5 60 aws:elb:healthcheck false
-- NoInterruption 1 BreachDuration Scalar 2 600 aws:autoscaling:trigger false
-- RestartEnvironment MonitoringInterval 1 minute 5 minute Scalar 5 minute
-- aws:autoscaling:launchconfiguration false RestartApplicationServer 2000
-- PARAM1 Scalar aws:elasticbeanstalk:application:environment false
-- NoInterruption 1 MaxSize Scalar 4 10000 aws:autoscaling:asg false
-- NoInterruption 0 LowerThreshold Scalar 2000000 20000000
-- aws:autoscaling:trigger false RestartApplicationServer 2000 AWS_SECRET_KEY
-- Scalar aws:elasticbeanstalk:application:environment false
-- RestartApplicationServer 2000 AWS_ACCESS_KEY_ID Scalar
-- aws:elasticbeanstalk:application:environment false NoInterruption 0
-- UpperThreshold Scalar 6000000 20000000 aws:autoscaling:trigger false
-- NoInterruption Notification Protocol http https email email-json sqs Scalar
-- email aws:elasticbeanstalk:sns:topics false NoInterruption Unit Seconds
-- Percent Bytes Bits Count Bytes/Second Bits/Second Count/Second None Scalar
-- Bytes aws:autoscaling:trigger false RestartApplicationServer 2000 ^\S*$
-- nospaces Xmx Scalar 256m aws:elasticbeanstalk:container:tomcat:jvmoptions
-- false NoInterruption 0 Cooldown Scalar 360 10000 aws:autoscaling:asg false
-- NoInterruption 1 Period Scalar 1 600 aws:autoscaling:trigger false
-- RestartApplicationServer 2000 ^\S*$ nospaces Xms Scalar 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions false RestartEnvironment
-- 2000 EC2KeyName Scalar aws:autoscaling:launchconfiguration false
-- NoInterruption Stickiness Policy Boolean false aws:elb:policies false
-- RestartApplicationServer 2000 PARAM3 Scalar
-- aws:elasticbeanstalk:application:environment false NoInterruption 2
-- HealthyThreshold Scalar 3 10 aws:elb:healthcheck false RestartEnvironment
-- 2000 SSLCertificateId Scalar aws:elb:loadbalancer
-- e8768900-f272-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions
    (
    -- * Request
      DescribeConfigurationOptions
    -- ** Request constructor
    , mkDescribeConfigurationOptionsMessage
    -- ** Request lenses
    , dcomApplicationName
    , dcomTemplateName
    , dcomEnvironmentName
    , dcomSolutionStackName
    , dcomOptions

    -- * Response
    , DescribeConfigurationOptionsResponse
    -- ** Response lenses
    , codSolutionStackName
    , codOptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConfigurationOptions' request.
mkDescribeConfigurationOptionsMessage :: DescribeConfigurationOptions
mkDescribeConfigurationOptionsMessage = DescribeConfigurationOptions
    { _dcomApplicationName = Nothing
    , _dcomTemplateName = Nothing
    , _dcomEnvironmentName = Nothing
    , _dcomSolutionStackName = Nothing
    , _dcomOptions = mempty
    }
{-# INLINE mkDescribeConfigurationOptionsMessage #-}

data DescribeConfigurationOptions = DescribeConfigurationOptions
    { _dcomApplicationName :: Maybe Text
      -- ^ The name of the application associated with the configuration
      -- template or environment. Only needed if you want to describe the
      -- configuration options associated with either the configuration
      -- template or environment.
    , _dcomTemplateName :: Maybe Text
      -- ^ The name of the configuration template whose configuration
      -- options you want to describe.
    , _dcomEnvironmentName :: Maybe Text
      -- ^ The name of the environment whose configuration options you want
      -- to describe.
    , _dcomSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack whose configuration options you
      -- want to describe.
    , _dcomOptions :: [OptionSpecification]
      -- ^ If specified, restricts the descriptions to only the specified
      -- options.
    } deriving (Show, Generic)

-- | The name of the application associated with the configuration template or
-- environment. Only needed if you want to describe the configuration options
-- associated with either the configuration template or environment.
dcomApplicationName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcomApplicationName = lens _dcomApplicationName (\s a -> s { _dcomApplicationName = a })
{-# INLINE dcomApplicationName #-}

-- | The name of the configuration template whose configuration options you want
-- to describe.
dcomTemplateName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcomTemplateName = lens _dcomTemplateName (\s a -> s { _dcomTemplateName = a })
{-# INLINE dcomTemplateName #-}

-- | The name of the environment whose configuration options you want to
-- describe.
dcomEnvironmentName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcomEnvironmentName = lens _dcomEnvironmentName (\s a -> s { _dcomEnvironmentName = a })
{-# INLINE dcomEnvironmentName #-}

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcomSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcomSolutionStackName = lens _dcomSolutionStackName (\s a -> s { _dcomSolutionStackName = a })
{-# INLINE dcomSolutionStackName #-}

-- | If specified, restricts the descriptions to only the specified options.
dcomOptions :: Lens' DescribeConfigurationOptions ([OptionSpecification])
dcomOptions = lens _dcomOptions (\s a -> s { _dcomOptions = a })
{-# INLINE dcomOptions #-}

instance ToQuery DescribeConfigurationOptions where
    toQuery = genericQuery def

data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse
    { _codSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack these configuration options belong
      -- to.
    , _codOptions :: [ConfigurationOptionDescription]
      -- ^ A list of ConfigurationOptionDescription.
    } deriving (Show, Generic)

-- | The name of the solution stack these configuration options belong to.
codSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
codSolutionStackName = lens _codSolutionStackName (\s a -> s { _codSolutionStackName = a })
{-# INLINE codSolutionStackName #-}

-- | A list of ConfigurationOptionDescription.
codOptions :: Lens' DescribeConfigurationOptionsResponse ([ConfigurationOptionDescription])
codOptions = lens _codOptions (\s a -> s { _codOptions = a })
{-# INLINE codOptions #-}

instance FromXML DescribeConfigurationOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConfigurationOptions where
    type Sv DescribeConfigurationOptions = ElasticBeanstalk
    type Rs DescribeConfigurationOptions = DescribeConfigurationOptionsResponse

    request = post "DescribeConfigurationOptions"
    response _ = xmlResponse

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Elastic Beanstalk is an easy-to-use service for deploying and scaling
-- web applications and services developed with Java, .NET, PHP, Node.js,
-- Python, Ruby, and Docker on familiar servers such as Apache HTTP Server,
-- Apache Tomcat, Nginx, Passenger, and IIS 7.5/8. You can simply upload your
-- code and Elastic Beanstalk automatically handles the deployment, from
-- capacity provisioning, load balancing, auto-scaling to application health
-- monitoring. At the same time, you retain full control over the AWS
-- resources powering your application and can access the underlying resources
-- at any time. There is no additional charge for Elastic Beanstalk - you pay
-- only for the AWS resources needed to store and run your applications.
--
-- This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.ElasticBeanstalk" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.ElasticBeanstalk
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.ElasticBeanstalk.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Network.AWS.ElasticBeanstalk.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
-- @
--
module Network.AWS.ElasticBeanstalk.V2010_12_01.Monadic
    (
    -- * CheckDNSAvailability
    -- $CheckDNSAvailability
      checkDNSAvailability
    , checkDNSAvailabilityCatch

    -- * CreateApplication
    -- $CreateApplication
    , createApplication
    , createApplicationCatch

    -- * CreateApplicationVersion
    -- $CreateApplicationVersion
    , createApplicationVersion
    , createApplicationVersionCatch

    -- * CreateConfigurationTemplate
    -- $CreateConfigurationTemplate
    , createConfigurationTemplate
    , createConfigurationTemplateCatch

    -- * CreateEnvironment
    -- $CreateEnvironment
    , createEnvironment
    , createEnvironmentCatch

    -- * CreateStorageLocation
    -- $CreateStorageLocation
    , createStorageLocation
    , createStorageLocationCatch

    -- * DeleteApplication
    -- $DeleteApplication
    , deleteApplication
    , deleteApplicationCatch

    -- * DeleteApplicationVersion
    -- $DeleteApplicationVersion
    , deleteApplicationVersion
    , deleteApplicationVersionCatch

    -- * DeleteConfigurationTemplate
    -- $DeleteConfigurationTemplate
    , deleteConfigurationTemplate
    , deleteConfigurationTemplateCatch

    -- * DeleteEnvironmentConfiguration
    -- $DeleteEnvironmentConfiguration
    , deleteEnvironmentConfiguration
    , deleteEnvironmentConfigurationCatch

    -- * DescribeApplicationVersions
    -- $DescribeApplicationVersions
    , describeApplicationVersions
    , describeApplicationVersionsCatch

    -- * DescribeApplications
    -- $DescribeApplications
    , describeApplications
    , describeApplicationsCatch

    -- * DescribeConfigurationOptions
    -- $DescribeConfigurationOptions
    , describeConfigurationOptions
    , describeConfigurationOptionsCatch

    -- * DescribeConfigurationSettings
    -- $DescribeConfigurationSettings
    , describeConfigurationSettings
    , describeConfigurationSettingsCatch

    -- * DescribeEnvironmentResources
    -- $DescribeEnvironmentResources
    , describeEnvironmentResources
    , describeEnvironmentResourcesCatch

    -- * DescribeEnvironments
    -- $DescribeEnvironments
    , describeEnvironments
    , describeEnvironmentsCatch

    -- * DescribeEvents
    -- $DescribeEvents
    , describeEvents
    , describeEventsCatch

    -- * ListAvailableSolutionStacks
    -- $ListAvailableSolutionStacks
    , listAvailableSolutionStacks
    , listAvailableSolutionStacksCatch

    -- * RebuildEnvironment
    -- $RebuildEnvironment
    , rebuildEnvironment
    , rebuildEnvironmentCatch

    -- * RequestEnvironmentInfo
    -- $RequestEnvironmentInfo
    , requestEnvironmentInfo
    , requestEnvironmentInfoCatch

    -- * RestartAppServer
    -- $RestartAppServer
    , restartAppServer
    , restartAppServerCatch

    -- * RetrieveEnvironmentInfo
    -- $RetrieveEnvironmentInfo
    , retrieveEnvironmentInfo
    , retrieveEnvironmentInfoCatch

    -- * SwapEnvironmentCNAMEs
    -- $SwapEnvironmentCNAMEs
    , swapEnvironmentCNAMEs
    , swapEnvironmentCNAMEsCatch

    -- * TerminateEnvironment
    -- $TerminateEnvironment
    , terminateEnvironment
    , terminateEnvironmentCatch

    -- * UpdateApplication
    -- $UpdateApplication
    , updateApplication
    , updateApplicationCatch

    -- * UpdateApplicationVersion
    -- $UpdateApplicationVersion
    , updateApplicationVersion
    , updateApplicationVersionCatch

    -- * UpdateConfigurationTemplate
    -- $UpdateConfigurationTemplate
    , updateConfigurationTemplate
    , updateConfigurationTemplateCatch

    -- * UpdateEnvironment
    -- $UpdateEnvironment
    , updateEnvironment
    , updateEnvironmentCatch

    -- * ValidateConfigurationSettings
    -- $ValidateConfigurationSettings
    , validateConfigurationSettings
    , validateConfigurationSettingsCatch

    -- * Re-exported
    , module Network.AWS.ElasticBeanstalk.V2010_12_01

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.V2010_12_01

type ServiceEr = Er ElasticBeanstalk


-- $CheckDNSAvailability
-- Checks if the specified CNAME is available.
-- https://elasticbeanstalk.us-east-1.amazon.com/?CNAMEPrefix=sampleapplication
-- &Operation=CheckDNSAvailability &AuthParams
-- sampleapplication.elasticbeanstalk.amazonaws.com true
-- 12f6701f-f1d6-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CheckDNSAvailability'

checkDNSAvailability :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'cdnsaCNAMEPrefix'
    -> State CheckDNSAvailability a
    -> m CheckDNSAvailabilityResponse
checkDNSAvailability p1 s =
    send $ (mkCheckDNSAvailability p1) &~ s

checkDNSAvailabilityCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'cdnsaCNAMEPrefix'
    -> State CheckDNSAvailability a
    -> m (Either ServiceEr CheckDNSAvailabilityResponse)
checkDNSAvailabilityCatch p1 s =
    sendCatch $ (mkCheckDNSAvailability p1) &~ s

-- $CreateApplication
-- Creates an application that has one configuration template named default
-- and no application versions. The &lt;code&gt;default&lt;/code&gt;
-- configuration template is for a 32-bit version of the Amazon Linux
-- operating system running the Tomcat 6 application container.
-- &lt;/note&gt;">
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Sample%20Description &Operation=CreateApplication &AuthParams
-- Sample Description SampleApp 2010-11-16T23:09:20.256Z
-- 2010-11-16T23:09:20.256Z Default 8b00e053-f1d6-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplication'

createApplication :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'caApplicationName'
    -> State CreateApplication a
    -> m CreateApplicationResponse
createApplication p1 s =
    send $ (mkCreateApplication p1) &~ s

createApplicationCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'caApplicationName'
    -> State CreateApplication a
    -> m (Either ServiceEr CreateApplicationResponse)
createApplicationCatch p1 s =
    sendCatch $ (mkCreateApplication p1) &~ s

-- $CreateApplicationVersion
-- Creates an application version for the specified application. Once you
-- create an application version with a specified Amazon S3 bucket and key
-- location, you cannot change that Amazon S3 location. If you change the
-- Amazon S3 location, you receive an exception when you attempt to launch an
-- environment from the application version.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=Version1 &Description=description
-- &SourceBundle.S3Bucket=amazonaws.com &SourceBundle.S3Key=sample.war
-- &AutoCreateApplication=true &Operation=CreateApplicationVersion &AuthParams
-- amazonaws.com sample.war Version1 description SampleApp
-- 2010-11-17T03:21:59.161Z 2010-11-17T03:21:59.161Z
-- d653efef-f1f9-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CreateApplicationVersion'

createApplicationVersion :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'cavApplicationName'
    -> Text -- ^ 'cavVersionLabel'
    -> State CreateApplicationVersion a
    -> m CreateApplicationVersionResponse
createApplicationVersion p1 p2 s =
    send $ (mkCreateApplicationVersion p1 p2) &~ s

createApplicationVersionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'cavApplicationName'
    -> Text -- ^ 'cavVersionLabel'
    -> State CreateApplicationVersion a
    -> m (Either ServiceEr CreateApplicationVersionResponse)
createApplicationVersionCatch p1 p2 s =
    sendCatch $ (mkCreateApplicationVersion p1 p2) &~ s

-- $CreateConfigurationTemplate
-- Creates a configuration template. Templates are associated with a specific
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
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CreateConfigurationTemplate'

createConfigurationTemplate :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cctApplicationName'
    -> Text -- ^ 'cctTemplateName'
    -> State CreateConfigurationTemplate a
    -> m CreateConfigurationTemplateResponse
createConfigurationTemplate p1 p2 s =
    send $ (mkCreateConfigurationTemplate p1 p2) &~ s

createConfigurationTemplateCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'cctApplicationName'
    -> Text -- ^ 'cctTemplateName'
    -> State CreateConfigurationTemplate a
    -> m (Either ServiceEr CreateConfigurationTemplateResponse)
createConfigurationTemplateCatch p1 p2 s =
    sendCatch $ (mkCreateConfigurationTemplate p1 p2) &~ s

-- $CreateEnvironment
-- Launches an environment for the specified application using the specified
-- configuration.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp
-- &SolutionStackName=32bit%20Amazon%20Linux%20running%20Tomcat%207
-- &Description=EnvDescrip &Operation=CreateEnvironment &AuthParams Version1
-- Deploying SampleApp Grey e-icsgecu3wf 2010-11-17T03:59:33.520Z 32bit Amazon
-- Linux running Tomcat 7 EnvDescrip SampleApp 2010-11-17T03:59:33.520Z
-- 15db925e-f1ff-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment'

createEnvironment :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'ceApplicationName'
    -> Text -- ^ 'ceEnvironmentName'
    -> State CreateEnvironment a
    -> m CreateEnvironmentResponse
createEnvironment p1 p2 s =
    send $ (mkCreateEnvironment p1 p2) &~ s

createEnvironmentCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'ceApplicationName'
    -> Text -- ^ 'ceEnvironmentName'
    -> State CreateEnvironment a
    -> m (Either ServiceEr CreateEnvironmentResponse)
createEnvironmentCatch p1 p2 s =
    sendCatch $ (mkCreateEnvironment p1 p2) &~ s

-- $CreateStorageLocation
-- Creates the Amazon S3 storage location for the account. This location is
-- used to store user log files.
-- https://elasticbeanstalk.us-east-1.amazon.com/?Operation=CreateStorageLocation
-- &AuthParams elasticbeanstalk-us-east-1-780612358023
-- ef51b94a-f1d6-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation'

createStorageLocation :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => State CreateStorageLocation a
    -> m CreateStorageLocationResponse
createStorageLocation s =
    send (mkCreateStorageLocation &~ s)

createStorageLocationCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => State CreateStorageLocation a
    -> m (Either ServiceEr CreateStorageLocationResponse)
createStorageLocationCatch s =
    sendCatch (mkCreateStorageLocation &~ s)

-- $DeleteApplication
-- Deletes the specified application along with all associated versions and
-- configurations. The application versions will not be deleted from your
-- Amazon S3 bucket. You cannot delete an application that has a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DeleteApplication &AuthParams
-- 1f155abd-f1d7-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplication'

deleteApplication :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'daApplicationName'
    -> State DeleteApplication a
    -> m DeleteApplicationResponse
deleteApplication p1 s =
    send $ (mkDeleteApplication p1) &~ s

deleteApplicationCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'daApplicationName'
    -> State DeleteApplication a
    -> m (Either ServiceEr DeleteApplicationResponse)
deleteApplicationCatch p1 s =
    sendCatch $ (mkDeleteApplication p1) &~ s

-- $DeleteApplicationVersion
-- Deletes the specified version from the specified application. You cannot
-- delete an application version that is associated with a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=First%20Release &Operation=DeleteApplicationVersion
-- &AuthParams 58dc7339-f272-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteApplicationVersion'

deleteApplicationVersion :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'davApplicationName'
    -> Text -- ^ 'davVersionLabel'
    -> State DeleteApplicationVersion a
    -> m DeleteApplicationVersionResponse
deleteApplicationVersion p1 p2 s =
    send $ (mkDeleteApplicationVersion p1 p2) &~ s

deleteApplicationVersionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'davApplicationName'
    -> Text -- ^ 'davVersionLabel'
    -> State DeleteApplicationVersion a
    -> m (Either ServiceEr DeleteApplicationVersionResponse)
deleteApplicationVersionCatch p1 p2 s =
    sendCatch $ (mkDeleteApplicationVersion p1 p2) &~ s

-- $DeleteConfigurationTemplate
-- Deletes the specified configuration template. When you launch an
-- environment using a configuration template, the environment gets a copy of
-- the template. You can delete or modify the environment's copy of the
-- template without affecting the running environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=SampleAppTemplate &Operation=DeleteConfigurationTemplate
-- &AuthParams af9cf1b6-f25e-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteConfigurationTemplate'

deleteConfigurationTemplate :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'dctApplicationName'
    -> Text -- ^ 'dctTemplateName'
    -> State DeleteConfigurationTemplate a
    -> m DeleteConfigurationTemplateResponse
deleteConfigurationTemplate p1 p2 s =
    send $ (mkDeleteConfigurationTemplate p1 p2) &~ s

deleteConfigurationTemplateCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'dctApplicationName'
    -> Text -- ^ 'dctTemplateName'
    -> State DeleteConfigurationTemplate a
    -> m (Either ServiceEr DeleteConfigurationTemplateResponse)
deleteConfigurationTemplateCatch p1 p2 s =
    sendCatch $ (mkDeleteConfigurationTemplate p1 p2) &~ s

-- $DeleteEnvironmentConfiguration
-- Deletes the draft configuration associated with the running environment.
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The DeploymentStatus for the draft configuration indicates
-- whether the deployment is in process or has failed. The draft configuration
-- remains in existence until it is deleted with this action.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp &Operation=DeleteEnvironmentConfiguration
-- &AuthParams fdf76507-f26d-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DeleteEnvironmentConfiguration'

deleteEnvironmentConfiguration :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'decApplicationName'
    -> Text -- ^ 'decEnvironmentName'
    -> State DeleteEnvironmentConfiguration a
    -> m DeleteEnvironmentConfigurationResponse
deleteEnvironmentConfiguration p1 p2 s =
    send $ (mkDeleteEnvironmentConfiguration p1 p2) &~ s

deleteEnvironmentConfigurationCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'decApplicationName'
    -> Text -- ^ 'decEnvironmentName'
    -> State DeleteEnvironmentConfiguration a
    -> m (Either ServiceEr DeleteEnvironmentConfigurationResponse)
deleteEnvironmentConfigurationCatch p1 p2 s =
    sendCatch $ (mkDeleteEnvironmentConfiguration p1 p2) &~ s

-- $DescribeApplicationVersions
-- Returns descriptions for existing application versions.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DescribeApplicationVersions &AuthParams amazonaws.com sample.war
-- Version1 description SampleApp 2010-11-17T03:21:59.161Z
-- 2010-11-17T03:21:59.161Z 773cd80a-f26c-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplicationVersions'

describeApplicationVersions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => State DescribeApplicationVersions a
    -> m DescribeApplicationVersionsResponse
describeApplicationVersions s =
    send (mkDescribeApplicationVersions &~ s)

describeApplicationVersionsCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => State DescribeApplicationVersions a
    -> m (Either ServiceEr DescribeApplicationVersionsResponse)
describeApplicationVersionsCatch s =
    sendCatch (mkDescribeApplicationVersions &~ s)

-- $DescribeApplications
-- Returns the descriptions of existing applications.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationNames.member.1=SampleApplication
-- &Operation=DescribeApplications &AuthParams Sample Description
-- SampleApplication 2010-11-16T20:20:51.974Z 2010-11-16T20:20:51.974Z Default
-- 577c70ff-f1d7-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications'

describeApplications :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State DescribeApplications a
    -> m DescribeApplicationsResponse
describeApplications s =
    send (mkDescribeApplications &~ s)

describeApplicationsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State DescribeApplications a
    -> m (Either ServiceEr DescribeApplicationsResponse)
describeApplicationsCatch s =
    sendCatch (mkDescribeApplications &~ s)

-- $DescribeConfigurationOptions
-- Describes the configuration options that are used in a particular
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
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationOptions'

describeConfigurationOptions :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => State DescribeConfigurationOptions a
    -> m DescribeConfigurationOptionsResponse
describeConfigurationOptions s =
    send (mkDescribeConfigurationOptions &~ s)

describeConfigurationOptionsCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => State DescribeConfigurationOptions a
    -> m (Either ServiceEr DescribeConfigurationOptionsResponse)
describeConfigurationOptionsCatch s =
    sendCatch (mkDescribeConfigurationOptions &~ s)

-- $DescribeConfigurationSettings
-- Returns a description of the settings for the specified configuration set,
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
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeConfigurationSettings'

describeConfigurationSettings :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'dcsApplicationName'
    -> State DescribeConfigurationSettings a
    -> m DescribeConfigurationSettingsResponse
describeConfigurationSettings p1 s =
    send $ (mkDescribeConfigurationSettings p1) &~ s

describeConfigurationSettingsCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => Text -- ^ 'dcsApplicationName'
    -> State DescribeConfigurationSettings a
    -> m (Either ServiceEr DescribeConfigurationSettingsResponse)
describeConfigurationSettingsCatch p1 s =
    sendCatch $ (mkDescribeConfigurationSettings p1) &~ s

-- $DescribeEnvironmentResources
-- Returns AWS resources for this environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=DescribeEnvironmentResources
-- &AuthParams elasticbeanstalk-SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-hbAc8cSZH7
-- elasticbeanstalk-SampleAppVersion-us-east-1c SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-us-east-1c
-- e1cb7b96-f287-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources'

describeEnvironmentResources :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => State DescribeEnvironmentResources a
    -> m DescribeEnvironmentResourcesResponse
describeEnvironmentResources s =
    send (mkDescribeEnvironmentResources &~ s)

describeEnvironmentResourcesCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => State DescribeEnvironmentResources a
    -> m (Either ServiceEr DescribeEnvironmentResourcesResponse)
describeEnvironmentResourcesCatch s =
    sendCatch (mkDescribeEnvironmentResources &~ s)

-- $DescribeEnvironments
-- Returns descriptions for existing environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &IncludeDeleted=true &IncludedDeletedBackTo=2008-11-05T06%3A00%3A00Z
-- &Operation=DescribeEnvironments &AuthParams Version1 Available SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Green e-icsgecu3wf
-- 2010-11-17T04:01:40.668Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 44790c68-f260-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironments'

describeEnvironments :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State DescribeEnvironments a
    -> m DescribeEnvironmentsResponse
describeEnvironments s =
    send (mkDescribeEnvironments &~ s)

describeEnvironmentsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State DescribeEnvironments a
    -> m (Either ServiceEr DescribeEnvironmentsResponse)
describeEnvironmentsCatch s =
    sendCatch (mkDescribeEnvironments &~ s)

-- $DescribeEvents
-- Returns list of event descriptions matching criteria up to the last 6
-- weeks. This action returns the most recent 1,000 events from the specified
-- NextToken.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Severity=TRACE &StartTime=2010-11-17T10%3A26%3A40Z
-- &Operation=DescribeEvents &AuthParams Successfully completed
-- createEnvironment activity. 2010-11-17T20:25:35.191Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Launching a new EC2 instance: i-04a8c569 2010-11-17T20:21:30Z New Version
-- SampleApp SampleAppVersion DEBUG At least one EC2 instance has entered the
-- InService lifecycle state. 2010-11-17T20:20:32.008Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Elastic Load Balancer elasticbeanstalk-SampleAppVersion has failed 0
-- healthy instances - Environment may not be available. 2010-11-17T20:19:28Z
-- New Version SampleApp SampleAppVersion WARN
-- f10d02dd-f288-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents'

describeEvents :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env (ResumableSource m)
                  )
    => State DescribeEvents a
    -> ResumableSource m DescribeEventsResponse
describeEvents s =
    paginate (mkDescribeEvents &~ s)

describeEventsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env (ResumableSource m)
                       )
    => State DescribeEvents a
    -> ResumableSource m (Either ServiceEr DescribeEventsResponse)
describeEventsCatch s =
    paginateCatch (mkDescribeEvents &~ s)

-- $ListAvailableSolutionStacks
-- Returns a list of the available solution stack names.
-- https://elasticbeanstalk.us-east-1.amazon.com/?Operation=ListAvailableSolutionStacks
-- &AuthParams 64bit Amazon Linux running Tomcat 6 32bit Amazon Linux running
-- Tomcat 6 64bit Amazon Linux running Tomcat 7 32bit Amazon Linux running
-- Tomcat 7 f21e2a92-f1fc-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks'

listAvailableSolutionStacks :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => State ListAvailableSolutionStacks a
    -> m ListAvailableSolutionStacksResponse
listAvailableSolutionStacks s =
    send (mkListAvailableSolutionStacks &~ s)

listAvailableSolutionStacksCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => State ListAvailableSolutionStacks a
    -> m (Either ServiceEr ListAvailableSolutionStacksResponse)
listAvailableSolutionStacksCatch s =
    sendCatch (mkListAvailableSolutionStacks &~ s)

-- $RebuildEnvironment
-- Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and forces
-- a restart.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RebuildEnvironment &AuthParams
-- a7d6606e-f289-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.RebuildEnvironment'

rebuildEnvironment :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => State RebuildEnvironment a
    -> m RebuildEnvironmentResponse
rebuildEnvironment s =
    send (mkRebuildEnvironment &~ s)

rebuildEnvironmentCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => State RebuildEnvironment a
    -> m (Either ServiceEr RebuildEnvironmentResponse)
rebuildEnvironmentCatch s =
    sendCatch (mkRebuildEnvironment &~ s)

-- $RequestEnvironmentInfo
-- Initiates a request to compile the specified type of information of the
-- deployed environment. Setting the InfoType to tail compiles the last lines
-- from the application server log files of every Amazon EC2 instance in your
-- environment. Use RetrieveEnvironmentInfo to access the compiled
-- information. Related Topics RetrieveEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RequestEnvironmentInfo &AuthParams
-- 126a4ff3-f28a-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.RequestEnvironmentInfo'

requestEnvironmentInfo :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => EnvironmentInfoType -- ^ 'reiInfoType'
    -> State RequestEnvironmentInfo a
    -> m RequestEnvironmentInfoResponse
requestEnvironmentInfo p3 s =
    send $ (mkRequestEnvironmentInfo p3) &~ s

requestEnvironmentInfoCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => EnvironmentInfoType -- ^ 'reiInfoType'
    -> State RequestEnvironmentInfo a
    -> m (Either ServiceEr RequestEnvironmentInfoResponse)
requestEnvironmentInfoCatch p3 s =
    sendCatch $ (mkRequestEnvironmentInfo p3) &~ s

-- $RestartAppServer
-- Causes the environment to restart the application container server running
-- on each Amazon EC2 instance.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RestartAppServer &AuthParams
-- 90e8d1d5-f28a-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.RestartAppServer'

restartAppServer :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => State RestartAppServer a
    -> m RestartAppServerResponse
restartAppServer s =
    send (mkRestartAppServer &~ s)

restartAppServerCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => State RestartAppServer a
    -> m (Either ServiceEr RestartAppServerResponse)
restartAppServerCatch s =
    sendCatch (mkRestartAppServer &~ s)

-- $RetrieveEnvironmentInfo
-- Retrieves the compiled information from a RequestEnvironmentInfo request.
-- Related Topics RequestEnvironmentInfo
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &InfoType=tail
-- &Operation=RetrieveEnvironmentInfo &AuthParams
-- https://elasticbeanstalk.us-east-1.s3.amazonaws.com/environments%2Fa514386a-709f-4888-9683-068c38d744b4%2Flogs%2Fi-92a3ceff%2F278756a8-7d83-4bc1-93db-b1763163705a.log?Expires=1291236023
-- &AuthParams 2010-11-17T20:40:23.210Z tail i-92a3ceff
-- e8e785c9-f28a-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.RetrieveEnvironmentInfo'

retrieveEnvironmentInfo :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => EnvironmentInfoType -- ^ 'rei1InfoType'
    -> State RetrieveEnvironmentInfo a
    -> m RetrieveEnvironmentInfoResponse
retrieveEnvironmentInfo p3 s =
    send $ (mkRetrieveEnvironmentInfo p3) &~ s

retrieveEnvironmentInfoCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => EnvironmentInfoType -- ^ 'rei1InfoType'
    -> State RetrieveEnvironmentInfo a
    -> m (Either ServiceEr RetrieveEnvironmentInfoResponse)
retrieveEnvironmentInfoCatch p3 s =
    sendCatch $ (mkRetrieveEnvironmentInfo p3) &~ s

-- $SwapEnvironmentCNAMEs
-- Swaps the CNAMEs of two environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?SourceEnvironmentName=SampleApp
-- &DestinationEnvironmentName=SampleApp2 &Operation=SwapEnvironmentCNAMEs
-- &AuthParams f4e1b145-9080-11e0-8e5a-a558e0ce1fc4.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.SwapEnvironmentCNAMEs'

swapEnvironmentCNAMEs :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => State SwapEnvironmentCNAMEs a
    -> m SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEs s =
    send (mkSwapEnvironmentCNAMEs &~ s)

swapEnvironmentCNAMEsCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => State SwapEnvironmentCNAMEs a
    -> m (Either ServiceEr SwapEnvironmentCNAMEsResponse)
swapEnvironmentCNAMEsCatch s =
    sendCatch (mkSwapEnvironmentCNAMEs &~ s)

-- $TerminateEnvironment
-- Terminates the specified environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-icsgecu3wf
-- &EnvironmentName=SampleApp &TerminateResources=true
-- &Operation=TerminateEnvironment &AuthParams Version1 Terminating SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Grey e-icsgecu3wf
-- 2010-11-17T17:10:41.976Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 9b71af21-f26d-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment'

terminateEnvironment :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State TerminateEnvironment a
    -> m TerminateEnvironmentResponse
terminateEnvironment s =
    send (mkTerminateEnvironment &~ s)

terminateEnvironmentCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State TerminateEnvironment a
    -> m (Either ServiceEr TerminateEnvironmentResponse)
terminateEnvironmentCatch s =
    sendCatch (mkTerminateEnvironment &~ s)

-- $UpdateApplication
-- Updates the specified application to have the specified properties. If a
-- property (for example, description) is not provided, the value remains
-- unchanged. To clear these properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Another%20Description &Operation=UpdateApplication &AuthParams
-- New Version Another Description SampleApp 2010-11-17T19:26:20.410Z
-- 2010-11-17T20:42:54.611Z Default 40be666b-f28b-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplication'

updateApplication :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'uaApplicationName'
    -> State UpdateApplication a
    -> m UpdateApplicationResponse
updateApplication p1 s =
    send $ (mkUpdateApplication p1) &~ s

updateApplicationCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'uaApplicationName'
    -> State UpdateApplication a
    -> m (Either ServiceEr UpdateApplicationResponse)
updateApplicationCatch p1 s =
    sendCatch $ (mkUpdateApplication p1) &~ s

-- $UpdateApplicationVersion
-- Updates the specified application version to have the specified properties.
-- If a property (for example, description) is not provided, the value remains
-- unchanged. To clear properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=New%20Version &Description=New%20Release%20Description
-- &Operation=UpdateApplicationVersion &AuthParams awsemr sample.war New
-- Version New Release Description SampleApp 2010-11-17T19:26:20.699Z
-- 2010-11-17T20:48:16.632Z 00b10aa1-f28c-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateApplicationVersion'

updateApplicationVersion :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'uavApplicationName'
    -> Text -- ^ 'uavVersionLabel'
    -> State UpdateApplicationVersion a
    -> m UpdateApplicationVersionResponse
updateApplicationVersion p1 p2 s =
    send $ (mkUpdateApplicationVersion p1 p2) &~ s

updateApplicationVersionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'uavApplicationName'
    -> Text -- ^ 'uavVersionLabel'
    -> State UpdateApplicationVersion a
    -> m (Either ServiceEr UpdateApplicationVersionResponse)
updateApplicationVersionCatch p1 p2 s =
    sendCatch $ (mkUpdateApplicationVersion p1 p2) &~ s

-- $UpdateConfigurationTemplate
-- Updates the specified configuration template to have the specified
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
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateConfigurationTemplate'

updateConfigurationTemplate :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'uctApplicationName'
    -> Text -- ^ 'uctTemplateName'
    -> State UpdateConfigurationTemplate a
    -> m UpdateConfigurationTemplateResponse
updateConfigurationTemplate p1 p2 s =
    send $ (mkUpdateConfigurationTemplate p1 p2) &~ s

updateConfigurationTemplateCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'uctApplicationName'
    -> Text -- ^ 'uctTemplateName'
    -> State UpdateConfigurationTemplate a
    -> m (Either ServiceEr UpdateConfigurationTemplateResponse)
updateConfigurationTemplateCatch p1 p2 s =
    sendCatch $ (mkUpdateConfigurationTemplate p1 p2) &~ s

-- $UpdateEnvironment
-- Updates the environment description, deploys a new application version,
-- updates the configuration settings to an entirely new configuration
-- template, or updates select configuration option values in the running
-- environment. Attempting to update both the release and configuration is not
-- allowed and AWS Elastic Beanstalk returns an InvalidParameterCombination
-- error. When updating the configuration settings to a new template or
-- individual settings, a draft configuration is created and
-- DescribeConfigurationSettings for this environment returns two setting
-- descriptions with different DeploymentStatus values.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &TemplateName=default
-- &OptionsToRemove.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionsToRemove.member.1.OptionName=MeasureName
-- &Operation=UpdateEnvironment &AuthParams New Version Deploying SampleApp
-- elasticbeanstalk-SampleAppVersion-246126201.us-east-1.elb.amazonaws.com
-- SampleApp.elasticbeanstalk.amazonaws.com Grey e-hc8mvnayrx
-- 2010-11-17T21:05:55.251Z 32bit Amazon Linux running Tomcat 7
-- SampleAppDescription SampleAppVersion 2010-11-17T20:17:42.339Z
-- 7705f0bc-f28e-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment'

updateEnvironment :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => State UpdateEnvironment a
    -> m UpdateEnvironmentResponse
updateEnvironment s =
    send (mkUpdateEnvironment &~ s)

updateEnvironmentCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => State UpdateEnvironment a
    -> m (Either ServiceEr UpdateEnvironmentResponse)
updateEnvironmentCatch s =
    sendCatch (mkUpdateEnvironment &~ s)

-- $ValidateConfigurationSettings
-- Takes a set of configuration settings and either a configuration template
-- or environment, and determines whether those values are valid. This action
-- returns a list of messages indicating any errors or warnings associated
-- with the selection of option values.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleAppVersion
-- &OptionSettings.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionSettings.member.1.OptionName=LowerThreshold
-- &OptionSettings.member.1.Value=1000000
-- &Operation=ValidateConfigurationSettings &AuthParams
-- 06f1cfff-f28f-11df-8a78-9f77047e0d0c.
--
-- See: 'Network.AWS.ElasticBeanstalk.V2010_12_01.ValidateConfigurationSettings'

validateConfigurationSettings :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'vcsApplicationName'
    -> [ConfigurationOptionSetting] -- ^ 'vcsOptionSettings'
    -> State ValidateConfigurationSettings a
    -> m ValidateConfigurationSettingsResponse
validateConfigurationSettings p1 p4 s =
    send $ (mkValidateConfigurationSettings p1 p4) &~ s

validateConfigurationSettingsCatch :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadReader Env m
                                      )
    => Text -- ^ 'vcsApplicationName'
    -> [ConfigurationOptionSetting] -- ^ 'vcsOptionSettings'
    -> State ValidateConfigurationSettings a
    -> m (Either ServiceEr ValidateConfigurationSettingsResponse)
validateConfigurationSettingsCatch p1 p4 s =
    sendCatch $ (mkValidateConfigurationSettings p1 p4) &~ s

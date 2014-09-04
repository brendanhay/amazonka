{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Launches an environment for the specified application using the specified
-- configuration.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp
-- &SolutionStackName=32bit%20Amazon%20Linux%20running%20Tomcat%207
-- &Description=EnvDescrip &Operation=CreateEnvironment &AuthParams Version1
-- Deploying SampleApp Grey e-icsgecu3wf 2010-11-17T03:59:33.520Z 32bit Amazon
-- Linux running Tomcat 7 EnvDescrip SampleApp 2010-11-17T03:59:33.520Z
-- 15db925e-f1ff-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateEnvironment
    (
    -- * Request
      CreateEnvironment
    -- ** Request constructor
    , mkCreateEnvironmentMessage
    -- ** Request lenses
    , cemApplicationName
    , cemEnvironmentName
    , cemDescription
    , cemCNAMEPrefix
    , cemTier
    , cemTags
    , cemVersionLabel
    , cemTemplateName
    , cemSolutionStackName
    , cemOptionSettings
    , cemOptionsToRemove

    -- * Response
    , CreateEnvironmentResponse
    -- ** Response lenses
    , edEnvironmentName
    , edEnvironmentId
    , edApplicationName
    , edVersionLabel
    , edSolutionStackName
    , edTemplateName
    , edDescription
    , edEndpointURL
    , edCNAME
    , edDateCreated
    , edDateUpdated
    , edStatus
    , edHealth
    , edResources
    , edTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateEnvironment' request.
mkCreateEnvironmentMessage :: Text -- ^ 'cemApplicationName'
                           -> Text -- ^ 'cemEnvironmentName'
                           -> CreateEnvironment
mkCreateEnvironmentMessage p1 p2 = CreateEnvironment
    { _cemApplicationName = p1
    , _cemEnvironmentName = p2
    , _cemDescription = Nothing
    , _cemCNAMEPrefix = Nothing
    , _cemTier = Nothing
    , _cemTags = mempty
    , _cemVersionLabel = Nothing
    , _cemTemplateName = Nothing
    , _cemSolutionStackName = Nothing
    , _cemOptionSettings = mempty
    , _cemOptionsToRemove = mempty
    }
{-# INLINE mkCreateEnvironmentMessage #-}

data CreateEnvironment = CreateEnvironment
    { _cemApplicationName :: Text
      -- ^ The name of the application that contains the version to be
      -- deployed. If no application is found with this name,
      -- CreateEnvironment returns an InvalidParameterValue error.
    , _cemEnvironmentName :: Text
      -- ^ A unique name for the deployment environment. Used in the
      -- application URL. Constraint: Must be from 4 to 23 characters in
      -- length. The name can contain only letters, numbers, and hyphens.
      -- It cannot start or end with a hyphen. This name must be unique in
      -- your account. If the specified name already exists, AWS Elastic
      -- Beanstalk returns an InvalidParameterValue error. Default: If the
      -- CNAME parameter is not specified, the environment name becomes
      -- part of the CNAME, and therefore part of the visible URL for your
      -- application.
    , _cemDescription :: Maybe Text
      -- ^ Describes this environment.
    , _cemCNAMEPrefix :: Maybe Text
      -- ^ If specified, the environment attempts to use this value as the
      -- prefix for the CNAME. If not specified, the CNAME is generated
      -- automatically by appending a random alphanumeric string to the
      -- environment name.
    , _cemTier :: Maybe EnvironmentTier
      -- ^ This specifies the tier to use for creating this environment.
    , _cemTags :: [Tag]
      -- ^ This specifies the tags applied to resources in the environment.
    , _cemVersionLabel :: Maybe Text
      -- ^ The name of the application version to deploy. If the specified
      -- application has no associated application versions, AWS Elastic
      -- Beanstalk UpdateEnvironment returns an InvalidParameterValue
      -- error. Default: If not specified, AWS Elastic Beanstalk attempts
      -- to launch the application version">sample application in the
      -- container.
    , _cemTemplateName :: Maybe Text
      -- ^ The name of the configuration template to use in deployment. If
      -- no configuration template is found with this name, AWS Elastic
      -- Beanstalk returns an InvalidParameterValue error. Condition: You
      -- must specify either this parameter or a SolutionStackName, but
      -- not both. If you specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns a MissingRequiredParameter error.
    , _cemSolutionStackName :: Maybe Text
      -- ^ This is an alternative to specifying a configuration name. If
      -- specified, AWS Elastic Beanstalk sets the configuration values to
      -- the default values associated with the specified solution stack.
      -- Condition: You must specify either this or a TemplateName, but
      -- not both. If you specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns a MissingRequiredParameter error.
    , _cemOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk sets the specified
      -- configuration options to the requested value in the configuration
      -- set for the new environment. These override the values obtained
      -- from the solution stack or the configuration template.
    , _cemOptionsToRemove :: [OptionSpecification]
      -- ^ A list of custom user-defined configuration options to remove
      -- from the configuration set for this new environment.
    } deriving (Show, Generic)

-- | The name of the application that contains the version to be deployed. If no
-- application is found with this name, CreateEnvironment returns an
-- InvalidParameterValue error.
cemApplicationName :: Lens' CreateEnvironment (Text)
cemApplicationName = lens _cemApplicationName (\s a -> s { _cemApplicationName = a })
{-# INLINE cemApplicationName #-}

-- | A unique name for the deployment environment. Used in the application URL.
-- Constraint: Must be from 4 to 23 characters in length. The name can contain
-- only letters, numbers, and hyphens. It cannot start or end with a hyphen.
-- This name must be unique in your account. If the specified name already
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Default: If the CNAME parameter is not specified, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for your
-- application.
cemEnvironmentName :: Lens' CreateEnvironment (Text)
cemEnvironmentName = lens _cemEnvironmentName (\s a -> s { _cemEnvironmentName = a })
{-# INLINE cemEnvironmentName #-}

-- | Describes this environment.
cemDescription :: Lens' CreateEnvironment (Maybe Text)
cemDescription = lens _cemDescription (\s a -> s { _cemDescription = a })
{-# INLINE cemDescription #-}

-- | If specified, the environment attempts to use this value as the prefix for
-- the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
cemCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
cemCNAMEPrefix = lens _cemCNAMEPrefix (\s a -> s { _cemCNAMEPrefix = a })
{-# INLINE cemCNAMEPrefix #-}

-- | This specifies the tier to use for creating this environment.
cemTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
cemTier = lens _cemTier (\s a -> s { _cemTier = a })
{-# INLINE cemTier #-}

-- | This specifies the tags applied to resources in the environment.
cemTags :: Lens' CreateEnvironment ([Tag])
cemTags = lens _cemTags (\s a -> s { _cemTags = a })
{-# INLINE cemTags #-}

-- | The name of the application version to deploy. If the specified application
-- has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the application
-- version">sample application in the container.
cemVersionLabel :: Lens' CreateEnvironment (Maybe Text)
cemVersionLabel = lens _cemVersionLabel (\s a -> s { _cemVersionLabel = a })
{-# INLINE cemVersionLabel #-}

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this parameter or a SolutionStackName, but not both. If you specify both,
-- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
-- do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemTemplateName :: Lens' CreateEnvironment (Maybe Text)
cemTemplateName = lens _cemTemplateName (\s a -> s { _cemTemplateName = a })
{-# INLINE cemTemplateName #-}

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default values
-- associated with the specified solution stack. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
cemSolutionStackName = lens _cemSolutionStackName (\s a -> s { _cemSolutionStackName = a })
{-# INLINE cemSolutionStackName #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack or
-- the configuration template.
cemOptionSettings :: Lens' CreateEnvironment ([ConfigurationOptionSetting])
cemOptionSettings = lens _cemOptionSettings (\s a -> s { _cemOptionSettings = a })
{-# INLINE cemOptionSettings #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
cemOptionsToRemove :: Lens' CreateEnvironment ([OptionSpecification])
cemOptionsToRemove = lens _cemOptionsToRemove (\s a -> s { _cemOptionsToRemove = a })
{-# INLINE cemOptionsToRemove #-}

instance ToQuery CreateEnvironment where
    toQuery = genericQuery def

data CreateEnvironmentResponse = CreateEnvironmentResponse
    { _edEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _edEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _edApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _edVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    , _edSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _edTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _edDescription :: Maybe Text
      -- ^ Describes this environment.
    , _edEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _edCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _edDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _edDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _edStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _edHealth :: Maybe EnvironmentHealth
      -- ^ Describes the health status of the environment. AWS Elastic
      -- Beanstalk indicates the failure levels for a running environment:
      -- Red : Indicates the environment is not working. Yellow: Indicates
      -- that something is wrong, the application might not be available,
      -- but the instances appear running. Green: Indicates the
      -- environment is healthy and fully functional. Red: Indicates the
      -- environment is not responsive. Occurs when three or more
      -- consecutive failures occur for an environment. Yellow: Indicates
      -- that something is wrong. Occurs when two consecutive failures
      -- occur for an environment. Green: Indicates the environment is
      -- healthy and fully functional. Grey: Default health for a new
      -- environment. The environment is not fully launched and health
      -- checks have not started or health checks are suspended during an
      -- UpdateEnvironment or RestartEnvironement request. Default: Grey.
    , _edResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _edTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    } deriving (Show, Generic)

-- | The name of this environment.
edEnvironmentName :: Lens' CreateEnvironmentResponse (Maybe Text)
edEnvironmentName = lens _edEnvironmentName (\s a -> s { _edEnvironmentName = a })
{-# INLINE edEnvironmentName #-}

-- | The ID of this environment.
edEnvironmentId :: Lens' CreateEnvironmentResponse (Maybe Text)
edEnvironmentId = lens _edEnvironmentId (\s a -> s { _edEnvironmentId = a })
{-# INLINE edEnvironmentId #-}

-- | The name of the application associated with this environment.
edApplicationName :: Lens' CreateEnvironmentResponse (Maybe Text)
edApplicationName = lens _edApplicationName (\s a -> s { _edApplicationName = a })
{-# INLINE edApplicationName #-}

-- | The application version deployed in this environment.
edVersionLabel :: Lens' CreateEnvironmentResponse (Maybe Text)
edVersionLabel = lens _edVersionLabel (\s a -> s { _edVersionLabel = a })
{-# INLINE edVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
edSolutionStackName :: Lens' CreateEnvironmentResponse (Maybe Text)
edSolutionStackName = lens _edSolutionStackName (\s a -> s { _edSolutionStackName = a })
{-# INLINE edSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
edTemplateName :: Lens' CreateEnvironmentResponse (Maybe Text)
edTemplateName = lens _edTemplateName (\s a -> s { _edTemplateName = a })
{-# INLINE edTemplateName #-}

-- | Describes this environment.
edDescription :: Lens' CreateEnvironmentResponse (Maybe Text)
edDescription = lens _edDescription (\s a -> s { _edDescription = a })
{-# INLINE edDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
edEndpointURL :: Lens' CreateEnvironmentResponse (Maybe Text)
edEndpointURL = lens _edEndpointURL (\s a -> s { _edEndpointURL = a })
{-# INLINE edEndpointURL #-}

-- | The URL to the CNAME for this environment.
edCNAME :: Lens' CreateEnvironmentResponse (Maybe Text)
edCNAME = lens _edCNAME (\s a -> s { _edCNAME = a })
{-# INLINE edCNAME #-}

-- | The creation date for this environment.
edDateCreated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
edDateCreated = lens _edDateCreated (\s a -> s { _edDateCreated = a })
{-# INLINE edDateCreated #-}

-- | The last modified date for this environment.
edDateUpdated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
edDateUpdated = lens _edDateUpdated (\s a -> s { _edDateUpdated = a })
{-# INLINE edDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
edStatus :: Lens' CreateEnvironmentResponse (Maybe EnvironmentStatus)
edStatus = lens _edStatus (\s a -> s { _edStatus = a })
{-# INLINE edStatus #-}

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates the
-- environment is not working. Yellow: Indicates that something is wrong, the
-- application might not be available, but the instances appear running.
-- Green: Indicates the environment is healthy and fully functional. Red:
-- Indicates the environment is not responsive. Occurs when three or more
-- consecutive failures occur for an environment. Yellow: Indicates that
-- something is wrong. Occurs when two consecutive failures occur for an
-- environment. Green: Indicates the environment is healthy and fully
-- functional. Grey: Default health for a new environment. The environment is
-- not fully launched and health checks have not started or health checks are
-- suspended during an UpdateEnvironment or RestartEnvironement request.
-- Default: Grey.
edHealth :: Lens' CreateEnvironmentResponse (Maybe EnvironmentHealth)
edHealth = lens _edHealth (\s a -> s { _edHealth = a })
{-# INLINE edHealth #-}

-- | The description of the AWS resources used by this environment.
edResources :: Lens' CreateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
edResources = lens _edResources (\s a -> s { _edResources = a })
{-# INLINE edResources #-}

-- | Describes the current tier of this environment.
edTier :: Lens' CreateEnvironmentResponse (Maybe EnvironmentTier)
edTier = lens _edTier (\s a -> s { _edTier = a })
{-# INLINE edTier #-}

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEnvironment where
    type Sv CreateEnvironment = ElasticBeanstalk
    type Rs CreateEnvironment = CreateEnvironmentResponse

    request = post "CreateEnvironment"
    response _ = xmlResponse

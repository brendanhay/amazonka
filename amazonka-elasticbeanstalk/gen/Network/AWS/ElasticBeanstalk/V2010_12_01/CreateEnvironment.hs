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
    , createEnvironment
    -- ** Request lenses
    , cemApplicationName
    , cemEnvironmentName
    , cemOptionSettings
    , cemTemplateName
    , cemCNAMEPrefix
    , cemDescription
    , cemTier
    , cemOptionsToRemove
    , cemSolutionStackName
    , cemTags
    , cemVersionLabel

    -- * Response
    , CreateEnvironmentResponse
    -- ** Response lenses
    , edApplicationName
    , edTemplateName
    , edDateCreated
    , edCNAME
    , edDescription
    , edEndpointURL
    , edHealth
    , edEnvironmentId
    , edEnvironmentName
    , edResources
    , edStatus
    , edTier
    , edSolutionStackName
    , edDateUpdated
    , edVersionLabel
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateEnvironment' request.
createEnvironment :: Text -- ^ 'cemApplicationName'
                  -> Text -- ^ 'cemEnvironmentName'
                  -> CreateEnvironment
createEnvironment p1 p2 = CreateEnvironment
    { _cemApplicationName = p1
    , _cemEnvironmentName = p2
    , _cemOptionSettings = mempty
    , _cemTemplateName = Nothing
    , _cemCNAMEPrefix = Nothing
    , _cemDescription = Nothing
    , _cemTier = Nothing
    , _cemOptionsToRemove = mempty
    , _cemSolutionStackName = Nothing
    , _cemTags = mempty
    , _cemVersionLabel = Nothing
    }
{-# INLINE createEnvironment #-}

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
    , _cemOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk sets the specified
      -- configuration options to the requested value in the configuration
      -- set for the new environment. These override the values obtained
      -- from the solution stack or the configuration template.
    , _cemTemplateName :: Maybe Text
      -- ^ The name of the configuration template to use in deployment. If
      -- no configuration template is found with this name, AWS Elastic
      -- Beanstalk returns an InvalidParameterValue error. Condition: You
      -- must specify either this parameter or a SolutionStackName, but
      -- not both. If you specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns a MissingRequiredParameter error.
    , _cemCNAMEPrefix :: Maybe Text
      -- ^ If specified, the environment attempts to use this value as the
      -- prefix for the CNAME. If not specified, the CNAME is generated
      -- automatically by appending a random alphanumeric string to the
      -- environment name.
    , _cemDescription :: Maybe Text
      -- ^ Describes this environment.
    , _cemTier :: Maybe EnvironmentTier
      -- ^ This specifies the tier to use for creating this environment.
    , _cemOptionsToRemove :: [OptionSpecification]
      -- ^ A list of custom user-defined configuration options to remove
      -- from the configuration set for this new environment.
    , _cemSolutionStackName :: Maybe Text
      -- ^ This is an alternative to specifying a configuration name. If
      -- specified, AWS Elastic Beanstalk sets the configuration values to
      -- the default values associated with the specified solution stack.
      -- Condition: You must specify either this or a TemplateName, but
      -- not both. If you specify both, AWS Elastic Beanstalk returns an
      -- InvalidParameterCombination error. If you do not specify either,
      -- AWS Elastic Beanstalk returns a MissingRequiredParameter error.
    , _cemTags :: [Tag]
      -- ^ This specifies the tags applied to resources in the environment.
    , _cemVersionLabel :: Maybe Text
      -- ^ The name of the application version to deploy. If the specified
      -- application has no associated application versions, AWS Elastic
      -- Beanstalk UpdateEnvironment returns an InvalidParameterValue
      -- error. Default: If not specified, AWS Elastic Beanstalk attempts
      -- to launch the application version">sample application in the
      -- container.
    } deriving (Show, Generic)

-- | The name of the application that contains the version to be deployed. If no
-- application is found with this name, CreateEnvironment returns an
-- InvalidParameterValue error.
cemApplicationName :: Lens' CreateEnvironment (Text)
cemApplicationName f x =
    f (_cemApplicationName x)
        <&> \y -> x { _cemApplicationName = y }
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
cemEnvironmentName f x =
    f (_cemEnvironmentName x)
        <&> \y -> x { _cemEnvironmentName = y }
{-# INLINE cemEnvironmentName #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack or
-- the configuration template.
cemOptionSettings :: Lens' CreateEnvironment ([ConfigurationOptionSetting])
cemOptionSettings f x =
    f (_cemOptionSettings x)
        <&> \y -> x { _cemOptionSettings = y }
{-# INLINE cemOptionSettings #-}

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this parameter or a SolutionStackName, but not both. If you specify both,
-- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
-- do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemTemplateName :: Lens' CreateEnvironment (Maybe Text)
cemTemplateName f x =
    f (_cemTemplateName x)
        <&> \y -> x { _cemTemplateName = y }
{-# INLINE cemTemplateName #-}

-- | If specified, the environment attempts to use this value as the prefix for
-- the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
cemCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
cemCNAMEPrefix f x =
    f (_cemCNAMEPrefix x)
        <&> \y -> x { _cemCNAMEPrefix = y }
{-# INLINE cemCNAMEPrefix #-}

-- | Describes this environment.
cemDescription :: Lens' CreateEnvironment (Maybe Text)
cemDescription f x =
    f (_cemDescription x)
        <&> \y -> x { _cemDescription = y }
{-# INLINE cemDescription #-}

-- | This specifies the tier to use for creating this environment.
cemTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
cemTier f x =
    f (_cemTier x)
        <&> \y -> x { _cemTier = y }
{-# INLINE cemTier #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
cemOptionsToRemove :: Lens' CreateEnvironment ([OptionSpecification])
cemOptionsToRemove f x =
    f (_cemOptionsToRemove x)
        <&> \y -> x { _cemOptionsToRemove = y }
{-# INLINE cemOptionsToRemove #-}

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default values
-- associated with the specified solution stack. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
cemSolutionStackName f x =
    f (_cemSolutionStackName x)
        <&> \y -> x { _cemSolutionStackName = y }
{-# INLINE cemSolutionStackName #-}

-- | This specifies the tags applied to resources in the environment.
cemTags :: Lens' CreateEnvironment ([Tag])
cemTags f x =
    f (_cemTags x)
        <&> \y -> x { _cemTags = y }
{-# INLINE cemTags #-}

-- | The name of the application version to deploy. If the specified application
-- has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the application
-- version">sample application in the container.
cemVersionLabel :: Lens' CreateEnvironment (Maybe Text)
cemVersionLabel f x =
    f (_cemVersionLabel x)
        <&> \y -> x { _cemVersionLabel = y }
{-# INLINE cemVersionLabel #-}

instance ToQuery CreateEnvironment where
    toQuery = genericQuery def

data CreateEnvironmentResponse = CreateEnvironmentResponse
    { _edApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _edTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _edDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _edCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _edDescription :: Maybe Text
      -- ^ Describes this environment.
    , _edEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
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
    , _edEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _edEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _edResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _edStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _edTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    , _edSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _edDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _edVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Show, Generic)

-- | The name of the application associated with this environment.
edApplicationName :: Lens' CreateEnvironmentResponse (Maybe Text)
edApplicationName f x =
    f (_edApplicationName x)
        <&> \y -> x { _edApplicationName = y }
{-# INLINE edApplicationName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
edTemplateName :: Lens' CreateEnvironmentResponse (Maybe Text)
edTemplateName f x =
    f (_edTemplateName x)
        <&> \y -> x { _edTemplateName = y }
{-# INLINE edTemplateName #-}

-- | The creation date for this environment.
edDateCreated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
edDateCreated f x =
    f (_edDateCreated x)
        <&> \y -> x { _edDateCreated = y }
{-# INLINE edDateCreated #-}

-- | The URL to the CNAME for this environment.
edCNAME :: Lens' CreateEnvironmentResponse (Maybe Text)
edCNAME f x =
    f (_edCNAME x)
        <&> \y -> x { _edCNAME = y }
{-# INLINE edCNAME #-}

-- | Describes this environment.
edDescription :: Lens' CreateEnvironmentResponse (Maybe Text)
edDescription f x =
    f (_edDescription x)
        <&> \y -> x { _edDescription = y }
{-# INLINE edDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
edEndpointURL :: Lens' CreateEnvironmentResponse (Maybe Text)
edEndpointURL f x =
    f (_edEndpointURL x)
        <&> \y -> x { _edEndpointURL = y }
{-# INLINE edEndpointURL #-}

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
edHealth f x =
    f (_edHealth x)
        <&> \y -> x { _edHealth = y }
{-# INLINE edHealth #-}

-- | The ID of this environment.
edEnvironmentId :: Lens' CreateEnvironmentResponse (Maybe Text)
edEnvironmentId f x =
    f (_edEnvironmentId x)
        <&> \y -> x { _edEnvironmentId = y }
{-# INLINE edEnvironmentId #-}

-- | The name of this environment.
edEnvironmentName :: Lens' CreateEnvironmentResponse (Maybe Text)
edEnvironmentName f x =
    f (_edEnvironmentName x)
        <&> \y -> x { _edEnvironmentName = y }
{-# INLINE edEnvironmentName #-}

-- | The description of the AWS resources used by this environment.
edResources :: Lens' CreateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
edResources f x =
    f (_edResources x)
        <&> \y -> x { _edResources = y }
{-# INLINE edResources #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
edStatus :: Lens' CreateEnvironmentResponse (Maybe EnvironmentStatus)
edStatus f x =
    f (_edStatus x)
        <&> \y -> x { _edStatus = y }
{-# INLINE edStatus #-}

-- | Describes the current tier of this environment.
edTier :: Lens' CreateEnvironmentResponse (Maybe EnvironmentTier)
edTier f x =
    f (_edTier x)
        <&> \y -> x { _edTier = y }
{-# INLINE edTier #-}

-- | The name of the SolutionStack deployed with this environment.
edSolutionStackName :: Lens' CreateEnvironmentResponse (Maybe Text)
edSolutionStackName f x =
    f (_edSolutionStackName x)
        <&> \y -> x { _edSolutionStackName = y }
{-# INLINE edSolutionStackName #-}

-- | The last modified date for this environment.
edDateUpdated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
edDateUpdated f x =
    f (_edDateUpdated x)
        <&> \y -> x { _edDateUpdated = y }
{-# INLINE edDateUpdated #-}

-- | The application version deployed in this environment.
edVersionLabel :: Lens' CreateEnvironmentResponse (Maybe Text)
edVersionLabel f x =
    f (_edVersionLabel x)
        <&> \y -> x { _edVersionLabel = y }
{-# INLINE edVersionLabel #-}

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEnvironment where
    type Sv CreateEnvironment = ElasticBeanstalk
    type Rs CreateEnvironment = CreateEnvironmentResponse

    request = post "CreateEnvironment"
    response _ = xmlResponse

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
cemApplicationName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemApplicationName f x =
    (\y -> x { _cemApplicationName = y })
       <$> f (_cemApplicationName x)
{-# INLINE cemApplicationName #-}

-- | A unique name for the deployment environment. Used in the application URL.
-- Constraint: Must be from 4 to 23 characters in length. The name can contain
-- only letters, numbers, and hyphens. It cannot start or end with a hyphen.
-- This name must be unique in your account. If the specified name already
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Default: If the CNAME parameter is not specified, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for your
-- application.
cemEnvironmentName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemEnvironmentName f x =
    (\y -> x { _cemEnvironmentName = y })
       <$> f (_cemEnvironmentName x)
{-# INLINE cemEnvironmentName #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack or
-- the configuration template.
cemOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> CreateEnvironment
    -> f CreateEnvironment
cemOptionSettings f x =
    (\y -> x { _cemOptionSettings = y })
       <$> f (_cemOptionSettings x)
{-# INLINE cemOptionSettings #-}

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this parameter or a SolutionStackName, but not both. If you specify both,
-- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
-- do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemTemplateName f x =
    (\y -> x { _cemTemplateName = y })
       <$> f (_cemTemplateName x)
{-# INLINE cemTemplateName #-}

-- | If specified, the environment attempts to use this value as the prefix for
-- the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
cemCNAMEPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemCNAMEPrefix f x =
    (\y -> x { _cemCNAMEPrefix = y })
       <$> f (_cemCNAMEPrefix x)
{-# INLINE cemCNAMEPrefix #-}

-- | Describes this environment.
cemDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemDescription f x =
    (\y -> x { _cemDescription = y })
       <$> f (_cemDescription x)
{-# INLINE cemDescription #-}

-- | This specifies the tier to use for creating this environment.
cemTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> CreateEnvironment
    -> f CreateEnvironment
cemTier f x =
    (\y -> x { _cemTier = y })
       <$> f (_cemTier x)
{-# INLINE cemTier #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
cemOptionsToRemove
    :: Functor f
    => ([OptionSpecification]
    -> f ([OptionSpecification]))
    -> CreateEnvironment
    -> f CreateEnvironment
cemOptionsToRemove f x =
    (\y -> x { _cemOptionsToRemove = y })
       <$> f (_cemOptionsToRemove x)
{-# INLINE cemOptionsToRemove #-}

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default values
-- associated with the specified solution stack. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
cemSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemSolutionStackName f x =
    (\y -> x { _cemSolutionStackName = y })
       <$> f (_cemSolutionStackName x)
{-# INLINE cemSolutionStackName #-}

-- | This specifies the tags applied to resources in the environment.
cemTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CreateEnvironment
    -> f CreateEnvironment
cemTags f x =
    (\y -> x { _cemTags = y })
       <$> f (_cemTags x)
{-# INLINE cemTags #-}

-- | The name of the application version to deploy. If the specified application
-- has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the application
-- version">sample application in the container.
cemVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironment
    -> f CreateEnvironment
cemVersionLabel f x =
    (\y -> x { _cemVersionLabel = y })
       <$> f (_cemVersionLabel x)
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
edApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edApplicationName f x =
    (\y -> x { _edApplicationName = y })
       <$> f (_edApplicationName x)
{-# INLINE edApplicationName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
edTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edTemplateName f x =
    (\y -> x { _edTemplateName = y })
       <$> f (_edTemplateName x)
{-# INLINE edTemplateName #-}

-- | The creation date for this environment.
edDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edDateCreated f x =
    (\y -> x { _edDateCreated = y })
       <$> f (_edDateCreated x)
{-# INLINE edDateCreated #-}

-- | The URL to the CNAME for this environment.
edCNAME
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edCNAME f x =
    (\y -> x { _edCNAME = y })
       <$> f (_edCNAME x)
{-# INLINE edCNAME #-}

-- | Describes this environment.
edDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edDescription f x =
    (\y -> x { _edDescription = y })
       <$> f (_edDescription x)
{-# INLINE edDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
edEndpointURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edEndpointURL f x =
    (\y -> x { _edEndpointURL = y })
       <$> f (_edEndpointURL x)
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
edHealth
    :: Functor f
    => (Maybe EnvironmentHealth
    -> f (Maybe EnvironmentHealth))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edHealth f x =
    (\y -> x { _edHealth = y })
       <$> f (_edHealth x)
{-# INLINE edHealth #-}

-- | The ID of this environment.
edEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edEnvironmentId f x =
    (\y -> x { _edEnvironmentId = y })
       <$> f (_edEnvironmentId x)
{-# INLINE edEnvironmentId #-}

-- | The name of this environment.
edEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edEnvironmentName f x =
    (\y -> x { _edEnvironmentName = y })
       <$> f (_edEnvironmentName x)
{-# INLINE edEnvironmentName #-}

-- | The description of the AWS resources used by this environment.
edResources
    :: Functor f
    => (Maybe EnvironmentResourcesDescription
    -> f (Maybe EnvironmentResourcesDescription))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edResources f x =
    (\y -> x { _edResources = y })
       <$> f (_edResources x)
{-# INLINE edResources #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
edStatus
    :: Functor f
    => (Maybe EnvironmentStatus
    -> f (Maybe EnvironmentStatus))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edStatus f x =
    (\y -> x { _edStatus = y })
       <$> f (_edStatus x)
{-# INLINE edStatus #-}

-- | Describes the current tier of this environment.
edTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edTier f x =
    (\y -> x { _edTier = y })
       <$> f (_edTier x)
{-# INLINE edTier #-}

-- | The name of the SolutionStack deployed with this environment.
edSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edSolutionStackName f x =
    (\y -> x { _edSolutionStackName = y })
       <$> f (_edSolutionStackName x)
{-# INLINE edSolutionStackName #-}

-- | The last modified date for this environment.
edDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edDateUpdated f x =
    (\y -> x { _edDateUpdated = y })
       <$> f (_edDateUpdated x)
{-# INLINE edDateUpdated #-}

-- | The application version deployed in this environment.
edVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateEnvironmentResponse
    -> f CreateEnvironmentResponse
edVersionLabel f x =
    (\y -> x { _edVersionLabel = y })
       <$> f (_edVersionLabel x)
{-# INLINE edVersionLabel #-}

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEnvironment where
    type Sv CreateEnvironment = ElasticBeanstalk
    type Rs CreateEnvironment = CreateEnvironmentResponse

    request = post "CreateEnvironment"
    response _ = xmlResponse

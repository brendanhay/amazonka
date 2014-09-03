{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the environment description, deploys a new application version,
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment
    (
    -- * Request
      UpdateEnvironment
    -- ** Request constructor
    , updateEnvironment
    -- ** Request lenses
    , uemOptionSettings
    , uemTemplateName
    , uemDescription
    , uemEnvironmentId
    , uemEnvironmentName
    , uemTier
    , uemOptionsToRemove
    , uemVersionLabel

    -- * Response
    , UpdateEnvironmentResponse
    -- ** Response lenses
    , eeeoApplicationName
    , eeeoTemplateName
    , eeeoDateCreated
    , eeeoCNAME
    , eeeoDescription
    , eeeoEndpointURL
    , eeeoHealth
    , eeeoEnvironmentId
    , eeeoEnvironmentName
    , eeeoResources
    , eeeoStatus
    , eeeoTier
    , eeeoSolutionStackName
    , eeeoDateUpdated
    , eeeoVersionLabel
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateEnvironment' request.
updateEnvironment :: UpdateEnvironment
updateEnvironment = UpdateEnvironment
    { _uemOptionSettings = mempty
    , _uemTemplateName = Nothing
    , _uemDescription = Nothing
    , _uemEnvironmentId = Nothing
    , _uemEnvironmentName = Nothing
    , _uemTier = Nothing
    , _uemOptionsToRemove = mempty
    , _uemVersionLabel = Nothing
    }

data UpdateEnvironment = UpdateEnvironment
    { _uemOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk updates the configuration set
      -- associated with the running environment and sets the specified
      -- configuration options to the requested value.
    , _uemTemplateName :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk deploys
      -- this configuration template to the environment. If no such
      -- configuration template is found, AWS Elastic Beanstalk returns an
      -- InvalidParameterValue error.
    , _uemDescription :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk updates the
      -- description of this environment.
    , _uemEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to update. If no environment with this
      -- ID exists, AWS Elastic Beanstalk returns an InvalidParameterValue
      -- error. Condition: You must specify either this or an
      -- EnvironmentName, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    , _uemEnvironmentName :: Maybe Text
      -- ^ The name of the environment to update. If no environment with
      -- this name exists, AWS Elastic Beanstalk returns an
      -- InvalidParameterValue error. Condition: You must specify either
      -- this or an EnvironmentId, or both. If you do not specify either,
      -- AWS Elastic Beanstalk returns MissingRequiredParameter error.
    , _uemTier :: Maybe EnvironmentTier
      -- ^ This specifies the tier to use to update the environment.
      -- Condition: You can only update the tier version for an
      -- environment. If you change the name of the type, AWS Elastic
      -- Beanstalk returns InvalidParameterValue error.
    , _uemOptionsToRemove :: [OptionSpecification]
      -- ^ A list of custom user-defined configuration options to remove
      -- from the configuration set for this environment.
    , _uemVersionLabel :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk deploys the
      -- named application version to the environment. If no such
      -- application version is found, returns an InvalidParameterValue
      -- error.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
uemOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemOptionSettings f x =
    (\y -> x { _uemOptionSettings = y })
       <$> f (_uemOptionSettings x)
{-# INLINE uemOptionSettings #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
uemTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemTemplateName f x =
    (\y -> x { _uemTemplateName = y })
       <$> f (_uemTemplateName x)
{-# INLINE uemTemplateName #-}

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
uemDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemDescription f x =
    (\y -> x { _uemDescription = y })
       <$> f (_uemDescription x)
{-# INLINE uemDescription #-}

-- | The ID of the environment to update. If no environment with this ID exists,
-- AWS Elastic Beanstalk returns an InvalidParameterValue error. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
uemEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemEnvironmentId f x =
    (\y -> x { _uemEnvironmentId = y })
       <$> f (_uemEnvironmentId x)
{-# INLINE uemEnvironmentId #-}

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
uemEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemEnvironmentName f x =
    (\y -> x { _uemEnvironmentName = y })
       <$> f (_uemEnvironmentName x)
{-# INLINE uemEnvironmentName #-}

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the name
-- of the type, AWS Elastic Beanstalk returns InvalidParameterValue error.
uemTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemTier f x =
    (\y -> x { _uemTier = y })
       <$> f (_uemTier x)
{-# INLINE uemTier #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
uemOptionsToRemove
    :: Functor f
    => ([OptionSpecification]
    -> f ([OptionSpecification]))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemOptionsToRemove f x =
    (\y -> x { _uemOptionsToRemove = y })
       <$> f (_uemOptionsToRemove x)
{-# INLINE uemOptionsToRemove #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
uemVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironment
    -> f UpdateEnvironment
uemVersionLabel f x =
    (\y -> x { _uemVersionLabel = y })
       <$> f (_uemVersionLabel x)
{-# INLINE uemVersionLabel #-}

instance ToQuery UpdateEnvironment where
    toQuery = genericQuery def

data UpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _eeeoApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeeoTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeeoDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeeoCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeeoDescription :: Maybe Text
      -- ^ Describes this environment.
    , _eeeoEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeeoHealth :: Maybe EnvironmentHealth
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
    , _eeeoEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeeoEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeeoResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeeoStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _eeeoTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    , _eeeoSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeeoDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeeoVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Show, Generic)

-- | The name of the application associated with this environment.
eeeoApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoApplicationName f x =
    (\y -> x { _eeeoApplicationName = y })
       <$> f (_eeeoApplicationName x)
{-# INLINE eeeoApplicationName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeeoTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoTemplateName f x =
    (\y -> x { _eeeoTemplateName = y })
       <$> f (_eeeoTemplateName x)
{-# INLINE eeeoTemplateName #-}

-- | The creation date for this environment.
eeeoDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoDateCreated f x =
    (\y -> x { _eeeoDateCreated = y })
       <$> f (_eeeoDateCreated x)
{-# INLINE eeeoDateCreated #-}

-- | The URL to the CNAME for this environment.
eeeoCNAME
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoCNAME f x =
    (\y -> x { _eeeoCNAME = y })
       <$> f (_eeeoCNAME x)
{-# INLINE eeeoCNAME #-}

-- | Describes this environment.
eeeoDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoDescription f x =
    (\y -> x { _eeeoDescription = y })
       <$> f (_eeeoDescription x)
{-# INLINE eeeoDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeeoEndpointURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoEndpointURL f x =
    (\y -> x { _eeeoEndpointURL = y })
       <$> f (_eeeoEndpointURL x)
{-# INLINE eeeoEndpointURL #-}

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
eeeoHealth
    :: Functor f
    => (Maybe EnvironmentHealth
    -> f (Maybe EnvironmentHealth))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoHealth f x =
    (\y -> x { _eeeoHealth = y })
       <$> f (_eeeoHealth x)
{-# INLINE eeeoHealth #-}

-- | The ID of this environment.
eeeoEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoEnvironmentId f x =
    (\y -> x { _eeeoEnvironmentId = y })
       <$> f (_eeeoEnvironmentId x)
{-# INLINE eeeoEnvironmentId #-}

-- | The name of this environment.
eeeoEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoEnvironmentName f x =
    (\y -> x { _eeeoEnvironmentName = y })
       <$> f (_eeeoEnvironmentName x)
{-# INLINE eeeoEnvironmentName #-}

-- | The description of the AWS resources used by this environment.
eeeoResources
    :: Functor f
    => (Maybe EnvironmentResourcesDescription
    -> f (Maybe EnvironmentResourcesDescription))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoResources f x =
    (\y -> x { _eeeoResources = y })
       <$> f (_eeeoResources x)
{-# INLINE eeeoResources #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeeoStatus
    :: Functor f
    => (Maybe EnvironmentStatus
    -> f (Maybe EnvironmentStatus))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoStatus f x =
    (\y -> x { _eeeoStatus = y })
       <$> f (_eeeoStatus x)
{-# INLINE eeeoStatus #-}

-- | Describes the current tier of this environment.
eeeoTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoTier f x =
    (\y -> x { _eeeoTier = y })
       <$> f (_eeeoTier x)
{-# INLINE eeeoTier #-}

-- | The name of the SolutionStack deployed with this environment.
eeeoSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoSolutionStackName f x =
    (\y -> x { _eeeoSolutionStackName = y })
       <$> f (_eeeoSolutionStackName x)
{-# INLINE eeeoSolutionStackName #-}

-- | The last modified date for this environment.
eeeoDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoDateUpdated f x =
    (\y -> x { _eeeoDateUpdated = y })
       <$> f (_eeeoDateUpdated x)
{-# INLINE eeeoDateUpdated #-}

-- | The application version deployed in this environment.
eeeoVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateEnvironmentResponse
    -> f UpdateEnvironmentResponse
eeeoVersionLabel f x =
    (\y -> x { _eeeoVersionLabel = y })
       <$> f (_eeeoVersionLabel x)
{-# INLINE eeeoVersionLabel #-}

instance FromXML UpdateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request = post "UpdateEnvironment"
    response _ = xmlResponse

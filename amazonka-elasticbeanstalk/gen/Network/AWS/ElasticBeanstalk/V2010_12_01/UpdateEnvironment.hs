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
    , mkUpdateEnvironmentMessage
    -- ** Request lenses
    , uemEnvironmentId
    , uemEnvironmentName
    , uemDescription
    , uemTier
    , uemVersionLabel
    , uemTemplateName
    , uemOptionSettings
    , uemOptionsToRemove

    -- * Response
    , UpdateEnvironmentResponse
    -- ** Response lenses
    , eeeoEnvironmentName
    , eeeoEnvironmentId
    , eeeoApplicationName
    , eeeoVersionLabel
    , eeeoSolutionStackName
    , eeeoTemplateName
    , eeeoDescription
    , eeeoEndpointURL
    , eeeoCNAME
    , eeeoDateCreated
    , eeeoDateUpdated
    , eeeoStatus
    , eeeoHealth
    , eeeoResources
    , eeeoTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateEnvironment' request.
mkUpdateEnvironmentMessage :: UpdateEnvironment
mkUpdateEnvironmentMessage = UpdateEnvironment
    { _uemEnvironmentId = Nothing
    , _uemEnvironmentName = Nothing
    , _uemDescription = Nothing
    , _uemTier = Nothing
    , _uemVersionLabel = Nothing
    , _uemTemplateName = Nothing
    , _uemOptionSettings = mempty
    , _uemOptionsToRemove = mempty
    }
{-# INLINE mkUpdateEnvironmentMessage #-}

data UpdateEnvironment = UpdateEnvironment
    { _uemEnvironmentId :: Maybe Text
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
    , _uemDescription :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk updates the
      -- description of this environment.
    , _uemTier :: Maybe EnvironmentTier
      -- ^ This specifies the tier to use to update the environment.
      -- Condition: You can only update the tier version for an
      -- environment. If you change the name of the type, AWS Elastic
      -- Beanstalk returns InvalidParameterValue error.
    , _uemVersionLabel :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk deploys the
      -- named application version to the environment. If no such
      -- application version is found, returns an InvalidParameterValue
      -- error.
    , _uemTemplateName :: Maybe Text
      -- ^ If this parameter is specified, AWS Elastic Beanstalk deploys
      -- this configuration template to the environment. If no such
      -- configuration template is found, AWS Elastic Beanstalk returns an
      -- InvalidParameterValue error.
    , _uemOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk updates the configuration set
      -- associated with the running environment and sets the specified
      -- configuration options to the requested value.
    , _uemOptionsToRemove :: [OptionSpecification]
      -- ^ A list of custom user-defined configuration options to remove
      -- from the configuration set for this environment.
    } deriving (Show, Generic)

-- | The ID of the environment to update. If no environment with this ID exists,
-- AWS Elastic Beanstalk returns an InvalidParameterValue error. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
uemEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
uemEnvironmentId = lens _uemEnvironmentId (\s a -> s { _uemEnvironmentId = a })
{-# INLINE uemEnvironmentId #-}

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
uemEnvironmentName :: Lens' UpdateEnvironment (Maybe Text)
uemEnvironmentName = lens _uemEnvironmentName (\s a -> s { _uemEnvironmentName = a })
{-# INLINE uemEnvironmentName #-}

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
uemDescription :: Lens' UpdateEnvironment (Maybe Text)
uemDescription = lens _uemDescription (\s a -> s { _uemDescription = a })
{-# INLINE uemDescription #-}

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the name
-- of the type, AWS Elastic Beanstalk returns InvalidParameterValue error.
uemTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
uemTier = lens _uemTier (\s a -> s { _uemTier = a })
{-# INLINE uemTier #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
uemVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
uemVersionLabel = lens _uemVersionLabel (\s a -> s { _uemVersionLabel = a })
{-# INLINE uemVersionLabel #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
uemTemplateName :: Lens' UpdateEnvironment (Maybe Text)
uemTemplateName = lens _uemTemplateName (\s a -> s { _uemTemplateName = a })
{-# INLINE uemTemplateName #-}

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
uemOptionSettings :: Lens' UpdateEnvironment ([ConfigurationOptionSetting])
uemOptionSettings = lens _uemOptionSettings (\s a -> s { _uemOptionSettings = a })
{-# INLINE uemOptionSettings #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
uemOptionsToRemove :: Lens' UpdateEnvironment ([OptionSpecification])
uemOptionsToRemove = lens _uemOptionsToRemove (\s a -> s { _uemOptionsToRemove = a })
{-# INLINE uemOptionsToRemove #-}

instance ToQuery UpdateEnvironment where
    toQuery = genericQuery def

data UpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _eeeoEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeeoEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeeoApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeeoVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    , _eeeoSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeeoTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeeoDescription :: Maybe Text
      -- ^ Describes this environment.
    , _eeeoEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeeoCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeeoDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeeoDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeeoStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
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
    , _eeeoResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeeoTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    } deriving (Show, Generic)

-- | The name of this environment.
eeeoEnvironmentName :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoEnvironmentName = lens _eeeoEnvironmentName (\s a -> s { _eeeoEnvironmentName = a })
{-# INLINE eeeoEnvironmentName #-}

-- | The ID of this environment.
eeeoEnvironmentId :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoEnvironmentId = lens _eeeoEnvironmentId (\s a -> s { _eeeoEnvironmentId = a })
{-# INLINE eeeoEnvironmentId #-}

-- | The name of the application associated with this environment.
eeeoApplicationName :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoApplicationName = lens _eeeoApplicationName (\s a -> s { _eeeoApplicationName = a })
{-# INLINE eeeoApplicationName #-}

-- | The application version deployed in this environment.
eeeoVersionLabel :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoVersionLabel = lens _eeeoVersionLabel (\s a -> s { _eeeoVersionLabel = a })
{-# INLINE eeeoVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
eeeoSolutionStackName :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoSolutionStackName = lens _eeeoSolutionStackName (\s a -> s { _eeeoSolutionStackName = a })
{-# INLINE eeeoSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeeoTemplateName :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoTemplateName = lens _eeeoTemplateName (\s a -> s { _eeeoTemplateName = a })
{-# INLINE eeeoTemplateName #-}

-- | Describes this environment.
eeeoDescription :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoDescription = lens _eeeoDescription (\s a -> s { _eeeoDescription = a })
{-# INLINE eeeoDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeeoEndpointURL :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoEndpointURL = lens _eeeoEndpointURL (\s a -> s { _eeeoEndpointURL = a })
{-# INLINE eeeoEndpointURL #-}

-- | The URL to the CNAME for this environment.
eeeoCNAME :: Lens' UpdateEnvironmentResponse (Maybe Text)
eeeoCNAME = lens _eeeoCNAME (\s a -> s { _eeeoCNAME = a })
{-# INLINE eeeoCNAME #-}

-- | The creation date for this environment.
eeeoDateCreated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
eeeoDateCreated = lens _eeeoDateCreated (\s a -> s { _eeeoDateCreated = a })
{-# INLINE eeeoDateCreated #-}

-- | The last modified date for this environment.
eeeoDateUpdated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
eeeoDateUpdated = lens _eeeoDateUpdated (\s a -> s { _eeeoDateUpdated = a })
{-# INLINE eeeoDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeeoStatus :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentStatus)
eeeoStatus = lens _eeeoStatus (\s a -> s { _eeeoStatus = a })
{-# INLINE eeeoStatus #-}

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
eeeoHealth :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentHealth)
eeeoHealth = lens _eeeoHealth (\s a -> s { _eeeoHealth = a })
{-# INLINE eeeoHealth #-}

-- | The description of the AWS resources used by this environment.
eeeoResources :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
eeeoResources = lens _eeeoResources (\s a -> s { _eeeoResources = a })
{-# INLINE eeeoResources #-}

-- | Describes the current tier of this environment.
eeeoTier :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentTier)
eeeoTier = lens _eeeoTier (\s a -> s { _eeeoTier = a })
{-# INLINE eeeoTier #-}

instance FromXML UpdateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request = post "UpdateEnvironment"
    response _ = xmlResponse

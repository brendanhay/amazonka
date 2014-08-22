{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.UpdateEnvironment where

import Control.Lens.TH (makeLenses)
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

makeLenses ''UpdateEnvironment

instance ToQuery UpdateEnvironment where
    toQuery = genericQuery def

data UpdateEnvironmentResponse = UpdateEnvironmentResponse
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

makeLenses ''UpdateEnvironmentResponse

instance FromXML UpdateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request = post "UpdateEnvironment"
    response _ = xmlResponse

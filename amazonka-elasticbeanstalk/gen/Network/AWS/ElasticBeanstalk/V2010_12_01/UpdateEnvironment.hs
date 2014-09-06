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
    , mkUpdateEnvironment
    -- ** Request lenses
    , ueEnvironmentId
    , ueEnvironmentName
    , ueDescription
    , ueTier
    , ueVersionLabel
    , ueTemplateName
    , ueOptionSettings
    , ueOptionsToRemove

    -- * Response
    , UpdateEnvironmentResponse
    -- ** Response lenses
    , uersEnvironmentName
    , uersEnvironmentId
    , uersApplicationName
    , uersVersionLabel
    , uersSolutionStackName
    , uersTemplateName
    , uersDescription
    , uersEndpointURL
    , uersCNAME
    , uersDateCreated
    , uersDateUpdated
    , uersStatus
    , uersHealth
    , uersResources
    , uersTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data UpdateEnvironment = UpdateEnvironment
    { _ueEnvironmentId :: Maybe Text
    , _ueEnvironmentName :: Maybe Text
    , _ueDescription :: Maybe Text
    , _ueTier :: Maybe EnvironmentTier
    , _ueVersionLabel :: Maybe Text
    , _ueTemplateName :: Maybe Text
    , _ueOptionSettings :: [ConfigurationOptionSetting]
    , _ueOptionsToRemove :: [OptionSpecification]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateEnvironment' request.
mkUpdateEnvironment :: UpdateEnvironment
mkUpdateEnvironment = UpdateEnvironment
    { _ueEnvironmentId = Nothing
    , _ueEnvironmentName = Nothing
    , _ueDescription = Nothing
    , _ueTier = Nothing
    , _ueVersionLabel = Nothing
    , _ueTemplateName = Nothing
    , _ueOptionSettings = mempty
    , _ueOptionsToRemove = mempty
    }
{-# INLINE mkUpdateEnvironment #-}

-- | The ID of the environment to update. If no environment with this ID exists,
-- AWS Elastic Beanstalk returns an InvalidParameterValue error. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
ueEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentId = lens _ueEnvironmentId (\s a -> s { _ueEnvironmentId = a })
{-# INLINE ueEnvironmentId #-}

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
ueEnvironmentName :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentName =
    lens _ueEnvironmentName (\s a -> s { _ueEnvironmentName = a })
{-# INLINE ueEnvironmentName #-}

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\s a -> s { _ueDescription = a })
{-# INLINE ueDescription #-}

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the name
-- of the type, AWS Elastic Beanstalk returns InvalidParameterValue error.
ueTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
ueTier = lens _ueTier (\s a -> s { _ueTier = a })
{-# INLINE ueTier #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
ueVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
ueVersionLabel = lens _ueVersionLabel (\s a -> s { _ueVersionLabel = a })
{-# INLINE ueVersionLabel #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
ueTemplateName :: Lens' UpdateEnvironment (Maybe Text)
ueTemplateName = lens _ueTemplateName (\s a -> s { _ueTemplateName = a })
{-# INLINE ueTemplateName #-}

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
ueOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
ueOptionSettings =
    lens _ueOptionSettings (\s a -> s { _ueOptionSettings = a })
{-# INLINE ueOptionSettings #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
ueOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
ueOptionsToRemove =
    lens _ueOptionsToRemove (\s a -> s { _ueOptionsToRemove = a })
{-# INLINE ueOptionsToRemove #-}

instance ToQuery UpdateEnvironment where
    toQuery = genericQuery def

-- | Describes the properties of an environment.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _uersEnvironmentName :: Maybe Text
    , _uersEnvironmentId :: Maybe Text
    , _uersApplicationName :: Maybe Text
    , _uersVersionLabel :: Maybe Text
    , _uersSolutionStackName :: Maybe Text
    , _uersTemplateName :: Maybe Text
    , _uersDescription :: Maybe Text
    , _uersEndpointURL :: Maybe Text
    , _uersCNAME :: Maybe Text
    , _uersDateCreated :: Maybe ISO8601
    , _uersDateUpdated :: Maybe ISO8601
    , _uersStatus :: Maybe EnvironmentStatus
    , _uersHealth :: Maybe EnvironmentHealth
    , _uersResources :: Maybe EnvironmentResourcesDescription
    , _uersTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | The name of this environment.
uersEnvironmentName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersEnvironmentName =
    lens _uersEnvironmentName (\s a -> s { _uersEnvironmentName = a })
{-# INLINE uersEnvironmentName #-}

-- | The ID of this environment.
uersEnvironmentId :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersEnvironmentId =
    lens _uersEnvironmentId (\s a -> s { _uersEnvironmentId = a })
{-# INLINE uersEnvironmentId #-}

-- | The name of the application associated with this environment.
uersApplicationName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersApplicationName =
    lens _uersApplicationName (\s a -> s { _uersApplicationName = a })
{-# INLINE uersApplicationName #-}

-- | The application version deployed in this environment.
uersVersionLabel :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersVersionLabel =
    lens _uersVersionLabel (\s a -> s { _uersVersionLabel = a })
{-# INLINE uersVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
uersSolutionStackName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersSolutionStackName =
    lens _uersSolutionStackName (\s a -> s { _uersSolutionStackName = a })
{-# INLINE uersSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
uersTemplateName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersTemplateName =
    lens _uersTemplateName (\s a -> s { _uersTemplateName = a })
{-# INLINE uersTemplateName #-}

-- | Describes this environment.
uersDescription :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersDescription = lens _uersDescription (\s a -> s { _uersDescription = a })
{-# INLINE uersDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
uersEndpointURL :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersEndpointURL = lens _uersEndpointURL (\s a -> s { _uersEndpointURL = a })
{-# INLINE uersEndpointURL #-}

-- | The URL to the CNAME for this environment.
uersCNAME :: Lens' UpdateEnvironmentResponse (Maybe Text)
uersCNAME = lens _uersCNAME (\s a -> s { _uersCNAME = a })
{-# INLINE uersCNAME #-}

-- | The creation date for this environment.
uersDateCreated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
uersDateCreated = lens _uersDateCreated (\s a -> s { _uersDateCreated = a })
{-# INLINE uersDateCreated #-}

-- | The last modified date for this environment.
uersDateUpdated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
uersDateUpdated = lens _uersDateUpdated (\s a -> s { _uersDateUpdated = a })
{-# INLINE uersDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
uersStatus :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentStatus)
uersStatus = lens _uersStatus (\s a -> s { _uersStatus = a })
{-# INLINE uersStatus #-}

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
uersHealth :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentHealth)
uersHealth = lens _uersHealth (\s a -> s { _uersHealth = a })
{-# INLINE uersHealth #-}

-- | The description of the AWS resources used by this environment.
uersResources :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
uersResources = lens _uersResources (\s a -> s { _uersResources = a })
{-# INLINE uersResources #-}

-- | Describes the current tier of this environment.
uersTier :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentTier)
uersTier = lens _uersTier (\s a -> s { _uersTier = a })
{-# INLINE uersTier #-}

instance FromXML UpdateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request = post "UpdateEnvironment"
    response _ = xmlResponse

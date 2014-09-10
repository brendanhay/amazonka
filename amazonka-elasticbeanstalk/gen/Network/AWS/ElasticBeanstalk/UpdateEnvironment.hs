{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
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
module Network.AWS.ElasticBeanstalk
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
    -- ** Response constructor
    , mkUpdateEnvironmentResponse
    -- ** Response lenses
    , uerEnvironmentName
    , uerEnvironmentId
    , uerApplicationName
    , uerVersionLabel
    , uerSolutionStackName
    , uerTemplateName
    , uerDescription
    , uerEndpointURL
    , uerCNAME
    , uerDateCreated
    , uerDateUpdated
    , uerStatus
    , uerHealth
    , uerResources
    , uerTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data UpdateEnvironment = UpdateEnvironment
    { _ueEnvironmentId :: !(Maybe Text)
    , _ueEnvironmentName :: !(Maybe Text)
    , _ueDescription :: !(Maybe Text)
    , _ueTier :: Maybe EnvironmentTier
    , _ueVersionLabel :: !(Maybe Text)
    , _ueTemplateName :: !(Maybe Text)
    , _ueOptionSettings :: [ConfigurationOptionSetting]
    , _ueOptionsToRemove :: [OptionSpecification]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateEnvironment' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Tier ::@ @Maybe EnvironmentTier@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @OptionSettings ::@ @[ConfigurationOptionSetting]@
--
-- * @OptionsToRemove ::@ @[OptionSpecification]@
--
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

-- | The ID of the environment to update. If no environment with this ID exists,
-- AWS Elastic Beanstalk returns an InvalidParameterValue error. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
ueEnvironmentId :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentId = lens _ueEnvironmentId (\s a -> s { _ueEnvironmentId = a })

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
ueEnvironmentName :: Lens' UpdateEnvironment (Maybe Text)
ueEnvironmentName =
    lens _ueEnvironmentName (\s a -> s { _ueEnvironmentName = a })

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\s a -> s { _ueDescription = a })

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the name
-- of the type, AWS Elastic Beanstalk returns InvalidParameterValue error.
ueTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
ueTier = lens _ueTier (\s a -> s { _ueTier = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
ueVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
ueVersionLabel = lens _ueVersionLabel (\s a -> s { _ueVersionLabel = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
ueTemplateName :: Lens' UpdateEnvironment (Maybe Text)
ueTemplateName = lens _ueTemplateName (\s a -> s { _ueTemplateName = a })

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
ueOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
ueOptionSettings =
    lens _ueOptionSettings (\s a -> s { _ueOptionSettings = a })

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
ueOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
ueOptionsToRemove =
    lens _ueOptionsToRemove (\s a -> s { _ueOptionsToRemove = a })

instance ToQuery UpdateEnvironment where
    toQuery = genericQuery def

-- | Describes the properties of an environment.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _uerEnvironmentName :: !(Maybe Text)
    , _uerEnvironmentId :: !(Maybe Text)
    , _uerApplicationName :: !(Maybe Text)
    , _uerVersionLabel :: !(Maybe Text)
    , _uerSolutionStackName :: !(Maybe Text)
    , _uerTemplateName :: !(Maybe Text)
    , _uerDescription :: !(Maybe Text)
    , _uerEndpointURL :: !(Maybe Text)
    , _uerCNAME :: !(Maybe Text)
    , _uerDateCreated :: !(Maybe ISO8601)
    , _uerDateUpdated :: !(Maybe ISO8601)
    , _uerStatus :: Maybe EnvironmentStatus
    , _uerHealth :: Maybe EnvironmentHealth
    , _uerResources :: Maybe EnvironmentResourcesDescription
    , _uerTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateEnvironmentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EndpointURL ::@ @Maybe Text@
--
-- * @CNAME ::@ @Maybe Text@
--
-- * @DateCreated ::@ @Maybe ISO8601@
--
-- * @DateUpdated ::@ @Maybe ISO8601@
--
-- * @Status ::@ @Maybe EnvironmentStatus@
--
-- * @Health ::@ @Maybe EnvironmentHealth@
--
-- * @Resources ::@ @Maybe EnvironmentResourcesDescription@
--
-- * @Tier ::@ @Maybe EnvironmentTier@
--
mkUpdateEnvironmentResponse :: UpdateEnvironmentResponse
mkUpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _uerEnvironmentName = Nothing
    , _uerEnvironmentId = Nothing
    , _uerApplicationName = Nothing
    , _uerVersionLabel = Nothing
    , _uerSolutionStackName = Nothing
    , _uerTemplateName = Nothing
    , _uerDescription = Nothing
    , _uerEndpointURL = Nothing
    , _uerCNAME = Nothing
    , _uerDateCreated = Nothing
    , _uerDateUpdated = Nothing
    , _uerStatus = Nothing
    , _uerHealth = Nothing
    , _uerResources = Nothing
    , _uerTier = Nothing
    }

-- | The name of this environment.
uerEnvironmentName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEnvironmentName =
    lens _uerEnvironmentName (\s a -> s { _uerEnvironmentName = a })

-- | The ID of this environment.
uerEnvironmentId :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEnvironmentId =
    lens _uerEnvironmentId (\s a -> s { _uerEnvironmentId = a })

-- | The name of the application associated with this environment.
uerApplicationName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerApplicationName =
    lens _uerApplicationName (\s a -> s { _uerApplicationName = a })

-- | The application version deployed in this environment.
uerVersionLabel :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerVersionLabel = lens _uerVersionLabel (\s a -> s { _uerVersionLabel = a })

-- | The name of the SolutionStack deployed with this environment.
uerSolutionStackName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerSolutionStackName =
    lens _uerSolutionStackName (\s a -> s { _uerSolutionStackName = a })

-- | The name of the configuration template used to originally launch this
-- environment.
uerTemplateName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerTemplateName = lens _uerTemplateName (\s a -> s { _uerTemplateName = a })

-- | Describes this environment.
uerDescription :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerDescription = lens _uerDescription (\s a -> s { _uerDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
uerEndpointURL :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEndpointURL = lens _uerEndpointURL (\s a -> s { _uerEndpointURL = a })

-- | The URL to the CNAME for this environment.
uerCNAME :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerCNAME = lens _uerCNAME (\s a -> s { _uerCNAME = a })

-- | The creation date for this environment.
uerDateCreated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
uerDateCreated = lens _uerDateCreated (\s a -> s { _uerDateCreated = a })

-- | The last modified date for this environment.
uerDateUpdated :: Lens' UpdateEnvironmentResponse (Maybe ISO8601)
uerDateUpdated = lens _uerDateUpdated (\s a -> s { _uerDateUpdated = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
uerStatus :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentStatus)
uerStatus = lens _uerStatus (\s a -> s { _uerStatus = a })

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
uerHealth :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentHealth)
uerHealth = lens _uerHealth (\s a -> s { _uerHealth = a })

-- | The description of the AWS resources used by this environment.
uerResources :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
uerResources = lens _uerResources (\s a -> s { _uerResources = a })

-- | Describes the current tier of this environment.
uerTier :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentTier)
uerTier = lens _uerTier (\s a -> s { _uerTier = a })

instance FromXML UpdateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request = post "UpdateEnvironment"
    response _ = xmlResponse

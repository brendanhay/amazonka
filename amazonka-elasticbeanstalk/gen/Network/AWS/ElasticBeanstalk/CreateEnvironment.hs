{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
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
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      CreateEnvironment
    -- ** Request constructor
    , mkCreateEnvironment
    -- ** Request lenses
    , ceApplicationName
    , ceEnvironmentName
    , ceDescription
    , ceCNAMEPrefix
    , ceTier
    , ceTags
    , ceVersionLabel
    , ceTemplateName
    , ceSolutionStackName
    , ceOptionSettings
    , ceOptionsToRemove

    -- * Response
    , CreateEnvironmentResponse
    -- ** Response constructor
    , mkCreateEnvironmentResponse
    -- ** Response lenses
    , cerEnvironmentName
    , cerEnvironmentId
    , cerApplicationName
    , cerVersionLabel
    , cerSolutionStackName
    , cerTemplateName
    , cerDescription
    , cerEndpointURL
    , cerCNAME
    , cerDateCreated
    , cerDateUpdated
    , cerStatus
    , cerHealth
    , cerResources
    , cerTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | 
data CreateEnvironment = CreateEnvironment
    { _ceApplicationName :: Text
    , _ceEnvironmentName :: Text
    , _ceDescription :: Maybe Text
    , _ceCNAMEPrefix :: Maybe Text
    , _ceTier :: Maybe EnvironmentTier
    , _ceTags :: [Tag]
    , _ceVersionLabel :: Maybe Text
    , _ceTemplateName :: Maybe Text
    , _ceSolutionStackName :: Maybe Text
    , _ceOptionSettings :: [ConfigurationOptionSetting]
    , _ceOptionsToRemove :: [OptionSpecification]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateEnvironment' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @EnvironmentName ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @CNAMEPrefix ::@ @Maybe Text@
--
-- * @Tier ::@ @Maybe EnvironmentTier@
--
-- * @Tags ::@ @[Tag]@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @OptionSettings ::@ @[ConfigurationOptionSetting]@
--
-- * @OptionsToRemove ::@ @[OptionSpecification]@
--
mkCreateEnvironment :: Text -- ^ 'ceApplicationName'
                    -> Text -- ^ 'ceEnvironmentName'
                    -> CreateEnvironment
mkCreateEnvironment p1 p2 = CreateEnvironment
    { _ceApplicationName = p1
    , _ceEnvironmentName = p2
    , _ceDescription = Nothing
    , _ceCNAMEPrefix = Nothing
    , _ceTier = Nothing
    , _ceTags = mempty
    , _ceVersionLabel = Nothing
    , _ceTemplateName = Nothing
    , _ceSolutionStackName = Nothing
    , _ceOptionSettings = mempty
    , _ceOptionsToRemove = mempty
    }

-- | The name of the application that contains the version to be deployed. If no
-- application is found with this name, CreateEnvironment returns an
-- InvalidParameterValue error.
ceApplicationName :: Lens' CreateEnvironment Text
ceApplicationName =
    lens _ceApplicationName (\s a -> s { _ceApplicationName = a })

-- | A unique name for the deployment environment. Used in the application URL.
-- Constraint: Must be from 4 to 23 characters in length. The name can contain
-- only letters, numbers, and hyphens. It cannot start or end with a hyphen.
-- This name must be unique in your account. If the specified name already
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Default: If the CNAME parameter is not specified, the environment name
-- becomes part of the CNAME, and therefore part of the visible URL for your
-- application.
ceEnvironmentName :: Lens' CreateEnvironment Text
ceEnvironmentName =
    lens _ceEnvironmentName (\s a -> s { _ceEnvironmentName = a })

-- | Describes this environment.
ceDescription :: Lens' CreateEnvironment (Maybe Text)
ceDescription = lens _ceDescription (\s a -> s { _ceDescription = a })

-- | If specified, the environment attempts to use this value as the prefix for
-- the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
ceCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
ceCNAMEPrefix = lens _ceCNAMEPrefix (\s a -> s { _ceCNAMEPrefix = a })

-- | This specifies the tier to use for creating this environment.
ceTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
ceTier = lens _ceTier (\s a -> s { _ceTier = a })

-- | This specifies the tags applied to resources in the environment.
ceTags :: Lens' CreateEnvironment [Tag]
ceTags = lens _ceTags (\s a -> s { _ceTags = a })

-- | The name of the application version to deploy. If the specified application
-- has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the application
-- version">sample application in the container.
ceVersionLabel :: Lens' CreateEnvironment (Maybe Text)
ceVersionLabel = lens _ceVersionLabel (\s a -> s { _ceVersionLabel = a })

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this parameter or a SolutionStackName, but not both. If you specify both,
-- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
-- do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
ceTemplateName :: Lens' CreateEnvironment (Maybe Text)
ceTemplateName = lens _ceTemplateName (\s a -> s { _ceTemplateName = a })

-- | This is an alternative to specifying a configuration name. If specified,
-- AWS Elastic Beanstalk sets the configuration values to the default values
-- associated with the specified solution stack. Condition: You must specify
-- either this or a TemplateName, but not both. If you specify both, AWS
-- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
-- not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
ceSolutionStackName :: Lens' CreateEnvironment (Maybe Text)
ceSolutionStackName =
    lens _ceSolutionStackName (\s a -> s { _ceSolutionStackName = a })

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack or
-- the configuration template.
ceOptionSettings :: Lens' CreateEnvironment [ConfigurationOptionSetting]
ceOptionSettings =
    lens _ceOptionSettings (\s a -> s { _ceOptionSettings = a })

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
ceOptionsToRemove :: Lens' CreateEnvironment [OptionSpecification]
ceOptionsToRemove =
    lens _ceOptionsToRemove (\s a -> s { _ceOptionsToRemove = a })

instance ToQuery CreateEnvironment where
    toQuery = genericQuery def

-- | Describes the properties of an environment.
data CreateEnvironmentResponse = CreateEnvironmentResponse
    { _cerEnvironmentName :: Maybe Text
    , _cerEnvironmentId :: Maybe Text
    , _cerApplicationName :: Maybe Text
    , _cerVersionLabel :: Maybe Text
    , _cerSolutionStackName :: Maybe Text
    , _cerTemplateName :: Maybe Text
    , _cerDescription :: Maybe Text
    , _cerEndpointURL :: Maybe Text
    , _cerCNAME :: Maybe Text
    , _cerDateCreated :: Maybe ISO8601
    , _cerDateUpdated :: Maybe ISO8601
    , _cerStatus :: Maybe EnvironmentStatus
    , _cerHealth :: Maybe EnvironmentHealth
    , _cerResources :: Maybe EnvironmentResourcesDescription
    , _cerTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateEnvironmentResponse' response.
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
mkCreateEnvironmentResponse :: CreateEnvironmentResponse
mkCreateEnvironmentResponse = CreateEnvironmentResponse
    { _cerEnvironmentName = Nothing
    , _cerEnvironmentId = Nothing
    , _cerApplicationName = Nothing
    , _cerVersionLabel = Nothing
    , _cerSolutionStackName = Nothing
    , _cerTemplateName = Nothing
    , _cerDescription = Nothing
    , _cerEndpointURL = Nothing
    , _cerCNAME = Nothing
    , _cerDateCreated = Nothing
    , _cerDateUpdated = Nothing
    , _cerStatus = Nothing
    , _cerHealth = Nothing
    , _cerResources = Nothing
    , _cerTier = Nothing
    }

-- | The name of this environment.
cerEnvironmentName :: Lens' CreateEnvironmentResponse (Maybe Text)
cerEnvironmentName =
    lens _cerEnvironmentName (\s a -> s { _cerEnvironmentName = a })

-- | The ID of this environment.
cerEnvironmentId :: Lens' CreateEnvironmentResponse (Maybe Text)
cerEnvironmentId =
    lens _cerEnvironmentId (\s a -> s { _cerEnvironmentId = a })

-- | The name of the application associated with this environment.
cerApplicationName :: Lens' CreateEnvironmentResponse (Maybe Text)
cerApplicationName =
    lens _cerApplicationName (\s a -> s { _cerApplicationName = a })

-- | The application version deployed in this environment.
cerVersionLabel :: Lens' CreateEnvironmentResponse (Maybe Text)
cerVersionLabel = lens _cerVersionLabel (\s a -> s { _cerVersionLabel = a })

-- | The name of the SolutionStack deployed with this environment.
cerSolutionStackName :: Lens' CreateEnvironmentResponse (Maybe Text)
cerSolutionStackName =
    lens _cerSolutionStackName (\s a -> s { _cerSolutionStackName = a })

-- | The name of the configuration template used to originally launch this
-- environment.
cerTemplateName :: Lens' CreateEnvironmentResponse (Maybe Text)
cerTemplateName = lens _cerTemplateName (\s a -> s { _cerTemplateName = a })

-- | Describes this environment.
cerDescription :: Lens' CreateEnvironmentResponse (Maybe Text)
cerDescription = lens _cerDescription (\s a -> s { _cerDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
cerEndpointURL :: Lens' CreateEnvironmentResponse (Maybe Text)
cerEndpointURL = lens _cerEndpointURL (\s a -> s { _cerEndpointURL = a })

-- | The URL to the CNAME for this environment.
cerCNAME :: Lens' CreateEnvironmentResponse (Maybe Text)
cerCNAME = lens _cerCNAME (\s a -> s { _cerCNAME = a })

-- | The creation date for this environment.
cerDateCreated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
cerDateCreated = lens _cerDateCreated (\s a -> s { _cerDateCreated = a })

-- | The last modified date for this environment.
cerDateUpdated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
cerDateUpdated = lens _cerDateUpdated (\s a -> s { _cerDateUpdated = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
cerStatus :: Lens' CreateEnvironmentResponse (Maybe EnvironmentStatus)
cerStatus = lens _cerStatus (\s a -> s { _cerStatus = a })

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
cerHealth :: Lens' CreateEnvironmentResponse (Maybe EnvironmentHealth)
cerHealth = lens _cerHealth (\s a -> s { _cerHealth = a })

-- | The description of the AWS resources used by this environment.
cerResources :: Lens' CreateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
cerResources = lens _cerResources (\s a -> s { _cerResources = a })

-- | Describes the current tier of this environment.
cerTier :: Lens' CreateEnvironmentResponse (Maybe EnvironmentTier)
cerTier = lens _cerTier (\s a -> s { _cerTier = a })

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEnvironment where
    type Sv CreateEnvironment = ElasticBeanstalk
    type Rs CreateEnvironment = CreateEnvironmentResponse

    request = post "CreateEnvironment"
    response _ = xmlResponse

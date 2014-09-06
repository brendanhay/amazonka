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
    -- ** Response lenses
    , cersEnvironmentName
    , cersEnvironmentId
    , cersApplicationName
    , cersVersionLabel
    , cersSolutionStackName
    , cersTemplateName
    , cersDescription
    , cersEndpointURL
    , cersCNAME
    , cersDateCreated
    , cersDateUpdated
    , cersStatus
    , cersHealth
    , cersResources
    , cersTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
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
{-# INLINE mkCreateEnvironment #-}

-- | The name of the application that contains the version to be deployed. If no
-- application is found with this name, CreateEnvironment returns an
-- InvalidParameterValue error.
ceApplicationName :: Lens' CreateEnvironment Text
ceApplicationName =
    lens _ceApplicationName (\s a -> s { _ceApplicationName = a })
{-# INLINE ceApplicationName #-}

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
{-# INLINE ceEnvironmentName #-}

-- | Describes this environment.
ceDescription :: Lens' CreateEnvironment (Maybe Text)
ceDescription = lens _ceDescription (\s a -> s { _ceDescription = a })
{-# INLINE ceDescription #-}

-- | If specified, the environment attempts to use this value as the prefix for
-- the CNAME. If not specified, the CNAME is generated automatically by
-- appending a random alphanumeric string to the environment name.
ceCNAMEPrefix :: Lens' CreateEnvironment (Maybe Text)
ceCNAMEPrefix = lens _ceCNAMEPrefix (\s a -> s { _ceCNAMEPrefix = a })
{-# INLINE ceCNAMEPrefix #-}

-- | This specifies the tier to use for creating this environment.
ceTier :: Lens' CreateEnvironment (Maybe EnvironmentTier)
ceTier = lens _ceTier (\s a -> s { _ceTier = a })
{-# INLINE ceTier #-}

-- | This specifies the tags applied to resources in the environment.
ceTags :: Lens' CreateEnvironment [Tag]
ceTags = lens _ceTags (\s a -> s { _ceTags = a })
{-# INLINE ceTags #-}

-- | The name of the application version to deploy. If the specified application
-- has no associated application versions, AWS Elastic Beanstalk
-- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
-- specified, AWS Elastic Beanstalk attempts to launch the application
-- version">sample application in the container.
ceVersionLabel :: Lens' CreateEnvironment (Maybe Text)
ceVersionLabel = lens _ceVersionLabel (\s a -> s { _ceVersionLabel = a })
{-# INLINE ceVersionLabel #-}

-- | The name of the configuration template to use in deployment. If no
-- configuration template is found with this name, AWS Elastic Beanstalk
-- returns an InvalidParameterValue error. Condition: You must specify either
-- this parameter or a SolutionStackName, but not both. If you specify both,
-- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
-- do not specify either, AWS Elastic Beanstalk returns a
-- MissingRequiredParameter error.
ceTemplateName :: Lens' CreateEnvironment (Maybe Text)
ceTemplateName = lens _ceTemplateName (\s a -> s { _ceTemplateName = a })
{-# INLINE ceTemplateName #-}

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
{-# INLINE ceSolutionStackName #-}

-- | If specified, AWS Elastic Beanstalk sets the specified configuration
-- options to the requested value in the configuration set for the new
-- environment. These override the values obtained from the solution stack or
-- the configuration template.
ceOptionSettings :: Lens' CreateEnvironment [ConfigurationOptionSetting]
ceOptionSettings =
    lens _ceOptionSettings (\s a -> s { _ceOptionSettings = a })
{-# INLINE ceOptionSettings #-}

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this new environment.
ceOptionsToRemove :: Lens' CreateEnvironment [OptionSpecification]
ceOptionsToRemove =
    lens _ceOptionsToRemove (\s a -> s { _ceOptionsToRemove = a })
{-# INLINE ceOptionsToRemove #-}

instance ToQuery CreateEnvironment where
    toQuery = genericQuery def

-- | Describes the properties of an environment.
data CreateEnvironmentResponse = CreateEnvironmentResponse
    { _cersEnvironmentName :: Maybe Text
    , _cersEnvironmentId :: Maybe Text
    , _cersApplicationName :: Maybe Text
    , _cersVersionLabel :: Maybe Text
    , _cersSolutionStackName :: Maybe Text
    , _cersTemplateName :: Maybe Text
    , _cersDescription :: Maybe Text
    , _cersEndpointURL :: Maybe Text
    , _cersCNAME :: Maybe Text
    , _cersDateCreated :: Maybe ISO8601
    , _cersDateUpdated :: Maybe ISO8601
    , _cersStatus :: Maybe EnvironmentStatus
    , _cersHealth :: Maybe EnvironmentHealth
    , _cersResources :: Maybe EnvironmentResourcesDescription
    , _cersTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | The name of this environment.
cersEnvironmentName :: Lens' CreateEnvironmentResponse (Maybe Text)
cersEnvironmentName =
    lens _cersEnvironmentName (\s a -> s { _cersEnvironmentName = a })
{-# INLINE cersEnvironmentName #-}

-- | The ID of this environment.
cersEnvironmentId :: Lens' CreateEnvironmentResponse (Maybe Text)
cersEnvironmentId =
    lens _cersEnvironmentId (\s a -> s { _cersEnvironmentId = a })
{-# INLINE cersEnvironmentId #-}

-- | The name of the application associated with this environment.
cersApplicationName :: Lens' CreateEnvironmentResponse (Maybe Text)
cersApplicationName =
    lens _cersApplicationName (\s a -> s { _cersApplicationName = a })
{-# INLINE cersApplicationName #-}

-- | The application version deployed in this environment.
cersVersionLabel :: Lens' CreateEnvironmentResponse (Maybe Text)
cersVersionLabel =
    lens _cersVersionLabel (\s a -> s { _cersVersionLabel = a })
{-# INLINE cersVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
cersSolutionStackName :: Lens' CreateEnvironmentResponse (Maybe Text)
cersSolutionStackName =
    lens _cersSolutionStackName (\s a -> s { _cersSolutionStackName = a })
{-# INLINE cersSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
cersTemplateName :: Lens' CreateEnvironmentResponse (Maybe Text)
cersTemplateName =
    lens _cersTemplateName (\s a -> s { _cersTemplateName = a })
{-# INLINE cersTemplateName #-}

-- | Describes this environment.
cersDescription :: Lens' CreateEnvironmentResponse (Maybe Text)
cersDescription = lens _cersDescription (\s a -> s { _cersDescription = a })
{-# INLINE cersDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
cersEndpointURL :: Lens' CreateEnvironmentResponse (Maybe Text)
cersEndpointURL = lens _cersEndpointURL (\s a -> s { _cersEndpointURL = a })
{-# INLINE cersEndpointURL #-}

-- | The URL to the CNAME for this environment.
cersCNAME :: Lens' CreateEnvironmentResponse (Maybe Text)
cersCNAME = lens _cersCNAME (\s a -> s { _cersCNAME = a })
{-# INLINE cersCNAME #-}

-- | The creation date for this environment.
cersDateCreated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
cersDateCreated = lens _cersDateCreated (\s a -> s { _cersDateCreated = a })
{-# INLINE cersDateCreated #-}

-- | The last modified date for this environment.
cersDateUpdated :: Lens' CreateEnvironmentResponse (Maybe ISO8601)
cersDateUpdated = lens _cersDateUpdated (\s a -> s { _cersDateUpdated = a })
{-# INLINE cersDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
cersStatus :: Lens' CreateEnvironmentResponse (Maybe EnvironmentStatus)
cersStatus = lens _cersStatus (\s a -> s { _cersStatus = a })
{-# INLINE cersStatus #-}

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
cersHealth :: Lens' CreateEnvironmentResponse (Maybe EnvironmentHealth)
cersHealth = lens _cersHealth (\s a -> s { _cersHealth = a })
{-# INLINE cersHealth #-}

-- | The description of the AWS resources used by this environment.
cersResources :: Lens' CreateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
cersResources = lens _cersResources (\s a -> s { _cersResources = a })
{-# INLINE cersResources #-}

-- | Describes the current tier of this environment.
cersTier :: Lens' CreateEnvironmentResponse (Maybe EnvironmentTier)
cersTier = lens _cersTier (\s a -> s { _cersTier = a })
{-# INLINE cersTier #-}

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEnvironment where
    type Sv CreateEnvironment = ElasticBeanstalk
    type Rs CreateEnvironment = CreateEnvironmentResponse

    request = post "CreateEnvironment"
    response _ = xmlResponse

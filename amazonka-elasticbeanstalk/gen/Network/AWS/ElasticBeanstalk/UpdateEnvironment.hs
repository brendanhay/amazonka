{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_UpdateEnvironment.html>
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    (
    -- * Request
      UpdateEnvironment
    -- ** Request constructor
    , updateEnvironment
    -- ** Request lenses
    , ueDescription
    , ueEnvironmentId
    , ueEnvironmentName
    , ueOptionSettings
    , ueOptionsToRemove
    , ueTemplateName
    , ueTier
    , ueVersionLabel

    -- * Response
    , UpdateEnvironmentResponse
    -- ** Response constructor
    , updateEnvironmentResponse
    -- ** Response lenses
    , uerApplicationName
    , uerCNAME
    , uerDateCreated
    , uerDateUpdated
    , uerDescription
    , uerEndpointURL
    , uerEnvironmentId
    , uerEnvironmentName
    , uerHealth
    , uerResources
    , uerSolutionStackName
    , uerStatus
    , uerTemplateName
    , uerTier
    , uerVersionLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data UpdateEnvironment = UpdateEnvironment
    { _ueDescription     :: Maybe Text
    , _ueEnvironmentId   :: Maybe Text
    , _ueEnvironmentName :: Maybe Text
    , _ueOptionSettings  :: [ConfigurationOptionSetting]
    , _ueOptionsToRemove :: [OptionSpecification]
    , _ueTemplateName    :: Maybe Text
    , _ueTier            :: Maybe EnvironmentTier
    , _ueVersionLabel    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateEnvironment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueDescription' @::@ 'Maybe' 'Text'
--
-- * 'ueEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'ueEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'ueOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'ueOptionsToRemove' @::@ ['OptionSpecification']
--
-- * 'ueTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'ueTier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'ueVersionLabel' @::@ 'Maybe' 'Text'
--
updateEnvironment :: UpdateEnvironment
updateEnvironment = UpdateEnvironment
    { _ueEnvironmentId   = Nothing
    , _ueEnvironmentName = Nothing
    , _ueDescription     = Nothing
    , _ueTier            = Nothing
    , _ueVersionLabel    = Nothing
    , _ueTemplateName    = Nothing
    , _ueOptionSettings  = mempty
    , _ueOptionsToRemove = mempty
    }

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
ueDescription :: Lens' UpdateEnvironment (Maybe Text)
ueDescription = lens _ueDescription (\s a -> s { _ueDescription = a })

-- | The ID of the environment to update. If no environment with this ID
-- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- MissingRequiredParameter error.
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

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
ueOptionSettings :: Lens' UpdateEnvironment [ConfigurationOptionSetting]
ueOptionSettings = lens _ueOptionSettings (\s a -> s { _ueOptionSettings = a })

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
ueOptionsToRemove :: Lens' UpdateEnvironment [OptionSpecification]
ueOptionsToRemove =
    lens _ueOptionsToRemove (\s a -> s { _ueOptionsToRemove = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an InvalidParameterValue
-- error.
ueTemplateName :: Lens' UpdateEnvironment (Maybe Text)
ueTemplateName = lens _ueTemplateName (\s a -> s { _ueTemplateName = a })

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the
-- name of the type, AWS Elastic Beanstalk returns InvalidParameterValue
-- error.
ueTier :: Lens' UpdateEnvironment (Maybe EnvironmentTier)
ueTier = lens _ueTier (\s a -> s { _ueTier = a })

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version is
-- found, returns an InvalidParameterValue error.
ueVersionLabel :: Lens' UpdateEnvironment (Maybe Text)
ueVersionLabel = lens _ueVersionLabel (\s a -> s { _ueVersionLabel = a })

data UpdateEnvironmentResponse = UpdateEnvironmentResponse
    { _uerApplicationName   :: Maybe Text
    , _uerCNAME             :: Maybe Text
    , _uerDateCreated       :: Maybe RFC822
    , _uerDateUpdated       :: Maybe RFC822
    , _uerDescription       :: Maybe Text
    , _uerEndpointURL       :: Maybe Text
    , _uerEnvironmentId     :: Maybe Text
    , _uerEnvironmentName   :: Maybe Text
    , _uerHealth            :: Maybe Text
    , _uerResources         :: Maybe EnvironmentResourcesDescription
    , _uerSolutionStackName :: Maybe Text
    , _uerStatus            :: Maybe Text
    , _uerTemplateName      :: Maybe Text
    , _uerTier              :: Maybe EnvironmentTier
    , _uerVersionLabel      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateEnvironmentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uerApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'uerCNAME' @::@ 'Maybe' 'Text'
--
-- * 'uerDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'uerDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'uerDescription' @::@ 'Maybe' 'Text'
--
-- * 'uerEndpointURL' @::@ 'Maybe' 'Text'
--
-- * 'uerEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'uerEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'uerHealth' @::@ 'Maybe' 'Text'
--
-- * 'uerResources' @::@ 'Maybe' 'EnvironmentResourcesDescription'
--
-- * 'uerSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'uerStatus' @::@ 'Maybe' 'Text'
--
-- * 'uerTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'uerTier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'uerVersionLabel' @::@ 'Maybe' 'Text'
--
updateEnvironmentResponse :: UpdateEnvironmentResponse
updateEnvironmentResponse = UpdateEnvironmentResponse
    { _uerEnvironmentName   = Nothing
    , _uerEnvironmentId     = Nothing
    , _uerApplicationName   = Nothing
    , _uerVersionLabel      = Nothing
    , _uerSolutionStackName = Nothing
    , _uerTemplateName      = Nothing
    , _uerDescription       = Nothing
    , _uerEndpointURL       = Nothing
    , _uerCNAME             = Nothing
    , _uerDateCreated       = Nothing
    , _uerDateUpdated       = Nothing
    , _uerStatus            = Nothing
    , _uerHealth            = Nothing
    , _uerResources         = Nothing
    , _uerTier              = Nothing
    }

-- | The name of the application associated with this environment.
uerApplicationName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerApplicationName =
    lens _uerApplicationName (\s a -> s { _uerApplicationName = a })

-- | The URL to the CNAME for this environment.
uerCNAME :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerCNAME = lens _uerCNAME (\s a -> s { _uerCNAME = a })

-- | The creation date for this environment.
uerDateCreated :: Lens' UpdateEnvironmentResponse (Maybe UTCTime)
uerDateCreated = lens _uerDateCreated (\s a -> s { _uerDateCreated = a })
    . mapping _Time

-- | The last modified date for this environment.
uerDateUpdated :: Lens' UpdateEnvironmentResponse (Maybe UTCTime)
uerDateUpdated = lens _uerDateUpdated (\s a -> s { _uerDateUpdated = a })
    . mapping _Time

-- | Describes this environment.
uerDescription :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerDescription = lens _uerDescription (\s a -> s { _uerDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
uerEndpointURL :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEndpointURL = lens _uerEndpointURL (\s a -> s { _uerEndpointURL = a })

-- | The ID of this environment.
uerEnvironmentId :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEnvironmentId = lens _uerEnvironmentId (\s a -> s { _uerEnvironmentId = a })

-- | The name of this environment.
uerEnvironmentName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerEnvironmentName =
    lens _uerEnvironmentName (\s a -> s { _uerEnvironmentName = a })

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates
-- the environment is not working. Yellow: Indicates that something is
-- wrong, the application might not be available, but the instances appear
-- running. Green: Indicates the environment is healthy and fully
-- functional. Red: Indicates the environment is not responsive. Occurs when
-- three or more consecutive failures occur for an environment. Yellow:
-- Indicates that something is wrong. Occurs when two consecutive failures
-- occur for an environment. Green: Indicates the environment is healthy and
-- fully functional. Grey: Default health for a new environment. The
-- environment is not fully launched and health checks have not started or
-- health checks are suspended during an UpdateEnvironment or
-- RestartEnvironement request. Default: Grey.
uerHealth :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerHealth = lens _uerHealth (\s a -> s { _uerHealth = a })

-- | The description of the AWS resources used by this environment.
uerResources :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
uerResources = lens _uerResources (\s a -> s { _uerResources = a })

-- | The name of the SolutionStack deployed with this environment.
uerSolutionStackName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerSolutionStackName =
    lens _uerSolutionStackName (\s a -> s { _uerSolutionStackName = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such
-- as update or terminate. Terminating: Environment is in the shut-down
-- process. Terminated: Environment is not running.
uerStatus :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerStatus = lens _uerStatus (\s a -> s { _uerStatus = a })

-- | The name of the configuration template used to originally launch this
-- environment.
uerTemplateName :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerTemplateName = lens _uerTemplateName (\s a -> s { _uerTemplateName = a })

-- | Describes the current tier of this environment.
uerTier :: Lens' UpdateEnvironmentResponse (Maybe EnvironmentTier)
uerTier = lens _uerTier (\s a -> s { _uerTier = a })

-- | The application version deployed in this environment.
uerVersionLabel :: Lens' UpdateEnvironmentResponse (Maybe Text)
uerVersionLabel = lens _uerVersionLabel (\s a -> s { _uerVersionLabel = a })

instance ToPath UpdateEnvironment where
    toPath = const "/"

instance ToQuery UpdateEnvironment

instance ToHeaders UpdateEnvironment

instance AWSRequest UpdateEnvironment where
    type Sv UpdateEnvironment = ElasticBeanstalk
    type Rs UpdateEnvironment = UpdateEnvironmentResponse

    request  = post "UpdateEnvironment"
    response = xmlResponse

instance FromXML UpdateEnvironmentResponse where
    parseXML c = UpdateEnvironmentResponse
        <$> c .: "ApplicationName"
        <*> c .: "CNAME"
        <*> c .: "DateCreated"
        <*> c .: "DateUpdated"
        <*> c .: "Description"
        <*> c .: "EndpointURL"
        <*> c .: "EnvironmentId"
        <*> c .: "EnvironmentName"
        <*> c .: "Health"
        <*> c .: "Resources"
        <*> c .: "SolutionStackName"
        <*> c .: "Status"
        <*> c .: "TemplateName"
        <*> c .: "Tier"
        <*> c .: "VersionLabel"

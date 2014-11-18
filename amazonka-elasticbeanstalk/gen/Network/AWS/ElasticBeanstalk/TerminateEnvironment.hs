{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified environment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_TerminateEnvironment.html>
module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironment
    -- ** Request constructor
    , terminateEnvironment
    -- ** Request lenses
    , teEnvironmentId
    , teEnvironmentName
    , teTerminateResources

    -- * Response
    , TerminateEnvironmentResponse
    -- ** Response constructor
    , terminateEnvironmentResponse
    -- ** Response lenses
    , terApplicationName
    , terCNAME
    , terDateCreated
    , terDateUpdated
    , terDescription
    , terEndpointURL
    , terEnvironmentId
    , terEnvironmentName
    , terHealth
    , terResources
    , terSolutionStackName
    , terStatus
    , terTemplateName
    , terTier
    , terVersionLabel
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data TerminateEnvironment = TerminateEnvironment
    { _teEnvironmentId      :: Maybe Text
    , _teEnvironmentName    :: Maybe Text
    , _teTerminateResources :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'TerminateEnvironment' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'teEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'teEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'teTerminateResources' @::@ 'Maybe' 'Bool'
--
terminateEnvironment :: TerminateEnvironment
terminateEnvironment = TerminateEnvironment
    { _teEnvironmentId      = Nothing
    , _teEnvironmentName    = Nothing
    , _teTerminateResources = Nothing
    }

-- | The ID of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentName, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
teEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentId = lens _teEnvironmentId (\s a -> s { _teEnvironmentId = a })

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either,
-- AWS Elastic Beanstalk returns MissingRequiredParameter error.
teEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentName =
    lens _teEnvironmentName (\s a -> s { _teEnvironmentName = a })

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated: true: (default) The user AWS resources (for
-- example, the Auto Scaling group, LoadBalancer, etc.) are terminated along
-- with the environment. false: The environment is removed from the AWS
-- Elastic Beanstalk but the AWS resources continue to operate. true: The
-- specified environment as well as the associated AWS resources, such as
-- Auto Scaling group and LoadBalancer, are terminated. false: AWS Elastic
-- Beanstalk resource management is removed from the environment, but the
-- AWS resources continue to operate. For more information, see the AWS
-- Elastic Beanstalk User Guide. Default: true Valid Values: true | false.
teTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
teTerminateResources =
    lens _teTerminateResources (\s a -> s { _teTerminateResources = a })

data TerminateEnvironmentResponse = TerminateEnvironmentResponse
    { _terApplicationName   :: Maybe Text
    , _terCNAME             :: Maybe Text
    , _terDateCreated       :: Maybe RFC822
    , _terDateUpdated       :: Maybe RFC822
    , _terDescription       :: Maybe Text
    , _terEndpointURL       :: Maybe Text
    , _terEnvironmentId     :: Maybe Text
    , _terEnvironmentName   :: Maybe Text
    , _terHealth            :: Maybe Text
    , _terResources         :: Maybe EnvironmentResourcesDescription
    , _terSolutionStackName :: Maybe Text
    , _terStatus            :: Maybe Text
    , _terTemplateName      :: Maybe Text
    , _terTier              :: Maybe EnvironmentTier
    , _terVersionLabel      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'TerminateEnvironmentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'terApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'terCNAME' @::@ 'Maybe' 'Text'
--
-- * 'terDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'terDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'terDescription' @::@ 'Maybe' 'Text'
--
-- * 'terEndpointURL' @::@ 'Maybe' 'Text'
--
-- * 'terEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'terEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'terHealth' @::@ 'Maybe' 'Text'
--
-- * 'terResources' @::@ 'Maybe' 'EnvironmentResourcesDescription'
--
-- * 'terSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'terStatus' @::@ 'Maybe' 'Text'
--
-- * 'terTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'terTier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'terVersionLabel' @::@ 'Maybe' 'Text'
--
terminateEnvironmentResponse :: TerminateEnvironmentResponse
terminateEnvironmentResponse = TerminateEnvironmentResponse
    { _terEnvironmentName   = Nothing
    , _terEnvironmentId     = Nothing
    , _terApplicationName   = Nothing
    , _terVersionLabel      = Nothing
    , _terSolutionStackName = Nothing
    , _terTemplateName      = Nothing
    , _terDescription       = Nothing
    , _terEndpointURL       = Nothing
    , _terCNAME             = Nothing
    , _terDateCreated       = Nothing
    , _terDateUpdated       = Nothing
    , _terStatus            = Nothing
    , _terHealth            = Nothing
    , _terResources         = Nothing
    , _terTier              = Nothing
    }

-- | The name of the application associated with this environment.
terApplicationName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terApplicationName =
    lens _terApplicationName (\s a -> s { _terApplicationName = a })

-- | The URL to the CNAME for this environment.
terCNAME :: Lens' TerminateEnvironmentResponse (Maybe Text)
terCNAME = lens _terCNAME (\s a -> s { _terCNAME = a })

-- | The creation date for this environment.
terDateCreated :: Lens' TerminateEnvironmentResponse (Maybe UTCTime)
terDateCreated = lens _terDateCreated (\s a -> s { _terDateCreated = a })
    . mapping _Time

-- | The last modified date for this environment.
terDateUpdated :: Lens' TerminateEnvironmentResponse (Maybe UTCTime)
terDateUpdated = lens _terDateUpdated (\s a -> s { _terDateUpdated = a })
    . mapping _Time

-- | Describes this environment.
terDescription :: Lens' TerminateEnvironmentResponse (Maybe Text)
terDescription = lens _terDescription (\s a -> s { _terDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
terEndpointURL :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEndpointURL = lens _terEndpointURL (\s a -> s { _terEndpointURL = a })

-- | The ID of this environment.
terEnvironmentId :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEnvironmentId = lens _terEnvironmentId (\s a -> s { _terEnvironmentId = a })

-- | The name of this environment.
terEnvironmentName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEnvironmentName =
    lens _terEnvironmentName (\s a -> s { _terEnvironmentName = a })

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
terHealth :: Lens' TerminateEnvironmentResponse (Maybe Text)
terHealth = lens _terHealth (\s a -> s { _terHealth = a })

-- | The description of the AWS resources used by this environment.
terResources :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
terResources = lens _terResources (\s a -> s { _terResources = a })

-- | The name of the SolutionStack deployed with this environment.
terSolutionStackName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terSolutionStackName =
    lens _terSolutionStackName (\s a -> s { _terSolutionStackName = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such
-- as update or terminate. Terminating: Environment is in the shut-down
-- process. Terminated: Environment is not running.
terStatus :: Lens' TerminateEnvironmentResponse (Maybe Text)
terStatus = lens _terStatus (\s a -> s { _terStatus = a })

-- | The name of the configuration template used to originally launch this
-- environment.
terTemplateName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terTemplateName = lens _terTemplateName (\s a -> s { _terTemplateName = a })

-- | Describes the current tier of this environment.
terTier :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentTier)
terTier = lens _terTier (\s a -> s { _terTier = a })

-- | The application version deployed in this environment.
terVersionLabel :: Lens' TerminateEnvironmentResponse (Maybe Text)
terVersionLabel = lens _terVersionLabel (\s a -> s { _terVersionLabel = a })

instance ToPath TerminateEnvironment where
    toPath = const "/"

instance ToQuery TerminateEnvironment

instance ToHeaders TerminateEnvironment

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request  = post "TerminateEnvironment"
    response = xmlResponse

instance FromXML TerminateEnvironmentResponse where
    parseXML x = TerminateEnvironmentResponse
        <$> x .@? "ApplicationName"
        <*> x .@? "CNAME"
        <*> x .@? "DateCreated"
        <*> x .@? "DateUpdated"
        <*> x .@? "Description"
        <*> x .@? "EndpointURL"
        <*> x .@? "EnvironmentId"
        <*> x .@? "EnvironmentName"
        <*> x .@? "Health"
        <*> x .@? "Resources"
        <*> x .@? "SolutionStackName"
        <*> x .@? "Status"
        <*> x .@? "TemplateName"
        <*> x .@? "Tier"
        <*> x .@? "VersionLabel"

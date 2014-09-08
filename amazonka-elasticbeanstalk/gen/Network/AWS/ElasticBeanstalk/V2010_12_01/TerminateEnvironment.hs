{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-icsgecu3wf
-- &EnvironmentName=SampleApp &TerminateResources=true
-- &Operation=TerminateEnvironment &AuthParams Version1 Terminating SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Grey e-icsgecu3wf
-- 2010-11-17T17:10:41.976Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 9b71af21-f26d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.TerminateEnvironment
    (
    -- * Request
      TerminateEnvironment
    -- ** Request constructor
    , mkTerminateEnvironment
    -- ** Request lenses
    , teEnvironmentId
    , teEnvironmentName
    , teTerminateResources

    -- * Response
    , TerminateEnvironmentResponse
    -- ** Response constructor
    , mkTerminateEnvironmentResponse
    -- ** Response lenses
    , terEnvironmentName
    , terEnvironmentId
    , terApplicationName
    , terVersionLabel
    , terSolutionStackName
    , terTemplateName
    , terDescription
    , terEndpointURL
    , terCNAME
    , terDateCreated
    , terDateUpdated
    , terStatus
    , terHealth
    , terResources
    , terTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data TerminateEnvironment = TerminateEnvironment
    { _teEnvironmentId :: Maybe Text
    , _teEnvironmentName :: Maybe Text
    , _teTerminateResources :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateEnvironment' request.
mkTerminateEnvironment :: TerminateEnvironment
mkTerminateEnvironment = TerminateEnvironment
    { _teEnvironmentId = Nothing
    , _teEnvironmentName = Nothing
    , _teTerminateResources = Nothing
    }

-- | The ID of the environment to terminate. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
teEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentId = lens _teEnvironmentId (\s a -> s { _teEnvironmentId = a })

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
teEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
teEnvironmentName =
    lens _teEnvironmentName (\s a -> s { _teEnvironmentName = a })

-- | Indicates whether the associated AWS resources should shut down when the
-- environment is terminated: true: (default) The user AWS resources (for
-- example, the Auto Scaling group, LoadBalancer, etc.) are terminated along
-- with the environment. false: The environment is removed from the AWS
-- Elastic Beanstalk but the AWS resources continue to operate. true: The
-- specified environment as well as the associated AWS resources, such as Auto
-- Scaling group and LoadBalancer, are terminated. false: AWS Elastic
-- Beanstalk resource management is removed from the environment, but the AWS
-- resources continue to operate. For more information, see the AWS Elastic
-- Beanstalk User Guide. Default: true Valid Values: true | false.
teTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
teTerminateResources =
    lens _teTerminateResources (\s a -> s { _teTerminateResources = a })

instance ToQuery TerminateEnvironment where
    toQuery = genericQuery def

-- | Describes the properties of an environment.
data TerminateEnvironmentResponse = TerminateEnvironmentResponse
    { _terEnvironmentName :: Maybe Text
    , _terEnvironmentId :: Maybe Text
    , _terApplicationName :: Maybe Text
    , _terVersionLabel :: Maybe Text
    , _terSolutionStackName :: Maybe Text
    , _terTemplateName :: Maybe Text
    , _terDescription :: Maybe Text
    , _terEndpointURL :: Maybe Text
    , _terCNAME :: Maybe Text
    , _terDateCreated :: Maybe ISO8601
    , _terDateUpdated :: Maybe ISO8601
    , _terStatus :: Maybe EnvironmentStatus
    , _terHealth :: Maybe EnvironmentHealth
    , _terResources :: Maybe EnvironmentResourcesDescription
    , _terTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateEnvironmentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkTerminateEnvironmentResponse :: TerminateEnvironmentResponse
mkTerminateEnvironmentResponse = TerminateEnvironmentResponse
    { _terEnvironmentName = Nothing
    , _terEnvironmentId = Nothing
    , _terApplicationName = Nothing
    , _terVersionLabel = Nothing
    , _terSolutionStackName = Nothing
    , _terTemplateName = Nothing
    , _terDescription = Nothing
    , _terEndpointURL = Nothing
    , _terCNAME = Nothing
    , _terDateCreated = Nothing
    , _terDateUpdated = Nothing
    , _terStatus = Nothing
    , _terHealth = Nothing
    , _terResources = Nothing
    , _terTier = Nothing
    }

-- | The name of this environment.
terEnvironmentName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEnvironmentName =
    lens _terEnvironmentName (\s a -> s { _terEnvironmentName = a })

-- | The ID of this environment.
terEnvironmentId :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEnvironmentId =
    lens _terEnvironmentId (\s a -> s { _terEnvironmentId = a })

-- | The name of the application associated with this environment.
terApplicationName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terApplicationName =
    lens _terApplicationName (\s a -> s { _terApplicationName = a })

-- | The application version deployed in this environment.
terVersionLabel :: Lens' TerminateEnvironmentResponse (Maybe Text)
terVersionLabel = lens _terVersionLabel (\s a -> s { _terVersionLabel = a })

-- | The name of the SolutionStack deployed with this environment.
terSolutionStackName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terSolutionStackName =
    lens _terSolutionStackName (\s a -> s { _terSolutionStackName = a })

-- | The name of the configuration template used to originally launch this
-- environment.
terTemplateName :: Lens' TerminateEnvironmentResponse (Maybe Text)
terTemplateName = lens _terTemplateName (\s a -> s { _terTemplateName = a })

-- | Describes this environment.
terDescription :: Lens' TerminateEnvironmentResponse (Maybe Text)
terDescription = lens _terDescription (\s a -> s { _terDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
terEndpointURL :: Lens' TerminateEnvironmentResponse (Maybe Text)
terEndpointURL = lens _terEndpointURL (\s a -> s { _terEndpointURL = a })

-- | The URL to the CNAME for this environment.
terCNAME :: Lens' TerminateEnvironmentResponse (Maybe Text)
terCNAME = lens _terCNAME (\s a -> s { _terCNAME = a })

-- | The creation date for this environment.
terDateCreated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
terDateCreated = lens _terDateCreated (\s a -> s { _terDateCreated = a })

-- | The last modified date for this environment.
terDateUpdated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
terDateUpdated = lens _terDateUpdated (\s a -> s { _terDateUpdated = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
terStatus :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentStatus)
terStatus = lens _terStatus (\s a -> s { _terStatus = a })

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
terHealth :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentHealth)
terHealth = lens _terHealth (\s a -> s { _terHealth = a })

-- | The description of the AWS resources used by this environment.
terResources :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
terResources = lens _terResources (\s a -> s { _terResources = a })

-- | Describes the current tier of this environment.
terTier :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentTier)
terTier = lens _terTier (\s a -> s { _terTier = a })

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request = post "TerminateEnvironment"
    response _ = xmlResponse

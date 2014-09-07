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
    -- ** Response lenses
    , tersEnvironmentName
    , tersEnvironmentId
    , tersApplicationName
    , tersVersionLabel
    , tersSolutionStackName
    , tersTemplateName
    , tersDescription
    , tersEndpointURL
    , tersCNAME
    , tersDateCreated
    , tersDateUpdated
    , tersStatus
    , tersHealth
    , tersResources
    , tersTier
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
    { _tersEnvironmentName :: Maybe Text
    , _tersEnvironmentId :: Maybe Text
    , _tersApplicationName :: Maybe Text
    , _tersVersionLabel :: Maybe Text
    , _tersSolutionStackName :: Maybe Text
    , _tersTemplateName :: Maybe Text
    , _tersDescription :: Maybe Text
    , _tersEndpointURL :: Maybe Text
    , _tersCNAME :: Maybe Text
    , _tersDateCreated :: Maybe ISO8601
    , _tersDateUpdated :: Maybe ISO8601
    , _tersStatus :: Maybe EnvironmentStatus
    , _tersHealth :: Maybe EnvironmentHealth
    , _tersResources :: Maybe EnvironmentResourcesDescription
    , _tersTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | The name of this environment.
tersEnvironmentName :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersEnvironmentName =
    lens _tersEnvironmentName (\s a -> s { _tersEnvironmentName = a })

-- | The ID of this environment.
tersEnvironmentId :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersEnvironmentId =
    lens _tersEnvironmentId (\s a -> s { _tersEnvironmentId = a })

-- | The name of the application associated with this environment.
tersApplicationName :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersApplicationName =
    lens _tersApplicationName (\s a -> s { _tersApplicationName = a })

-- | The application version deployed in this environment.
tersVersionLabel :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersVersionLabel =
    lens _tersVersionLabel (\s a -> s { _tersVersionLabel = a })

-- | The name of the SolutionStack deployed with this environment.
tersSolutionStackName :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersSolutionStackName =
    lens _tersSolutionStackName (\s a -> s { _tersSolutionStackName = a })

-- | The name of the configuration template used to originally launch this
-- environment.
tersTemplateName :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersTemplateName =
    lens _tersTemplateName (\s a -> s { _tersTemplateName = a })

-- | Describes this environment.
tersDescription :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersDescription = lens _tersDescription (\s a -> s { _tersDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
tersEndpointURL :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersEndpointURL = lens _tersEndpointURL (\s a -> s { _tersEndpointURL = a })

-- | The URL to the CNAME for this environment.
tersCNAME :: Lens' TerminateEnvironmentResponse (Maybe Text)
tersCNAME = lens _tersCNAME (\s a -> s { _tersCNAME = a })

-- | The creation date for this environment.
tersDateCreated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
tersDateCreated = lens _tersDateCreated (\s a -> s { _tersDateCreated = a })

-- | The last modified date for this environment.
tersDateUpdated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
tersDateUpdated = lens _tersDateUpdated (\s a -> s { _tersDateUpdated = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
tersStatus :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentStatus)
tersStatus = lens _tersStatus (\s a -> s { _tersStatus = a })

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
tersHealth :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentHealth)
tersHealth = lens _tersHealth (\s a -> s { _tersHealth = a })

-- | The description of the AWS resources used by this environment.
tersResources :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
tersResources = lens _tersResources (\s a -> s { _tersResources = a })

-- | Describes the current tier of this environment.
tersTier :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentTier)
tersTier = lens _tersTier (\s a -> s { _tersTier = a })

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request = post "TerminateEnvironment"
    response _ = xmlResponse

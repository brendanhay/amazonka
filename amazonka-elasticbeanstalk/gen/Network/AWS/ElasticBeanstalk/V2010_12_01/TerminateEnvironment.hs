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
    , mkTerminateEnvironmentMessage
    -- ** Request lenses
    , temEnvironmentId
    , temEnvironmentName
    , temTerminateResources

    -- * Response
    , TerminateEnvironmentResponse
    -- ** Response lenses
    , eeenEnvironmentName
    , eeenEnvironmentId
    , eeenApplicationName
    , eeenVersionLabel
    , eeenSolutionStackName
    , eeenTemplateName
    , eeenDescription
    , eeenEndpointURL
    , eeenCNAME
    , eeenDateCreated
    , eeenDateUpdated
    , eeenStatus
    , eeenHealth
    , eeenResources
    , eeenTier
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateEnvironment' request.
mkTerminateEnvironmentMessage :: TerminateEnvironment
mkTerminateEnvironmentMessage = TerminateEnvironment
    { _temEnvironmentId = Nothing
    , _temEnvironmentName = Nothing
    , _temTerminateResources = Nothing
    }
{-# INLINE mkTerminateEnvironmentMessage #-}

data TerminateEnvironment = TerminateEnvironment
    { _temEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to terminate. Condition: You must
      -- specify either this or an EnvironmentName, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _temEnvironmentName :: Maybe Text
      -- ^ The name of the environment to terminate. Condition: You must
      -- specify either this or an EnvironmentId, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _temTerminateResources :: Maybe Bool
      -- ^ Indicates whether the associated AWS resources should shut down
      -- when the environment is terminated: true: (default) The user AWS
      -- resources (for example, the Auto Scaling group, LoadBalancer,
      -- etc.) are terminated along with the environment. false: The
      -- environment is removed from the AWS Elastic Beanstalk but the AWS
      -- resources continue to operate. true: The specified environment as
      -- well as the associated AWS resources, such as Auto Scaling group
      -- and LoadBalancer, are terminated. false: AWS Elastic Beanstalk
      -- resource management is removed from the environment, but the AWS
      -- resources continue to operate. For more information, see the AWS
      -- Elastic Beanstalk User Guide. Default: true Valid Values: true |
      -- false.
    } deriving (Show, Generic)

-- | The ID of the environment to terminate. Condition: You must specify either
-- this or an EnvironmentName, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentId :: Lens' TerminateEnvironment (Maybe Text)
temEnvironmentId = lens _temEnvironmentId (\s a -> s { _temEnvironmentId = a })
{-# INLINE temEnvironmentId #-}

-- | The name of the environment to terminate. Condition: You must specify
-- either this or an EnvironmentId, or both. If you do not specify either, AWS
-- Elastic Beanstalk returns MissingRequiredParameter error.
temEnvironmentName :: Lens' TerminateEnvironment (Maybe Text)
temEnvironmentName = lens _temEnvironmentName (\s a -> s { _temEnvironmentName = a })
{-# INLINE temEnvironmentName #-}

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
temTerminateResources :: Lens' TerminateEnvironment (Maybe Bool)
temTerminateResources = lens _temTerminateResources (\s a -> s { _temTerminateResources = a })
{-# INLINE temTerminateResources #-}

instance ToQuery TerminateEnvironment where
    toQuery = genericQuery def

data TerminateEnvironmentResponse = TerminateEnvironmentResponse
    { _eeenEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeenEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeenApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeenVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    , _eeenSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeenTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeenDescription :: Maybe Text
      -- ^ Describes this environment.
    , _eeenEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeenCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeenDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeenDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeenStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _eeenHealth :: Maybe EnvironmentHealth
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
    , _eeenResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeenTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    } deriving (Show, Generic)

-- | The name of this environment.
eeenEnvironmentName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEnvironmentName = lens _eeenEnvironmentName (\s a -> s { _eeenEnvironmentName = a })
{-# INLINE eeenEnvironmentName #-}

-- | The ID of this environment.
eeenEnvironmentId :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEnvironmentId = lens _eeenEnvironmentId (\s a -> s { _eeenEnvironmentId = a })
{-# INLINE eeenEnvironmentId #-}

-- | The name of the application associated with this environment.
eeenApplicationName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenApplicationName = lens _eeenApplicationName (\s a -> s { _eeenApplicationName = a })
{-# INLINE eeenApplicationName #-}

-- | The application version deployed in this environment.
eeenVersionLabel :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenVersionLabel = lens _eeenVersionLabel (\s a -> s { _eeenVersionLabel = a })
{-# INLINE eeenVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
eeenSolutionStackName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenSolutionStackName = lens _eeenSolutionStackName (\s a -> s { _eeenSolutionStackName = a })
{-# INLINE eeenSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeenTemplateName :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenTemplateName = lens _eeenTemplateName (\s a -> s { _eeenTemplateName = a })
{-# INLINE eeenTemplateName #-}

-- | Describes this environment.
eeenDescription :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenDescription = lens _eeenDescription (\s a -> s { _eeenDescription = a })
{-# INLINE eeenDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeenEndpointURL :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenEndpointURL = lens _eeenEndpointURL (\s a -> s { _eeenEndpointURL = a })
{-# INLINE eeenEndpointURL #-}

-- | The URL to the CNAME for this environment.
eeenCNAME :: Lens' TerminateEnvironmentResponse (Maybe Text)
eeenCNAME = lens _eeenCNAME (\s a -> s { _eeenCNAME = a })
{-# INLINE eeenCNAME #-}

-- | The creation date for this environment.
eeenDateCreated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
eeenDateCreated = lens _eeenDateCreated (\s a -> s { _eeenDateCreated = a })
{-# INLINE eeenDateCreated #-}

-- | The last modified date for this environment.
eeenDateUpdated :: Lens' TerminateEnvironmentResponse (Maybe ISO8601)
eeenDateUpdated = lens _eeenDateUpdated (\s a -> s { _eeenDateUpdated = a })
{-# INLINE eeenDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeenStatus :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentStatus)
eeenStatus = lens _eeenStatus (\s a -> s { _eeenStatus = a })
{-# INLINE eeenStatus #-}

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
eeenHealth :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentHealth)
eeenHealth = lens _eeenHealth (\s a -> s { _eeenHealth = a })
{-# INLINE eeenHealth #-}

-- | The description of the AWS resources used by this environment.
eeenResources :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentResourcesDescription)
eeenResources = lens _eeenResources (\s a -> s { _eeenResources = a })
{-# INLINE eeenResources #-}

-- | Describes the current tier of this environment.
eeenTier :: Lens' TerminateEnvironmentResponse (Maybe EnvironmentTier)
eeenTier = lens _eeenTier (\s a -> s { _eeenTier = a })
{-# INLINE eeenTier #-}

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateEnvironment where
    type Sv TerminateEnvironment = ElasticBeanstalk
    type Rs TerminateEnvironment = TerminateEnvironmentResponse

    request = post "TerminateEnvironment"
    response _ = xmlResponse
